
#####################
## Load Packages ##
####################

require(sf)
require(dplyr)
require(tidyr)
require(mapview)
require(FedData)
require(terra)
require(zoo)

################
## Data Prep ##
################

#Read in summer New York brood data 
brood <- readRDS("Raw RDS Files/New York/NY.rds") 

#Temporarily Remove MU column to get rid of NA observations
#Create duplicate lat and lon columns for the geometry column (This keeps original lat/lon)
#Get rid of NA observations within coordinates
brood_subset<- brood %>%
  dplyr::select(-MU) %>%
  dplyr::mutate("lat1"= lat) %>%
  dplyr::mutate("lon1"=lon) %>%
  tidyr::drop_na(lat)

#Change brood subset to an Sf object
#Transform to Albers
brood.ready <-sf::st_as_sf(brood_subset,coords= c("lon1", 
                                                  "lat1"),
                           crs=4326) %>%
                        sf::st_transform(5070)

##############################
## Extract Land Cover Data ##
##############################

#Use the tigris package to filter through states and remove GEOIDs above 60
#GEOIDs above 60 are territories and islands so they are being removed for scaling
st <- tigris::states() %>%
  dplyr::filter(GEOID < "60") %>% 
  tigris::shift_geometry()

#Transform to albers equal area conic projection, epsg code is 5070
st <- sf::st_transform(st, 5070)

#Grab outline of New York
ny.outline <- subset(st, st$NAME=="New York")


#Extract New Jersey land cover classifications using the FedData package 
nyNLCD <- get_nlcd(template = ny.outline, label = "ny NLCD")
plot(nyNLCD) #Check

#Reclassify NLCD -- Skyrockets RAM
#See covertypesfor NLCD here: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
terra::values(nyNLCD) <- ifelse(terra::values(nyNLCD) %in% c(21:24), yes = "Developed", ## Developed Open, low, medium, high intensity
                         no = ifelse(terra::values(nyNLCD) %in% c(41:43), yes = "Forest", ## Deciduous, Evergreen, Mixed Forest 
                                     no= ifelse(terra::values(nyNLCD) %in% c(81:82), yes = "Agriculture", ## Pasture/Hay, Cultivated Crops
                                                no=  "Non Forest"))) ## Everything else

#Code to classify land cover types into groups 1-4
#Essentially I wanted to be sure each land cover types matched so I 3 created groups
m <- c(21, 24, 1, 41,43, 2, 81, 83, 3) #Group 4 is everything else coded "Non-Forest"
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- terra::classify(nyNLCD, rclmat)
plot(rc) #Check

#Save Raster
#ny.raster <-writeRaster(rc, "NY.tiff")
nyNLCD <- terra::rast("Rasters/New York NLCD/NY.tiff")
plot(nyNLCD) #Check

#Buffer each observation by 1 km 
#Transform to Albers for exactextractr
brood.buffer <- sf::st_buffer(brood.ready, 1000) %>%
  sf::st_transform(5070)
mapview(brood.buffer)#Check

#Create dataframe that has buffered nest values 
#Extract land cover type values using the exact extractr (has its own package) in R
brood.buffered.values <- exactextractr::exact_extract(nyNLCD, sf::st_as_sf(brood.buffer))
head(brood.buffered.values[[1]])#Percent in each category (buffers 1-6)

#Create a table of land cover types within each buffer
#lapply just applies a given function through a list in this instance 
et=lapply(brood.buffered.values,table)

#Create proportions list object using for loop
prop <- list()

#For loop takes the proportion of defined NLCD classifications throughout the entirety of buffers
for(i in 1:length(brood.buffered.values)[1] ){
  #margin.table: Compute table margins
  #This takes the proportion of land cover types
  prop[[i]] <- round((margin.table(et[[i]],1)/margin.table(et[[i]])),digits = 6)
}

#coredata function: Generic functions for extracting the core data contained in a more complex object
M <- zoo::coredata(do.call(cbind, lapply(prop, zoo)))
colnames(M) <- NULL
#Transpose matrix so land cover become separate columns of data
matrix <- t(M)
#Now convert the matrix to a data frame so it is easier to manipulate
dfland <- as.data.frame(matrix)
head(dfland) #Check

#Assign column names to land cover
#Column names are in order from ifelse statement 
colnames(dfland) <- c("Developed","Forest", "Agriculture", "Non-Forest")
head(dfland) #Check

#Function to fill dataframe NA values with 0.000001
#This prevents NA values from being present in models 
fill_NA_with_value <- function(df, value = 0.000001) {
  df[is.na(dfland)] <- 0.000001
  return(df)
}

#Apply the function to fill NA values with 0.0001
dfland.final <- fill_NA_with_value(dfland)
head(dfland.final) #Check

#Create broods dataframe with proportions
#Rename WMU to MU to match New Jersey
  broods.prop <- cbind(brood.buffer, dfland.final)%>%
    dplyr::rename("MU"=WMU) %>%
  dplyr::select(MU, Developed, Forest, Agriculture, Year, State, Date, lat, lon,
                Total, PHratio, doy )
head(broods.prop) #Check

#Create unique identifier column
#Needed for merge later
 broods.prop$unique.id<-seq(1,nrow(broods.prop))


###########################
## Extract Road Density ##
###########################

#Read in New Jersey Roads Shapefile
 #Transform to Albers
nyroads <- st_read("Shapefiles/New York Roads/tl_2019_36_prisecroads.shp") %>%
sf::st_transform(5070) %>%
sf::st_zm() #Function drops Z dimension of geometry for compatibility with GEOS

mapview(nyroads)#Check
#This looks a bit short of a comprehensive roads shapefile? Especially for New York

# #Create unique identifier column for later
# #We need this to link each buffer to an individual proportion of roads within
 brood.buffer$countID<-seq(1,nrow(brood.buffer))

#Use st_intersection to gauge amount of roads that intersect with each buffer
road.int<-st_intersection(nyroads,brood.buffer)

#st_length calculates the distance in meters of each road within the buffer
#Convert meters to km
road.int$Length <- st_length(road.int) /1000
head(road.int) #Check

#Calculate the sum of the roads within each buffer based upon measured length
road.densities <-aggregate(road.int$Length,by=list(road.int$countID),sum) %>%
dplyr::rename("unique.id"=Group.1) %>%
dplyr::rename("Road Density"= x)

#############
## Merge ##
#############

#Merge land cover data and road density data into a single dataframe
#Remove Geometry column
ny.data <- merge(broods.prop, road.densities,
              by= "unique.id", all=T) %>%
  st_drop_geometry()

#Fill Na values for unique.id with 0
#These were just observations that didn't have a road feature within the buffer
#Road density labeled as meters but it is in km
ny.data$`Road Density`[is.na(ny.data$`Road Density`)] <- 0

#Write csv
write.csv(ny.data, "NewYorkAnalysis.csv", row.names = F)






