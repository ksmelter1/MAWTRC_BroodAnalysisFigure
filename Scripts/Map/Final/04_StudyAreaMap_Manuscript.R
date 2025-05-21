#'---
#' title: Study Area Map for "A Framework for Analyzing Wild Turkey Summer Sighting Data" Manuscript
#' authors: "K. Smelter, F. Buderman, D. Diefenbach"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'   html_document: 
#'     toc: true
#'---
#'
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)
#'  
#' **Purpose**: This script creates a study area map for "A Framework for Analyzing Wild Turkey Summer Sighting Data" Manuscript


#####################
## Load Packages ##
####################

#' Vector of package names
packages <- c("dplyr",
              "ggspatial",
              "ggplot2",
              "mapview",
              "sf",
              "rmapshaper")

#' Function to load a package or install it if not already installed
load_packages <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    require(package_name, character.only = TRUE)
  }
}

#' Apply the function to each package 
lapply(packages, load_packages)

#' Set font to Times New Roman
windowsFonts(Times=windowsFont("Times New Roman"))

############################
## Shapefile Management ##
############################

#' Read in wildlife management unit shapefiles
#' Transform all shapefiles to Albers (5070)
#' st_zm removes the z dimensions
#' ms_simplify: Uses mapshaper to simplify polygons

#' Pennsylvania
pa.wmus <- st_read("Shapefiles/Pennsylvania WMUs/PGC_BNDWildlifeManagementUnits2021.shp") %>%
  st_transform(5070)%>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(pa.wmus)

#' New Jersey
nj.thas <-  st_read("Shapefiles/New Jersey Turkey Hunting Areas/NJ_THA.shp") %>%
  st_transform(5070) %>%
  st_zm()%>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(nj.thas)

#' Virginia
va.regions <-  st_read("Shapefiles/Virginia/Virginia4Regions.shp")%>%
  st_transform(5070) %>% 
  st_make_valid() %>%
  dplyr::group_by(Region) %>%
  summarise() %>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(va.regions)

#' Ohio
oh.wmus <-  st_read("Shapefiles/Ohio Counties and Wildlife Districts/wildllife_districts_Dissolve.shp") %>%
  st_transform(5070) %>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(oh.wmus)

#' West Virginia
wv.regions <-  st_read("Shapefiles/West Virginia/WVTurkeyRegions.shp") %>%
  st_transform(5070) %>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(wv.regions)


#' Maryland wmus (Thanks Jake!)
#' Project to Albers
md.wmus.ready <- st_read("Shapefiles/Final/md_wmu_boundaries.gpkg") %>%
  st_transform(5070) %>%
    rmapshaper::ms_simplify(keep = 0.007, weighting = 1) %>%
  dplyr::rename("geometry"=geom)

#' Check
mapview(md.wmus.ready)


#' New York
#' WMU shapefile consolidated and created by Jake
ny.wmus <- st_read("Shapefiles/New York WMUs/Wildlife_Management_Units.shp")%>%
  st_transform(5070) %>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Check
mapview(ny.wmus)

#############################
## Organize Spatial Data ##
############################

  ##------New Jersey-----##
#' Consolidate New Jersey into 3 turkey hunting areas
 nj.thas$THA <- ifelse(nj.thas$THA<11,"North",
                  ifelse(nj.thas$THA>10 & nj.thas$THA<15,"Central","South")) 

#' Summarise allows you to filter by a group's geometries
 nj.thas <-nj.thas %>%  dplyr::group_by(THA) %>%
                  summarise()
#' Check
 mapview(nj.thas, zcol="THA")
 
 #st_write(nj.thas, "nj.3thas.shp")

##------Pennsylvania------##
#' Consolidate Pennsylvania WMUs
#' Table that defines grouping of WMUs
 pa.wmus$WMU <- ifelse(substr(pa.wmus$WMU,1,1)=="1",1,
     ifelse(pa.wmus$WMU=="2A"|pa.wmus$WMU=="2C"|pa.wmus$WMU=="2D"|pa.wmus$WMU=="2E",2,
               ifelse(pa.wmus$WMU=="2B",3,
                  ifelse(pa.wmus$WMU=="2F"|pa.wmus$WMU=="2G"|pa.wmus$WMU=="2H"|pa.wmus$WMU=="3A"|pa.wmus$WMU=="3B",4,
                           ifelse(pa.wmus$WMU=="3C",5,
                                 ifelse(pa.wmus$WMU=="3D",6,
                                    ifelse(pa.wmus$WMU=="4A"|pa.wmus$WMU=="4B"|pa.wmus$WMU=="4D",7,
                                       ifelse(pa.wmus$WMU=="4C"|pa.wmus$WMU=="4E",8,
                                         ifelse(pa.wmus$WMU=="5A"|pa.wmus$WMU=="5B",9,10)))))))))

#' Check structure
str(pa.wmus)

#' Summarize like above to consolidate geometries
 pa.wmus1 <-pa.wmus %>%
   dplyr::group_by(WMU) %>%
   summarise() %>%
   st_as_sf()
 
 #' Check to see overlapping WMUs
 #' Addressed now
 ggplot() +
   geom_sf(data = pa.wmus, fill = 'lightblue', color = 'black') +
   #geom_sf(data = pa.wmus1, aes(label = MU), color = 'red') +
   geom_sf_text(data = pa.wmus, aes(label = WMU), size = 5, color = 'red') +
   theme_minimal()
   
 #' Check
mapview(pa.wmus1, zcol="WMU") +
mapview(pa.wmus, zcol= "WMU")

#' Write updated shapefile
#st_write(pa.wmus, "pa.10wmus.shp")

##------New York-----##
### Group WMUs
# Create lookup table that defines grouping of WMUs
## first reassign WMUs to the appropriate MU
ny.wmus$UNIT <- ifelse(ny.wmus$UNIT=="7M"|ny.wmus$UNIT=="7P"|ny.wmus$UNIT=="7S","4F",ny.wmus$UNIT)
ny.wmus$UNIT <- ifelse(ny.wmus$UNIT=="6R"|ny.wmus$UNIT=="6S","4F",ny.wmus$UNIT)
ny.wmus$UNIT <- ifelse(ny.wmus$UNIT=="6P","7F",ny.wmus$UNIT)
ny.wmus$UNIT <- ifelse(ny.wmus$UNIT=="5R"|ny.wmus$UNIT=="5S"|ny.wmus$UNIT=="5T","4C",ny.wmus$UNIT)

## then assign to MU
ny.wmus$MU <- ifelse(substr(ny.wmus$UNIT,1,1)=="2"|ny.wmus$UNIT=="1A","NYC",NA)
ny.wmus$MU <- ifelse(substr(ny.wmus$UNIT,1,1)=="9"|substr(ny.wmus$UNIT,1,1)=="8"|substr(ny.wmus$UNIT,1,1)=="7","West",ny.wmus$MU)
ny.wmus$MU <- ifelse(substr(ny.wmus$UNIT,1,1)=="4"|substr(ny.wmus$UNIT,1,1)=="3","South",ny.wmus$MU)
ny.wmus$MU <- ifelse(substr(ny.wmus$UNIT,1,1)=="5"|substr(ny.wmus$UNIT,1,1)=="6","North",ny.wmus$MU)
ny.wmus$MU <- ifelse(ny.wmus$UNIT=="1C","Long Island",ny.wmus$MU)

#' Summarize like above to consolidate geometries
ny.wmus1 <-ny.wmus %>%
  dplyr::group_by(MU) %>%
  summarise() %>%
  st_as_sf() 

#' Check
mapview(ny.wmus1)

#' Remove NYC and NA
ny.wmus1 <- ny.wmus1[-c(2, 6), ]

#' Check
mapview(ny.wmus1)

##-----Maryland----##

#' Filter the rows to combine
 rows_2combine <- md.wmus.ready %>% 
   filter(wmu %in% c("Upper Eastern Shore", "Lower Eastern Shore"))

#' Combine the filtered rows into a new geometry
combined_row <- rows_2combine %>%
  summarise(geometry = st_union(geometry), wmu = "Eastern Shore Combined")

#' Remove the original rows and bind the new combined row
md.wmus.ready <- md.wmus.ready %>%
  filter(!wmu %in% c("Upper Eastern Shore", "Lower Eastern Shore")) %>%
  bind_rows(combined_row) 

#' Check
mapview(md.wmus.ready)


################
## Outlines ##
################

#' Grab outline of USA states
#' Shapefile from Alberto that crops out the great lakes
sa.outline <- st_read("Shapefiles/USA_no_greatlakes/USA_adm1_without_great_lakes.shp")%>%
  st_transform(5070)%>%
  rmapshaper::ms_simplify(keep = 0.002, weighting = 1)

#' Remove the row for Maryland (assuming the identifier for Maryland is 'MD')
sa.outline.noMD <- sa.outline %>%
  filter(NAME_1 != "Maryland")

#' Check structure
str(sa.outline)

#' Read in separate Maryland Shapefile
#' Grab outline of USA states
#' Shapefile from Alberto that crops out the great lakes
md.outline <- st_read("Shapefiles/Final/State_Boundaries.shp") %>%
  st_transform(5070) %>%
  dplyr::filter(NAME=="Maryland") %>%
  rmapshaper::ms_simplify(keep = 0.008, weighting = 1)

mapview(md.outline)
 
#################
## Create Map ##
#################

#' Crop Virginia to fix Chesapeake Bay Issue
va.updated <- sa.outline[sa.outline$NAME_1=="Virginia",]
va.regions <- st_intersection(va.regions, va.updated)
mapview(va.regions)

#' Combine shapefiles into one sf object
combined_sf <- bind_rows(pa.wmus1,va.regions, oh.wmus, wv.regions, ny.wmus1, md.wmus.ready, nj.thas) 

#' Crop map to study area by using st_bbox
box_zoom = (st_bbox(c(xmin= 852757.4, ymin= 1376761, xmax= 1991072, ymax= 2658539))) %>%
  st_as_sfc() %>% st_set_crs(5070)

#' Check
ggplot()+
  geom_sf(data= sa.outline)+
  geom_sf(data= box_zoom)

#' Crop map to our study area
zoomed <- st_intersection(sa.outline.noMD, box_zoom)
plot(st_geometry(zoomed))

#' Check structure
str(combined_sf)

#' Map data
#' Drop Z dimensions from feature geometries, resetting classes
map_data <- st_zm(sa.outline.noMD,drop=TRUE, what="ZM") 
ggplot()+
  geom_sf(data = map_data) 

#' Check structure
str(map_data)

#' Visualize map
map <- ggplot() +
  geom_sf(data = zoomed, color = "black", fill="white", linewidth= 0.4)+
  geom_sf(data = combined_sf, color = "grey48", fill="grey90")  +
  geom_sf(data = zoomed, color = "black", fill="transparent", linewidth= 0.4)+
  geom_sf(data = md.wmus.ready, color = "grey48", fill="grey90", linewidth= 0.4)+
  geom_sf(data = md.outline, color = "black", fill="transparent", linewidth= 0.4)+
  theme_classic() +
  theme(axis.text = element_text(size = 11, family="sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotation_north_arrow(location = 'br',
                         style = north_arrow_nautical(text_family = "sans"),
                         which_north = "false") +
  annotation_scale(style = "bar",
                   pad_x = unit(0.05, "in"),
                   pad_y = unit(0.05, "in"),
                   text_family = "sans")
#' Plot map
map


#' Visualize Maryland
map <- ggplot() +
  geom_sf(data = md.outline, color = "black", fill="transparent")+
  theme_classic() +
  theme(axis.text = element_text(size = 11, family="sans"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  annotation_north_arrow(location = 'br',
                         style = north_arrow_nautical(text_family = "sans"),
                         which_north = "false") +
  annotation_scale(style = "bar",
                   pad_x = unit(0.05, "in"),
                   pad_y = unit(0.05, "in"),
                   text_family = "sans")
#' Plot map
map
sessionInfo()

