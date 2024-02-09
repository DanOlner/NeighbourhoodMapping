#NIEIGHBOURHOOD MAPPING
library(tidyverse)
library(sf)
library(tmap)
sf::sf_use_s2(FALSE)
options(scipen = 99)

#Some manual deletes of corrupted columns in the exported CSV to get sf to be able to parse WKT
df <- read_csv('data/Sheffield Neighbourhood Mapping - anonymised responses to 09.02.24doedit.csv')
df <- st_as_sf(x = df, wkt = "WKT Click the button below to start drawing on the map", crs = 4326)

plot(st_geometry(df[10:21,]))
plot(st_geometry(st_make_valid(df[10:21,])))
plot(st_geometry(df))

#Get bounding box to remove any additions beyond a range we want
shef <- osmdata::getbb("South Yorkshire")
#expand southwards slightly
shef[2,1] <- 53.2

#Adapted from https://r-spatial.github.io/sf/reference/st_bbox.html
a = st_sf(a = 1:2, geom = st_sfc(st_point(c(shef[1,1],shef[2,1])), st_point(c(shef[1,2],shef[2,2]))), crs = 4326)
st_bbox(a)
st_as_sfc(st_bbox(a))

#Check
plot(st_geometry(df))
plot(st_geometry(st_as_sfc(st_bbox(a))), add = T)

sf::sf_use_s2(FALSE)

#Reduce to that bounding box
#Table number of rows beyond it
table(lengths(st_intersects(df,st_as_sfc(st_bbox(a)))))


#Filter
df <- df %>% 
  filter(
    lengths(st_intersects(.,st_as_sfc(st_bbox(a)))) > 0
    )

#How many unique respondents? 342
length(unique(df$`Respondent ID`))
table(table(df$`Respondent ID`))



#Filter out invalid shapes e.g. the lines that have been drawn
#Their area should be low...
# df <- df %>% 
#   mutate(m2 = as.numeric(st_area(.))/1000000)
# df$area <- st_area(df)
# df$km2 <- as.numeric(st_area(df))/1000000
# 
# #This doesn't quite work, need another filtering method
# plot(st_geometry(df %>% filter(km2 > 10)))
# plot(st_geometry(df %>% filter(km2 <= 10)))
# plot(st_geometry(df %>% filter(km2 <= 0.1)))
# 
# #Not working to filter. Check in QGIS
# st_write(df,'data/qgis/test_geog.shp')
# 
# tmap_mode('view')
# 
# # tm_shape(st_make_valid(df[5:31,])) +
# tm_shape(df[5:31,]) +
#   tm_borders()
# 
# tm_shape(st_make_valid(df[5:31,])) +
#   tm_polygons(id = 'Thinking about this neighbourhood, is there a place, organisation or person that you think of as being at the heart of it? List as many things as you like.', alpha = 0)
# 
# 
# tmap_mode('plot')
# 
# # dfchk <- st_cast(df, to = 'POLYGON')
# 
# #plot the lot to see which is weird, work out filtering process
# for(i in 1:nrow(df)){
#   
#   tryCatch(
#   
#   {
#     tm <- tm_shape(st_make_valid(df[i,])) +
#     tm_borders()
#   
#     tmap_save(tm,paste0('local/polygonchecks/',i,'.png'))
#   
#   },
#   #if an error occurs, tell me the error
#   error=function(e) {
#     message('An Error Occurred')
#     print(e)
#   },
#   #if a warning occurs, tell me the warning
#   warning=function(w) {
#     message('A Warning Occurred')
#     print(w)
#     return(NA)
#   })
#   
# }

#Test methods for deducing which are not full polygons
polygons <- df

# Calculate the convex hull for each polygon and its area
convex_hull <- st_convex_hull(polygons)
convex_hull$convex_hull_km2 <- as.numeric(st_area(convex_hull))/1000000

# Calculate convexity measure
convex_hull$convexity_measure <- convex_hull$km2 / convex_hull$convex_hull_km2

# Filtering based on convexity measure
convexity_threshold <- 0.5 # Example threshold
well_formed_polygons_convexity <- polygons[convex_hull$convexity_measure > convexity_threshold, ]

plot(st_geometry(well_formed_polygons_convexity))


#Compare to area/perimeter ratio test

# Calculate area and perimeter for each polygon
polygons$perimeter <- as.numeric(st_length(polygons))/1000

# Calculate area to perimeter ratio
polygons$area_perimeter_ratio <- polygons$km2 / polygons$perimeter

# Threshold for determining well-formed polygons might need to be adjusted based on dataset
# threshold <- median(polygons$area_perimeter_ratio) # Example threshold
threshold <- quantile(polygons$area_perimeter_ratio, probs = 0.2) # Example threshold, quantile
well_formed_polygons <- polygons[polygons$area_perimeter_ratio > threshold, ]

plot(st_geometry(well_formed_polygons))


#Check the rejects?
tmap_mode('view')

#THIS ONE'S WINNING
threshold <- quantile(polygons$area_perimeter_ratio, probs = 0.05) # Example threshold, quantile
badly_formed_polygons <- polygons[polygons$area_perimeter_ratio < threshold, ]
# badly_formed_polygons_convexity <- polygons[convex_hull$convexity_measure < convexity_threshold, ]

# tm_shape(st_make_valid(df[5:31,])) +
# tm_shape(well_formed_polygons_convexity) +
tm_shape(badly_formed_polygons) +
  tm_borders()

#Check keeps
tm_shape(well_formed_polygons) +
  tm_borders()


#Overlap?
tmap_mode('plot')
tmap_mode('view')

tm_shape(well_formed_polygons) +
  tm_polygons(alpha = 0.1) +
  tm_borders()

#Being a pain, try QGIS
st_write(well_formed_polygons,'data/qgis/wellformedpolygons.shp')




