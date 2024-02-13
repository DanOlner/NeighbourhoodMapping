library(tidyverse)
library(sf)
library(tmap)
library(pryr)
sf::sf_use_s2(FALSE)
options(scipen = 99)

#Get Y&H boundaries for various bodies
#Compare, work towards overall map

#Note ONS diagram on various available geographies
#https://open-innovations.org/data/geographies
#The original: https://geoportal.statistics.gov.uk/documents/ons::hierarchical-representation-of-uk-statistical-geographies-august-2022-1/explore

#Let's start with local authorities
#Downloaded from https://geoportal.statistics.gov.uk/datasets/127c4bda06314409a1fa0df505f510e6_0/explore
la <- st_read('../../MapPolygons/UK/LocalAuthorityDistricts/Local_Authority_Districts_December_2023_Boundaries_UK_BFC_9042356933902664268/LAD_DEC_2023_UK_BFC.shp')

plot(st_geometry(la))

#Looking just at the Y&H region (old GOR now ITL1)
#https://geoportal.statistics.gov.uk/datasets/dab19b99e737499d92a99844163e7099_0/explore
regions <- st_read('../../MapPolygons/UK/ITL1/International_Territorial_Level_1_January_2021_UK_BFC_2022_6357033457384992142/ITL1_MAY_2021_UK_BFC.shp')

plot(st_geometry(regions))


#Get region / LA overlap and work out areas
#Keep largest in Y&H
la.overlap <- la %>% st_intersection(regions)

#km^2
la.overlap$area <- as.numeric(st_area(la.overlap))/1000000

#How many LAs get split across different regions?
#Many splits may be tiny slivers on edges that don't match
#Rather than substantially overlapping
table(table(la.overlap$LAD23NM))

#Flag largest areas for each LA
#Don't need the geometry at this point, as we're just going to use this to subset the original LA list
la.overlap <- la.overlap %>% 
  st_set_geometry(NULL) %>% 
  group_by(LAD23NM) %>% 
  mutate(largest_area = ifelse(area == max(area), 1,0))


#Just keep the lookup components for the largest area overlaps
#Region is actually in the LA code but no name from that
la.itl1lookup <- la.overlap %>% filter(largest_area == 1) %>% select(LAD23CD,LAD23NM,ITL121CD,ITL121NM)

write_csv(la.itl1lookup,'data/localauthority_itl1lookup.csv')


#Y&H local authorities
ynh.la <- la %>% filter(LAD23NM %in% la.itl1lookup$LAD23NM[la.itl1lookup$ITL121NM=='Yorkshire and The Humber'])

#Check
plot(st_geometry(ynh.la), border = 'blue')
plot(st_geometry(regions %>% filter(ITL121NM == 'Yorkshire and The Humber')), lwd = 3, add = T)






