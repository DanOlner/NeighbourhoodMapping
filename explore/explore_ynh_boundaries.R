library(tidyverse)
library(sf)
library(tmap)
library(pryr)
library(leaflet)
sf::sf_use_s2(FALSE)
options(scipen = 99)
source('misc_functions.R')

#Get Y&H boundaries for various bodies
#Compare, work towards overall map

#Note ONS diagram on various available geographies
#https://open-innovations.org/data/geographies
#The original: https://geoportal.statistics.gov.uk/documents/db2a0b3bee594f96a4869644fc32395b/explore

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
ynh.la <- la %>% filter(
  LAD23NM %in% la.itl1lookup$LAD23NM[la.itl1lookup$ITL121NM=='Yorkshire and The Humber']
  )

#Overlapping Y&H, proposed Greater Lincolnshire Combined Authority
#(Which would mean every Y&H LA was in a CA)
#https://www.northlincs.gov.uk/site/documents/your-council/greater-lincolnshire-devolution-combined-county-authority-proposal
#So let's add that one in to the Y&H+ list of LAs

#List of district councils that make up Lincolnshire County Council
#https://en.wikipedia.org/wiki/Lincolnshire_County_Council#Governance
ynh.la <- ynh.la %>% 
  bind_rows(la %>% filter(LAD23NM %in% c('Boston','Lincoln','East Lindsey','North Kesteven','South Holland','South Kesteven','West Lindsey')))




#Combined authorities just for Y&H, manually combining
#Gets North Yorkshire too, but going to label as 'open consultation'
# ynh.ca <- la %>% filter(grepl('Sheff|Rotherham|Doncast|Barns|Bradford|Calderdale|Kirklees|Leeds|Wakefield|York|North York|Hull',LAD23NM, ignore.case = T)) 
ynh.la <- ynh.la %>% 
  mutate(
    combinedauthorityname = case_when(
      grepl('Sheff|Rotherham|Doncast|Barns',LAD23NM, ignore.case = T) ~ 'South Yorkshire Combined Mayoral Authority',
      grepl('Bradford|Calderdale|Kirklees|Leeds|Wakefield',LAD23NM, ignore.case = T) ~ 'West Yorkshire Combined Authority',
      LAD23NM %in% c('York','North Yorkshire') ~ 'York and North Yorkshire Combined Authority',
      grepl('Hull|riding',LAD23NM, ignore.case = T) ~ 'Hull and East Yorkshire Mayoral Combined Authority',
      LAD23NM %in% c('Boston','Lincoln','East Lindsey','North Kesteven','South Holland','South Kesteven','West Lindsey','North Lincolnshire','North East Lincolnshire') ~ 'Greater Lincolnshire Combined Authority'
    )
  )




#Check
plot(st_geometry(ynh.la), border = 'blue')
plot(st_geometry(regions %>% filter(ITL121NM == 'Yorkshire and The Humber')), lwd = 3, add = T)

tmap_mode('view')

tm_shape(ynh.la) +
  tm_polygons(col = 'combinedauthorityname', id = 'LAD23NM', alpha = 0.3) +
  tm_shape(regions %>% filter(ITL121NM == 'Yorkshire and The Humber')) +
  tm_borders(lwd = 4)


#~~~~~~~~~~~~~~~~~~~~~
#OTHER GEOGRAPHIES----
#~~~~~~~~~~~~~~~~~~~~~

#Health / Integrated care boards
#https://geoportal.statistics.gov.uk/datasets/76dad7f9577147b2b636d4f95345d28d_0/explore?location=52.830551%2C-2.000000%2C7.52
icb <- st_read('D:/Dropbox/MapPolygons/England/2023/Integrated_Care_Boards_April_2023_EN_BSC_-68820443008618605/ICB_APR_2023_EN_BSC.shp')

#Use function version to create larger polygon / input polygon lookup
#GOR regions as target
# debugonce(getoverlaps)
# lookup <- getoverlaps(regions, icb, targetID = ITL121NM, inputID = ICB23NM)

#SF with region label added
icb.w.GOR <- getoverlaps(regions, icb, targetID = ITL121NM, inputID = ICB23NM, returntype = 'sf')

#Do directly, don't load in twice
fire.w.GOR <- getoverlaps(regions, st_read('D:/Dropbox/MapPolygons/EnglandWalesMerged/2022/Fire_and_Rescue_Authorities_December_2022_EW_BGC_-4716477646245389191/FRA_DEC_2022_EW_BGC.shp'), targetID = ITL121NM, inputID = FRA22NM, returntype = 'sf')

police.w.GOR <- getoverlaps(regions, st_read('D:/Dropbox/MapPolygons/EnglandWalesMerged/2022/Police_Force_Areas_December_2022_EW_BFE_-4079265021907526758/PFA_DEC_2022_EW_BFE.shp'), targetID = ITL121NM, inputID = PFA22NM, returntype = 'sf')
  
#NHS regions
NHSregion.w.GOR <- getoverlaps(regions, st_read('D:/Dropbox/MapPolygons/England/2019/NHS_Region_Local_Offices_April_2019_GCB_EN_2022_7882941909319148229/NHS_Region_Local_Offices_April_2019_GCB_EN.shp'), targetID = ITL121NM, inputID = nhsrlo19nm, returntype = 'sf')




#Add a bunch to the same map as selectable layers
ynh.icb <- icb.w.GOR %>% filter(ITL121NM == 'Yorkshire and The Humber')

#https://stackoverflow.com/questions/53094379/in-r-tmap-how-do-i-control-layer-visibility-in-interactive-mode
tm <- tm_shape(ynh.la %>% rename(`Combined Authority` = combinedauthorityname), name = 'Local authorities') +
  tm_polygons(col = 'Combined Authority', id = 'LAD23NM', alpha = 0.3) +
  tm_shape(regions %>% filter(ITL121NM == 'Yorkshire and The Humber'), name = 'Yorkshire & The Humber') +
  tm_borders(lwd = 8, alpha = 0.2) +
  tm_shape(NHSregion.w.GOR %>% filter(nhsrlo19cd %in% c('E39000048','E39000047')), name = 'NHS region') +#hack until fixing function to get any overlaps
  tm_polygons(lwd = 4, col = 'cyan', border.col = 'cyan4', alpha = 0.3, id = 'nhsrlo19nm') + 
  # tm_shape(NHSregion.w.GOR %>% filter(ITL121NM == 'Yorkshire and The Humber'), name = 'NHS region') +
  # tm_polygons(lwd = 4, col = 'cyan', border.col = 'cyan4', alpha = 0.3) + 
  tm_shape(ynh.icb, name = 'Integrated Care Boards') +
  tm_polygons(lwd = 4, col = 'green', border.col = 'darkgreen', alpha = 0.3, id = 'ICB23NM') + 
  tm_shape(fire.w.GOR %>% filter(ITL121NM == 'Yorkshire and The Humber'), name = 'Fire & rescue authorities') +
  tm_polygons(lwd = 4, col = 'red', border.col = 'darkred', alpha = 0.3, id = 'FRA22NM') + 
  tm_shape(police.w.GOR %>% filter(ITL121NM == 'Yorkshire and The Humber'), name = 'Police force areas') +
  tm_polygons(lwd = 4, col = 'blue', border.col = 'darkblue', alpha = 0.3, id = 'PFA22NM')  
  
tm %>% 
  tmap_leaflet() %>%
  leaflet::hideGroup("Integrated Care Boards") %>% 
  leaflet::hideGroup("NHS region") %>% 
  leaflet::hideGroup("Fire & rescue authorities") %>% 
  leaflet::hideGroup("Police force areas") 










