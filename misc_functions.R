#Functions
library(tidyverse)
library(sf)
library(tmap)
library(pryr)
sf::sf_use_s2(FALSE)
options(scipen = 99)

#TAKE IN TARGET POLYGON OR POLYGONS, OUTPUT AN SF OBJECT OF INPUT POLYGONS THAT OVERLAP IT
#ASSIGN TO TARGET POLYGONS BASED ON WHICH PART HAS MAXIMUM AREA
#Let me try and explain that in English.
#E.g. If we have an SF object of local authorities
#And another of a smaller set of geographies, some of which may overlap more than one local authority
#Do two things:
#1. Keep only the smaller geographies that overlap with any local authorities we have here
#so some can  go beyond the local authority boundaries
#2. Assign a local authority label to the smaller geography based on which LA it most overlaps
#i.e. if it's broken up into more than one LA, which of those pieces has the largest area?

#It might be we input just a single polygon to check overlap with, like the Y&H old government office region
#Then the function just outputs what other geographies have any contact with that single geography
getoverlaps <- function(targetpolygons, inputpolygons, targetID, inputID, returntype = 'lookup'){
  
  #We need the inputID
  if (missing(targetID)) {
    stop("The argument 'inputID' must be provided.") 
  }
  
  if (missing(inputID)) {
    stop("The argument 'inputID' must be provided.") 
  }
  
  if (!returntype %in% c('lookup','sf')) {
    stop("Return type must be one of 'lookup' or 'sf'.") 
  }
  
  #polygon ID to use to pick largest area 
  targetID <- enquo(targetID)
  inputID <- enquo(inputID)
  
  #Get intersection
  overlap <- inputpolygons %>% st_intersection(targetpolygons)
  
  #km^2
  overlap$area <- as.numeric(st_area(overlap))/1000000
  
  #Flag largest areas for each input polygon in a separate non geo df
  lookup <- overlap %>% 
    st_set_geometry(NULL) %>% 
    group_by(!!inputID) %>% 
    mutate(largest_area = ifelse(area == max(area), 1,0)) %>% 
    filter(largest_area == 1)
  
  
  if(returntype=='lookup'){
  
    return(lookup)
    
  } else if(returntype=='sf'){
    
    #else return the actual input polygons with the labels of the largest target polygon attached
    #Add in only the targetID column name
    inputpolygons %>%
      left_join(lookup %>% select(!!inputID, !!targetID), by = quo_name(inputID)) %>% 
      return()
    
  }
  
}
