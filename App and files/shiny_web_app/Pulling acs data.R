######## Libraries #########

library(tidycensus)
library(tidyverse)
library(purrr)


######## Constants #########

ACS_YEAR = 2017

###### pulling the vars that are needed ##########

# test_profile = load_variables(ACS_YEAR, 'acs5/profile')
# test_subject = load_variables(ACS_YEAR, 'acs5/subject')
# test = load_variables(ACS_YEAR, 'acs5')
setwd('shiny_web_app/')

vars_needed = read.csv('variable_mapping.csv', stringsAsFactors = FALSE, header = TRUE)
acs_vars_needed = vars_needed[vars_needed$Dataset == 'ACS',]

acs_codes = unique(acs_vars_needed$var_name)

###### seting up the census api #########

census_key = readLines('C:\\Users\\gehami\\Documents\\API Keys\\census_api_key.txt')
census_api_key(key = census_key)


##### downloading the data ##########

#shout out to Kyle walker for this code snippet on grabbing every state's census tracts
#https://walkerke.github.io/2017/05/tidycensus-every-tract/
us <- unique(fips_codes$state)[1:2]

all_acs_dat <- map_df(us, function(x) {
  get_acs(geography = "tract", variables = acs_codes[1], year = ACS_YEAR, survey = 'acs5',
          state = x, cache_table = TRUE)
})
all_acs_dat$moe = NULL
colnames(all_acs_dat)[colnames(all_acs_dat) == 'estimate'] = all_acs_dat$variable[1]
all_acs_dat$variable = NULL
for(n in 2:length(acs_codes)){
  new_var <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = acs_codes[n], year = ACS_YEAR, survey = 'acs5',
            state = x, cache_table = TRUE)
  })
  all_acs_dat = cbind(all_acs_dat, estimate = new_var$estimate)
  colnames(all_acs_dat)[colnames(all_acs_dat) == 'estimate'] = new_var$variable[1]
  
}

##### saving the data #########

saveRDS(all_acs_dat, paste0('data_tables/all_acs_dat_',ACS_YEAR, '.rds'))




###### identifying only the tracts in the 500 cdc cities ##########


cdc_dat = readRDS('data_tables/cdc_2018.rds')
city_list = data.frame(city = cdc_dat$placename, state = cdc_dat$stateabbr, stringsAsFactors = FALSE)[!duplicated(cdc_dat$placename),]

#for each city, I need to download that city's shapefile, and then find all the tracts that are within or intersect with that city.
city_to_county_map = read.csv('uscities-counties.csv', stringsAsFactors = FALSE)

length(which(city_list$city %in% city_to_county_map$city)) == length(city_list$city) #confirming all cdc cities are in city-to-county-map
city_counties = city_to_county_map[which(paste0(city_to_county_map$city, city_to_county_map$state_id) %in% paste0(city_list$city, city_list$state)),
                                   which(colnames(city_to_county_map) %in% c("city", "county_name", 'county_fips', 
                                                                                        'state_name', 'state_id'))]

city_counties = city_counties[order(city_counties$city, city_counties$state_id),]
city_counties$county_fips = substr(city_counties$county_fips, nchar(city_counties$county_fips) - 2, nchar(city_counties$county_fips))
city_list = city_list[order(city_list$city, city_list$state),]
#now that the two lists are identical and ordered, we can append them
city_county_list = data.frame(city_list, city_counties, stringsAsFactors = FALSE)


n = 1
city_name = city_county_list$city[n]
county_fips = city_county_list$county_fips[n]
state_abb = city_county_list$state_id[n]


library(rgeos)
library(tigris)
library(sp)
library(hash)

#given a single row in an spdf, returns spdf row with only the largest polygon (by area)
get_largest_shape = function(spdf, row_id = 1){
  require(sp)
  spolys = spdf@polygons[[1]]@Polygons
  max_area = 0
  for(n in seq_along(spolys)){
    if(spolys[[n]]@area > max_area){
      max_area = spolys[[n]]@area
      max_area_n = n
    }
  }
  single_poly = Polygons(list(spolys[[max_area_n]]), row_id)
  single_spoly = SpatialPolygons(list(single_poly))
  proj4string(single_spoly) = spdf@proj4string
  data_for_spdf = data.frame(spdf@data[1,], stringsAsFactors = FALSE, row.names = row_id)
  ret_spdf = SpatialPolygonsDataFrame(single_spoly, data = data_for_spdf, match.ID = TRUE)
  return(ret_spdf)
}
#given two shapes (spdf), determines what %age of points one is within the other
points_in_shape = function(shape, shape_within, ret_perc = TRUE, n = 1){
  points = SpatialPoints(shape_within[n,]@polygons[[1]]@Polygons[[1]]@coords, proj4string = shape_within@proj4string)
  if(ret_perc) return(length(which(!is.na(over(points, shape)[,1])))/nrow(points@coords))
  return(length(which(!is.na(over(points, shape)[,1]))))
}
#given all of the census tracts, returns the census tracts that are within the city limits and additional tracts 
tracts_in_shape = function(tracts, shape, contain_threshold = .5){
  require(rgeos)
  if(contain_threshold <= 0){
    return(tracts[which(gIntersects(shape, tracts, byid = TRUE)),])
  }
  contain_tracts = tracts[which(gContains(shape, tracts, byid = TRUE)),]
  if(is.numeric(contain_threshold) & contain_threshold < 1){
    border_tracts = tracts[which(gOverlaps(shape, tracts, byid = TRUE)),]
    keep_tracts = NULL
    for(n in seq_len(nrow(border_tracts))){
      if(points_in_shape(shape, border_tracts, ret_perc = TRUE, n) > contain_threshold){
        keep_tracts = c(keep_tracts, n)
      }
    }
    contain_tracts = rbind(contain_tracts, border_tracts[keep_tracts,])
  }
  return(contain_tracts)
}

#given the city name, the county fips for the city, and the state abbreviation, returns all of the tracts in that city
get_tracts_in_city = function(city_name, county_fips, state_abb){
  require(rgeos)
  require(tigris)
  county_tracts = tracts(state_abb, county_fips, cb = TRUE)
  city_shape = places(state_abb, cb = TRUE)
  city_shape = city_shape[city_shape$NAME == city_name,] %>% get_largest_shape()

  # shape = city_shape
  # tracts = county_tracts
  ret_tracts = tracts_in_shape(county_tracts, city_shape, contain_threshold = 0)
  return(ret_tracts$GEOID)
}


#creating a hash for each city to have its own list of tracts, and a large list of all the tracts to include
include_tracts = NULL
fail_cities = NULL
tract_city_dictionary = hash::hash()

for(n in seq_along(city_county_list[,1])){
  city_name = city_county_list$city[n]
  county_fips = city_county_list$county_fips[n]
  state_abb = city_county_list$state_id[n]
  city_state_name = paste0(city_name, ' ', state_abb)
  
  tryCatch({tracts_in_city = get_tracts_in_city(city_name, county_fips, state_abb)
  include_tracts = c(include_tracts, tracts_in_city)
  tract_city_dictionary[[city_state_name]] = tracts_in_city},
  error = function(e){
    fail_cities = c(fail_cities, city_state_name)
  })
}


#seeing which cities were not included in the has
missing_cities = city_county_list[which(!(paste0(city_county_list$city, ' ', city_county_list$state_id) %in% keys(tract_city_dictionary))),]

#we will need to figure out how to add those cities into the hash. all told there are 21 missing cities, so not too bad.

###### filtering the acs data down to only the tracts in the 500 cdc cities ##########




