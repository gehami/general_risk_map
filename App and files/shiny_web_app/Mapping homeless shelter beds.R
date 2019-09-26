######### Libraries #########
STOPSTOPSTOP}}}}

library(devtools)


# devtools::install_github("dkahle/ggmap")
library(ggmap)


library(leaflet)
library(tidyverse)
library(magrittr)
library(dplyr)
library(sp)
library(htmltools)

##### directory #######
setwd("shiny_web_app")

######## Constants #######

PROJ_4_STRING = CRS("+proj=longlat +datum=WGS84")


############ Globally used functions #############
#given a vector of numeric values, or something that can be coerced to numeric, returns a vector of the percentile each observation is within the vector.
#Example: if vec == c(1,2,3), then get_percentile(vec) == c(0.3333333, 0.6666667, 1.0000000)
get_percentile = function(vec, compare_vec = NULL){
  if(is.null(compare_vec)){
    return(ecdf(vec)(vec))
  }else{
    new_vec = rep(0, length(vec))
    for(n in seq_along(vec)){
      new_vec[n] = ecdf(c(vec[n], compare_vec))(c(vec[n], compare_vec))[1]
    }
    return(new_vec)
  }
}
#given a vector of numeric values, and the number of bins you want to place values into, returns a vector of 'vec' length where each observation is the quantile of that observation.
#Example: if vec == c(1,2,3), then get_quantile(vec, quantile_bins = 2) = as.factor(c(0, 50, 50)). 
#To have the top observation be marked as the 100%ile, set ret_100_ile == TRUE. 
#To return a factor variable, ret_factor == TRUE, otherwise it will return a numeric vector. 
get_quantile = function(vec, quantile_bins, ret_factor = TRUE, ret_100_ile = FALSE, compare_vec = NULL){
  quantile_bins = round(min(max(quantile_bins, 2), 100)) #ensuring the quantile bins is an integer between 2 and 100
  quant_val = floor(get_percentile(vec, compare_vec)*100 / (100/quantile_bins)) * (100/quantile_bins)
  if(!ret_100_ile){ quant_val[quant_val == 100] = unique(quant_val)[order(-unique(quant_val))][2]}
  if(ret_factor){return(factor(quant_val))}
  return(quant_val)
}
######### geocoding the addresses ##########
api <- readLines("C:\\Users\\gehami\\Documents\\API Keys\\apikey.txt")
register_google(key = api)

#given a vector of addresses, returns the geocoded data
geocode_addys = function(addy_list){
  geocoded <- data.frame(name = addy_list, lon = 0, lat = 0, geoaddress = "", stringsAsFactors = FALSE)
  
  # Loop through the addresses to get the latitude and longitude of each address and add it to the
  # origAddress data frame in new columns lat and lon
  for(i in 1:nrow(geocoded)){
    # Print("Working...")
    worked = FALSE
    n = 1
    while(!worked){
      try({result <- geocode(addy_list[i], output = "latlona", source = "google")
      worked = TRUE
      })
      n = n+1
      if(n > 100) break
    }
    
    geocoded$lon[i] <- as.numeric(result[1])
    geocoded$lat[i] <- as.numeric(result[2])
    geocoded$geoaddress[i] <- as.character(result[3])
  }
  return(geocoded)
}


###### geocoding shelters #######
shelter_dat = read.csv("long-beach_housing_inventory_count.csv", stringsAsFactors = FALSE)
shelters =shelter_dat$Organization.Name[shelter_dat$Organization.Name != '']
shelters = paste0(as.character(shelters), ", Long Beach, CA")
shelters[shelter_dat$Filler_address != ""] = shelter_dat$Filler_address[shelter_dat$Filler_address != ""]

geocoded_shelters = geocode_addys(shelters)

####### geocoding the foodbanks and making spatial database - foodbanks_spdf ##########
food_banks = read.csv('Food banks in long beach.csv', stringsAsFactors = FALSE)

geocoded_foodbanks = geocode_addys(food_banks$address)

ma_foodbanks = data.frame(org = gsub('â€™|â€\u0090', '',food_banks$ï..name), phone = gsub('â€\u0090','-',food_banks$phone), 
                     geocoded_foodbanks[,-1], stringsAsFactors = FALSE)

food_banks_spdf = SpatialPointsDataFrame(coords = cbind(lon = as.numeric(ma_foodbanks$lon), lat = as.numeric(ma_foodbanks$lat)),
                                         data = ma_foodbanks, proj4string = PROJ_4_STRING)

##### Putting together the final shelter spatial database - shelter_spdf #########

ma_shelter = cbind(geocoded_shelters[,-1], shelter_dat) %>% select(org = Organization.Name, project = Project.Name, proj_type = Proj..Type, target_pop = Target.Pop.,
                                                          house_type = Housing.Type,
                                                          beds_hh_kids = Beds.HH.w..Children, beds_hh_no_kids = Beds.HH.w.o.Children,
                                                          year_round_beds = Year.Round.Beds, pit_count = PIT.Count, tot_beds = Total.Beds, utilization_rate = Utilization.Rate,
                                                          lon, lat, geoaddress)

ma_shelter$house_type = gsub(pattern = '[ ]?â€“', replacement = ':', x = shelter_dat$Housing.Type)

#collapsing the data
rowaddys = unique(ma_shelter$geoaddress)

#given a set of rows with the same geoaddress, collapses them properly into a single row
collapse_rows = function(rowaddy, ma_shelter){
  rows = ma_shelter[ma_shelter$geoaddress == rowaddy,]
  if(length(unique(rows$project)) > 1){
    rows$project = "multiple projects"
  }
  rows$proj_type = paste(unique(rows$proj_type), collapse = ', ')
  if(is.na(rows$proj_type[1])) rows$proj_type = ""
  rows$house_type = paste(unique(gsub(":[[:print:]]*", '', rows$house_type)), collapse = ' & ')
  ret_row = data.frame(org = rows$org[1], project = rows$project[1], proj_type = rows$proj_type[1], target_pop = rows$target_pop[1], house_type = rows$house_type[1], 
                       beds_hh_kids = sum(rows$beds_hh_kids), beds_hh_no_kids = sum(rows$beds_hh_no_kids), year_round_beds = sum(rows$year_round_beds), 
                       pit_count = sum(rows$pit_count), tot_beds = sum(rows$tot_beds), utilization_rate = sum(rows$pit_count)/sum(rows$tot_beds), 
                       lon = rows$lon[1], lat = rows$lat[1], geoaddress = rows$geoaddress[1], stringsAsFactors = FALSE)
  return(ret_row)
}

shelter_map_dat = collapse_rows(rowaddys[1], ma_shelter)
for(n in 2:length(rowaddys)){
  shelter_map_dat = rbind(shelter_map_dat, collapse_rows(rowaddys[n], ma_shelter))
}
for(col in 1:ncol(shelter_map_dat)){
  shelter_map_dat[is.na(shelter_map_dat[,col]),col] = ""
}

#making the labels
attach(shelter_map_dat)
labels = paste(sep = "<br/>", paste0(org, '- ', project), paste0("Beds: ", tot_beds), paste0('Occupied: ', pit_count, " (", round(as.numeric(utilization_rate), digits = 2)*100, "% occpuancy)"),
               paste0("Housing: ", house_type), paste0("Address: ", gsub(", ca [0-9]*, usa", '', geoaddress))) 
detach(shelter_map_dat)

shelter_map_dat$label = labels

shelter_spdf = sp::SpatialPointsDataFrame(coords = cbind(lon = as.numeric(shelter_map_dat$lon), lat = as.numeric(shelter_map_dat$lat)),
                                          data = shelter_map_dat, proj4string = PROJ_4_STRING)


####### mapping out the shelter_spdf ###########

##### starter map #############
lon_med = median(shelter_spdf@coords[,1])
lat_med = median(shelter_spdf@coords[,2])

map <- leaflet() %>% 
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  # focus map in a certain area / zoom level
  setView(lng = lon_med, lat = lat_med, zoom = 12) 






##### Adding the shelters ######

#adds a legend of circles to show size
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border-radius: 50%")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px; margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
}



QUANT_BINS = 5
RET_FACTOR = TRUE
RET_100_ILE = TRUE
COMPARE_VEC = c(seq(0,1, length.out = QUANT_BINS+1))
#see here for color palletes: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
COLOR_PAL = 'Spectral'

circle_marker_radius_values = log10(as.numeric(shelter_spdf@data$tot_beds)^2)*5
color_val = get_quantile(as.numeric(shelter_spdf@data$utilization_rate), quantile_bins = QUANT_BINS, 
                         ret_factor = RET_FACTOR, ret_100_ile = RET_100_ILE, compare_vec = COMPARE_VEC)

map_pal = colorFactor(
  palette = COLOR_PAL,
  domain = color_val
)

shelter_map <- map %>% addCircleMarkers(data = shelter_spdf, radius = circle_marker_radius_values, 
                                        label = lapply(shelter_spdf@data$label, HTML), stroke =TRUE, 
                                        color = ~map_pal(color_val),
                                        opacity = .8) %>% 
  addLegend(position = "topright", pal = map_pal, title = "Bed Utilization Rate",
            values = color_val) 
  #%>% addLegendCustom(colors = 'grey', labels = round(seq(0, max(as.numeric(shelter_spdf@data$tot_beds), na.rm = TRUE), length.out = 5)),
  #                 sizes = round(seq(0, max(circle_marker_radius_values*2, na.rm = TRUE), length.out = 5)), title = "Number of beds")


######## adding the food banks ######
food_and_shelter_map = shelter_map %>% addMarkers(data = food_banks_spdf, label = lapply(paste(sep ='<br/>',"Food Bank", food_banks_spdf$org, food_banks_spdf$phone), HTML))



######## Adding demographic data from the acs - lb_acs #######

spdf = lb_city
tracts = la_county
shape = lb_city


#given a single row in an spdf, returns spdf row with only the largest polygon (by area)
get_largest_shape = function(spdf, row_id = 1){
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
points_in_shape = function(shape, shape_within, ret_perc = TRUE){
  points = SpatialPoints(shape_within[n,]@polygons[[1]]@Polygons[[1]]@coords, proj4string = shape_within@proj4string)
  if(ret_perc) return(length(which(!is.na(over(points, shape)[,1])))/nrow(points@coords))
  return(length(which(!is.na(over(points, shape)[,1]))))
}
#given all of the census tracts, returns the census tracts that are within the city limits and additional tracts 
tracts_in_shape = function(tracts, shape, contain_threshold = .5){
  require(rgeos)
  contain_tracts = tracts[which(gContains(shape, tracts, byid = TRUE)),]
  if(is.numeric(contain_threshold) & contain_threshold < 1){
    border_tracts = tracts[which(gOverlaps(shape, tracts, byid = TRUE)),]
    keep_tracts = NULL
    for(n in seq_len(nrow(border_tracts))){
      if(points_in_shape(shape, border_tracts, ret_perc = TRUE) > contain_threshold){
        keep_tracts = c(keep_tracts, n)
      }
    }
    contain_tracts = rbind(contain_tracts, border_tracts[keep_tracts,])
  }
  return(contain_tracts)
}




#Identify the census tracts in long beach
library(tigris)
library(rgeos)

CITY_NAME = 'Long Beach'

la_county = tracts('CA', 'Los Angeles', cb = TRUE)
lb_city = places('CA', cb = TRUE)
lb_city = lb_city[lb_city$NAME == CITY_NAME,] %>% get_largest_shape()

lb_tracts = tracts_in_shape(la_county, lb_city, contain_threshold = .95)

acs_2017 = readRDS('data_tables/all_acs_dat_2017.rds')

lb_acs = lb_tracts
lb_acs@data = merge(lb_acs@data, acs_2017, by = 'GEOID')

###### Adding in some cdc data - lb_all #############

cdc_2018 = readRDS('data_tables/cdc_2018.rds')

cdc_merge = cdc_2018[which(cdc_2018$tractfips %in% lb_acs$GEOID),]
cdc_merge_geoid = cdc_merge$tractfips
for(n in seq_len(ncol(cdc_merge))){if(!is.na(as.numeric(cdc_merge[1,n]))) cdc_merge[,n] = as.numeric(cdc_merge[,n])}
cdc_merge = cdc_merge %>% select_if(is.numeric)
cdc_merge$placefips = NULL
cdc_merge$tractfips = NULL
cdc_merge$GEOID = cdc_merge_geoid


tracts_not_in_cdc = lb_acs$GEOID[which(!((lb_acs$GEOID) %in% (cdc_merge$GEOID)))]
for(n in seq_along(tracts_not_in_cdc)){
  add_row = c(rep(NA, (ncol(cdc_merge) - 1)), tracts_not_in_cdc[n])
  cdc_merge = rbind(cdc_merge, add_row)
}

lb_all = lb_acs
lb_all@data = merge(lb_acs@data, cdc_merge, by = 'GEOID')

#making all of the columns that need to be numeric numeric
for(n in data_code_book$var_name){
  lb_all@data[,n] = as.numeric(lb_all@data[,n])
}

#making the education variable negative (from hs graduation rate to rate of not graduating high school)
lb_all@data$DP02_0066P = 100 - lb_all@data$DP02_0066P

########## calculating the score and creating label - lb_all ##########

min_max_vec = function(vec, na.rm = TRUE, ...){
  if(max(vec, na.rm = na.rm, ...) == min(vec, na.rm = na.rm, ...)){
    return(rep(0, length(vec)))
  }
  return((vec - min(vec, na.rm = na.rm, ...))/(max(vec, na.rm = na.rm, ...)-min(vec, na.rm = na.rm, ...)))
}

#given a vector of numeric values, or something that can be coerced to numeric, returns a vector of the percentile each observation is within the vector.
#Example: if vec == c(1,2,3), then get_percentile(vec) == c(0.3333333, 0.6666667, 1.0000000)
get_percentile = function(vec, compare_vec = NULL){
  if(is.null(compare_vec)){
    return(ecdf(vec)(vec))
  }else{
    new_vec = rep(0, length(vec))
    for(n in seq_along(vec)){
      new_vec[n] = ecdf(c(vec[n], compare_vec))(c(vec[n], compare_vec))[1]
    }
    return(new_vec)
  }
}
#given a vector of numeric values, and the number of bins you want to place values into, returns a vector of 'vec' length where each observation is the quantile of that observation.
#Example: if vec == c(1,2,3), then get_quantile(vec, quantile_bins = 2) = as.factor(c(0, 50, 50)). 
#To have the top observation be marked as the 100%ile, set ret_100_ile == TRUE. 
#To return a factor variable, ret_factor == TRUE, otherwise it will return a numeric vector. 
get_quantile = function(vec, quantile_bins, ret_factor = TRUE, ret_100_ile = FALSE, compare_vec = NULL){
  quantile_bins = round(min(max(quantile_bins, 2), 100)) #ensuring the quantile bins is an integer between 2 and 100
  quant_val = floor(get_percentile(vec, compare_vec)*100 / (100/quantile_bins)) * (100/quantile_bins)
  if(!ret_100_ile){ quant_val[quant_val == 100] = unique(quant_val)[order(-unique(quant_val))][2]}
  if(ret_factor){return(factor(quant_val))}
  return(quant_val)
}



data_code_book = read.csv('variable_mapping.csv', stringsAsFactors = FALSE)


#data for homelessness - 
#binge drinking, mental health, low education, households under poverty, unemployment

homeless_risk_vars = c('Rate of binge drinking', 
                       'Mental health diagnoses',
                       'Low education (no high school diploma)',
                       'Unemployment rate', 
                       'Households under poverty line')
homeless_risk_weights = c(1,
                          1,
                          1,
                          1,
                          1)

risk_vars = homeless_risk_vars
risk_weights = homeless_risk_weights
spdf = lb_all
data_code_book = data_code_book

calculate_score = function(risk_vars, risk_weights, spdf, data_code_book){
  
  if(length(risk_vars) != length(risk_weights)){
    warning("risk vars and risk weights are not the same length")
    return(NULL)
  }
  if(!all(risk_vars %in% data_code_book$ï..Variable.name)){
    warning("some var names are not in the codebook")
    return(NULL)
  }
  
  
  
  data_code_book = data_code_book[order(data_code_book$ï..Variable.name),]
  risk_dataset = data.frame(risk_vars, risk_weights, stringsAsFactors = FALSE)[order(risk_vars),]
  risk_dataset$var_code = data_code_book$var_name[data_code_book$ï..Variable.name %in% risk_dataset$risk_vars]
  
  #get the vars from the spdf that are valuable here and order them in the same was as the risk_dataset
  score_vars = spdf@data[,which(colnames(spdf@data) %in% risk_dataset$var_code)]
  score_vars = score_vars[,risk_dataset$var_code]
  
  #standardizing the score_vars between 0 and 1
  for(n in seq_len(ncol(score_vars))){
    score_vars[,n] = min_max_vec(score_vars[,n], na.rm = TRUE)
  }
  
  #Marking any entries with NAs as a large negative number to ensure the resulting value is negative, so we can mark NA scores.
  for(n in seq_len(ncol(score_vars))){
    score_vars[is.na(score_vars[,n]),n] = -10000000
  }
  score_mat = data.matrix(score_vars)
  
  #multiplying those scores by the weights and summing them. works
  score = score_mat %*% risk_dataset$risk_weights
  
  score[score < 0] = NA
  
  return(data.frame(geoid = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE)))
  
}

lb_all@data$score = calculate_score(homeless_risk_vars, homeless_risk_weights,
                                    lb_all, data_code_book)$score

#making the label from this
make_label_for_score = function(risk_vars, spdf, data_code_book, quantile_bins = 10){
  label_list = NULL
  for(row_ind in 1:nrow(spdf@data)){

    label_string = NULL
    for(n in seq_along(risk_vars)){
      label_string = c(label_string, paste0(risk_vars[n], ': ', 
                                            round(spdf@data[row_ind,which(colnames(spdf@data) == data_code_book$var_name[data_code_book$ï..Variable.name == risk_vars[n]])]), '% (', 
                                            get_quantile(spdf@data[,which(colnames(spdf@data) == data_code_book$var_name[data_code_book$ï..Variable.name == risk_vars[n]])], quantile_bins = quantile_bins)[row_ind], '%ile)'))
    }
    full_label = c(paste0("<b>Overall metric: ", get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score), "%ile</b>"), label_string)
    label_list = c(label_list, paste(full_label, collapse = '</br>')) 
  }
  return(label_list)
}

lb_all@data$label = make_label_for_score(homeless_risk_vars, lb_all, data_code_book)



######## Making the map from the score ########

TRACT_PAL = 'Spectral'
tract_color_vals = get_quantile(lb_all@data$score, quantile_bins = 10)

tract_pal = colorFactor(
  palette = TRACT_PAL, 
  domain = tract_color_vals
)

map_all = map %>% addPolygons(data = lb_all, fillColor = ~tract_pal(tract_color_vals), popup = lb_all@data$label, stroke = F,
                              opacity = .8) %>% 
  addCircleMarkers(data = shelter_spdf, radius = circle_marker_radius_values, 
                   label = lapply(shelter_spdf@data$label, HTML), stroke =TRUE, 
                   color = ~map_pal(color_val),
                   opacity = .8) %>% 
  addLegend(position = "topright", pal = map_pal, title = "Bed Utilization Rate",
            values = color_val) %>%
  addMarkers(data = food_banks_spdf, label = lapply(paste(sep ='<br/>',"Food Bank", food_banks_spdf$org, food_banks_spdf$phone), HTML))








