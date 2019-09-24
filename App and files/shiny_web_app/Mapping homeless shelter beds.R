######### Libraries #########


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
api <- readLines("apikey.txt")
register_google(key = api)

shelter_dat = read.csv("long-beach_housing_inventory_count.csv", stringsAsFactors = FALSE)
shelters =shelter_dat$Organization.Name[shelter_dat$Organization.Name != '']
shelters = paste0(as.character(shelters), ", Long Beach, CA")
shelters[shelter_dat$Filler_address != ""] = shelter_dat$Filler_address[shelter_dat$Filler_address != ""]

u_shelters = shelters

geocoded <- data.frame(shelter_name = u_shelters, lon = 0, lat = 0, geoaddress = "", stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(geocoded)){
  # Print("Working...")
  worked = FALSE
  n = 1
  while(!worked){
    try({result <- geocode(u_shelters[i], output = "latlona", source = "google")
          worked = TRUE
          })
    n = n+1
    if(n > 100) break
  }
  
  geocoded$lon[i] <- as.numeric(result[1])
  geocoded$lat[i] <- as.numeric(result[2])
  geocoded$geoaddress[i] <- as.character(result[3])
}


##### Putting together the final shelter spatial database - shelter_spdf #########

ma_shelter = cbind(geocoded[,-1], shelter_dat) %>% select(org = Organization.Name, project = Project.Name, proj_type = Proj..Type, target_pop = Target.Pop.,
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

circle_marker_radius_values = log10(as.numeric(shelter_spdf@data$tot_beds)^2)*5


QUANT_BINS = 5
RET_FACTOR = TRUE

map_pal = colorFactor(
  palette = 'Reds',
  domain = get_quantile(as.numeric(shelter_spdf@data$utilization_rate), quantile_bins = QUANT_BINS, ret_factor = RET_FACTOR, ret_100_ile = TRUE)
)

shelter_map <- map %>% addCircleMarkers(data = shelter_spdf, radius = circle_marker_radius_values, 
                                        label = lapply(shelter_spdf@data$label, HTML), stroke =TRUE, 
                                        color = ~map_pal(get_quantile(as.numeric(shelter_spdf$utilization_rate), quantile_bins = QUANT_BINS, ret_factor = RET_FACTOR)),
                                        opacity = .8) %>% 
  addLegend(position = "topright", pal = map_pal, title = "Bed Utilization Rate",
            values = get_quantile(as.numeric(shelter_spdf$utilization_rate), quantile_bins = QUANT_BINS, ret_factor = RET_FACTOR, ret_100_ile = TRUE)) 
  #%>% addLegendCustom(colors = 'grey', labels = round(seq(0, max(as.numeric(shelter_spdf@data$tot_beds), na.rm = TRUE), length.out = 5)),
  #                 sizes = round(seq(0, max(circle_marker_radius_values*2, na.rm = TRUE), length.out = 5)), title = "Number of beds")


