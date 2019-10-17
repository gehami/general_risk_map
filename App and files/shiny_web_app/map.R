#map page

# AVAILABLE_CDC_YEARS = seq()


# if(grepl('shiny_web_app'))
# setwd('shiny_web_app')

#reading in inputs from prior page
inputs = readRDS('inputs_outputs/home_inputs.rds')
print(inputs)


####### Reading in data ########

#reading in the cdc data
if(!exists("cdc_hash")){cdc_hash = hash()}
years = seq(inputs$year_range[1], inputs$year_range[2])
for(year in years){
  if(!(year %in% keys(cdc_hash))){
    cdc_data = readRDS(paste0('data_tables/cdc_', as.character(year), '.rds'))
    colnames(cdc_data)[colnames(cdc_data) == 'tractfips'] = 'GEOID'
    cdc_hash[[as.character(year)]] = cdc_data
  }
}

#reading in the acs data. Note that the year of the data is actually the year before the key 
#(i.e. acs_hash[['2018']] actually stores 2017 acs data), becuase the acs data is one year behind the cdc data. 
if(!exists("acs_hash")){acs_hash = readRDS('data_tables/acs_dat_hash.rds')}

#reading in the spatial data
if(!exists('trimmed_tracts')){trimmed_tracts = readRDS('data_tables/trimmed_tract_data.rds')}

#reading in the tract_city_database
if(!exists('tract_city_dictionary')){tract_city_dictionary = readRDS('data_tables/tract_city_dictionary.rds')}

#reading in codebook to translate the names of the acs vars to their name in the dataset
if(!exists('codebook')){
  codebook = read.csv('variable_mapping.csv', stringsAsFactors = FALSE)
  }

#get just the tracts from the cities that we care about
city_tracts = tract_city_dictionary[inputs$cities] %>% values() %>% unlist() %>% as.character()

#identifying which tracts to use
tracts_map = trimmed_tracts[trimmed_tracts$GEOID %in% city_tracts,]

####### Contstants #########

INITIAL_WEIGHTS = 1
#percent of a variable that is allowed to be NA for me to keep it in the predictors dataset
NA_TOL = .1
QUANTILE_BINS = 10
# 1/x_ij where x is number of blocks between block i and j (starting at 1), 0 if more than MAX_BLOCK_DIST away
MAX_LOC_DIST = 1 #looking at neighbords directly next to tract

TRACT_PAL = 'RdYlGn'
TRACT_OPACITY = .7


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

#given a vector, min-max-scales it between 0 and 1
min_max_vec = function(vec, na.rm = TRUE, ...){
  if(max(vec, na.rm = na.rm, ...) == min(vec, na.rm = na.rm, ...)){
    return(rep(0, length(vec)))
  }
  return((vec - min(vec, na.rm = na.rm, ...))/(max(vec, na.rm = na.rm, ...)-min(vec, na.rm = na.rm, ...)))
}


#like the %in% function, but retains the order of the vector that your checking to see if the other vector is in it.
#EX: if x <- c(1,4,6) and y <- c(6,1,4), then in_match_order(x, y) = c(3,1,2) since 6 appears at x_ind 3, 1 appears at x_ind 1, and 4 appears at x_ind 2
in_match_order = function(vec_in, vec){
  ret_inds = NULL
  for(n in vec){
    ret_inds = c(ret_inds,
                 try(which(n == vec_in)[1])
    )
  }
  return(ret_inds[!is.na(ret_inds)])
}





######## Createing the initial map #########
#Given therisk vars, risk weights, spdf, and codebook, calculates the overall risk factor score
calculate_score = function(risk_vars, risk_weights, spdf, data_code_book){
  
  if(length(risk_vars) != length(risk_weights)){
    warning("risk vars and risk weights are not the same length")
    return(NULL)
  }
  if(!all(risk_vars %in% data_code_book$risk_factor_name)){
    warning("some var names are not in the codebook")
    return(NULL)
  }
  
  
  data_code_book = data_code_book[order(data_code_book$Name),]
  risk_dataset = data.frame(risk_vars, risk_weights, stringsAsFactors = FALSE)[order(risk_vars),]
  risk_dataset$var_code = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, risk_dataset$risk_vars)]
  
  #get the vars from the spdf that are valuable here and order them in the same was as the risk_dataset
  score_vars = tryCatch(spdf@data[,which(colnames(spdf@data) %in% risk_dataset$var_code)], 
                        error = function(e) spdf[,which(colnames(spdf) %in% risk_dataset$var_code)])
  score_vars = score_vars[,risk_dataset$var_code]
  
  #converting each column in score_vars to numeric
  for(n in seq_len(ncol(score_vars))){
    score_vars[,n] = as.numeric(score_vars[,n])
  }
  
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
  if(is.null(spdf$GEOID)) return(data.frame(score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  return(data.frame(GEOID = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE), stringsAsFactors = FALSE))
  
}
#making the label for the map from the risk_vars, spdf, codebook, and quantile bins
make_label_for_score = function(risk_vars, spdf, data_code_book, quantile_bins = 10){
  label_list = NULL
  #cleaning up the risk_var names
  risk_var_cats = unique(gsub('([[:alpha:]]+)(_[[:print:]]*)', '\\1', names(risk_vars)))
  risk_var_cats_name_conversion = data.frame(cats = risk_var_cats, display_names = paste0(tools::toTitleCase(risk_var_cats), ' factors'), stringsAsFactors = FALSE)
  risk_var_cats_name_conversion$display_names[risk_var_cats_name_conversion$cats == 'qol'] = "Quality of life factors"
  
  if(length(risk_var_cats) > 1){
    risk_cats_scores = data.frame(array(dim = c(nrow(spdf@data), length(risk_var_cats))))
    colnames(risk_cats_scores) = risk_var_cats
    for(risk_cat in risk_var_cats){
      interest_vars = risk_vars[grep(risk_cat, names(risk_vars))]
      interest_var_names = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, interest_vars)] #works
      min_max_vars = data.frame(spdf@data[,interest_var_names])
      for(n in seq_len(ncol(min_max_vars))){
        min_max_vars[,n] = min_max_vec(min_max_vars[,n], na.rm = TRUE)
      }
      risk_cats_scores[,risk_cat] = rowSums(min_max_vars)
    }
    risk_cats_quantiles = risk_cats_scores
    for(n in seq_len(ncol(risk_cats_quantiles))){
      risk_cats_quantiles[,n] = get_quantile(risk_cats_scores[,n], quantile_bins = quantile_bins)
    }
  }
  
  for(row_ind in 1:nrow(spdf@data)){
    
    label_string = NULL
    if(length(risk_var_cats) < 2){
      for(n in seq_along(risk_vars)){
        label_string = c(label_string, paste0(risk_vars[n], ': ', 
                                              round(spdf@data[row_ind,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]]), '% (', 
                                              get_quantile(spdf@data[,data_code_book$Name[data_code_book$risk_factor_name == risk_vars[n]]], quantile_bins = quantile_bins)[row_ind], '%ile)'))
      }
      full_label = c(paste0("<b>Overall ", risk_var_cats_name_conversion$display_names[1], " metric: ", get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score), "%ile</b>"), label_string)
      label_list = c(label_list, paste(full_label, collapse = '</br>')) 
      
    }else{
      for(risk_cat in risk_var_cats){
        # risk_cat = risk_var_cats[1]
        cat_score = risk_cats_quantiles[row_ind,risk_cat]
        label_string = c(label_string, paste0('<i>', risk_var_cats_name_conversion$display_names[risk_var_cats_name_conversion$cats == risk_cat],
                                              ': ', as.character(cat_score), '%ile</i>'))
        
        interest_vars = risk_vars[grep(risk_cat, names(risk_vars))]
        interest_var_names = data_code_book$Name[in_match_order(data_code_book$risk_factor_name, interest_vars)] #works
        for(sub_vars_ind in seq_len(length(interest_vars))){
          label_string = c(label_string, 
                           paste0('<small>', interest_vars[sub_vars_ind], ': ', 
                                  round(spdf@data[row_ind,interest_var_names[sub_vars_ind]]), '% (', 
                                  get_quantile(spdf@data[,interest_var_names[sub_vars_ind]], quantile_bins = quantile_bins)[row_ind], '%ile)', '</small>')
          )
        }
      }
      full_label = c(paste0("<b>Overall risk metric: ", 
                            get_quantile(spdf@data$score[row_ind], quantile_bins = quantile_bins, compare_vec = spdf@data$score), "%ile</b>"), label_string)
      label_list = c(label_list, paste(full_label, collapse = '</br>'))
    }
  }
  return(label_list)
}
#given an spdf, codebook, risk_vars, risk_weights, and quantiles, returns the updated spdf with the metric score and full label as new vars
make_full_spdf = function(spdf, data_code_book, risk_vars, risk_weights, quantile_bins){
  #making sure the variables of interest are numeric
  for(n in risk_vars){
    spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]] = 
      as.numeric(spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]])
  }
  
  spdf@data = merge(spdf@data, calculate_score(risk_vars, risk_weights, spdf, data_code_book), by = 'GEOID')
  
  spdf@data$label = make_label_for_score(risk_vars, spdf, codebook, quantile_bins)
  
  return(spdf)
}


#given the spdf, returns the loc_dist_matrix
get_loc_dist_matrix = function(spdf, MAX_LOC_DIST = 1){
  #initializing the matrix
  loc_dist_matrix = matrix(0, nrow = nrow(spdf@data), ncol = nrow(spdf@data))
  loc_matrix = rgeos::gTouches(spdf, byid = TRUE)
  
  
  #iterates through all blocks of 1 - MAX_BLOCK_DIST away, identifies which iteration it was picked up, and marks that number into matrix
  #this will likely take hours (lol, takes 1 second).
  for(loc_it_count in 1 : ncol(loc_dist_matrix)){
    layer_locs = loc_it_count
    marked_locs = loc_it_count
    for(its in 1  : MAX_LOC_DIST){
      if(length(layer_locs) > 1){
        layer_locs_vec = which(rowSums(loc_matrix[,layer_locs])>0)
        
      }else{
        layer_locs_vec = which(loc_matrix[,layer_locs])
      }
      layer_locs_vec = layer_locs_vec[which(!(layer_locs_vec %in% marked_locs))]
      loc_dist_matrix[layer_locs_vec,loc_it_count] = its
      layer_locs = layer_locs_vec
      marked_locs = c(marked_locs, layer_locs)
    }
    if(loc_it_count %% 50 == 0) print(loc_it_count)
  }
  
  colnames(loc_dist_matrix) = spdf@data$GEOID
  rownames(loc_dist_matrix) = spdf@data$GEOID
  
  loc_dist_matrix = 1/loc_dist_matrix
  loc_dist_matrix[loc_dist_matrix > 1] = 0
  
  return(loc_dist_matrix)
  
}
#given a vector of length n and the n by n neighbor matrix, returns a vector of n length of the averaged value for each GEOID's neibs on that var
get_neib_average_vec = function(vec, loc_dist_matrix, na_neibs_count = 0){
  return((vec %*% loc_dist_matrix)/(rowSums(loc_dist_matrix) - na_neibs_count)) 
}#checked and works
#given a vec of n values and a n by n neighbor matrix, returns the number of neighbors with an NA value for each row in the vec
count_na_neibs = function(vec, loc_dist_matrix){
  vec[!is.na(vec)] = 0
  vec[is.na(vec)] = 1
  na_neibs_count = vec %*% loc_dist_matrix
  return(na_neibs_count)
}#works
#given a vector of cn length (where c is an integer) and the n by n neighbor matrix, returns a vector of cn length of the averaged value for each row's neibs on that var
get_full_neib_average_vec = function(vec, loc_dist_matrix, na.rm = TRUE){
  ret_vec = rep(0, length(vec))
  next_start_vec_ind = 1
  n = nrow(loc_dist_matrix)
  
  if(na.rm){
    na_neibs_count = count_na_neibs(vec, loc_dist_matrix)
    vec[is.na(vec)] = 0
  } 
  
  for(c in seq_len(length(vec)/n)){
    focus_inds = next_start_vec_ind:(next_start_vec_ind+n-1)
    ret_vec[focus_inds] = get_neib_average_vec(vec[focus_inds], loc_dist_matrix, na_neibs_count)
    next_start_vec_ind = next_start_vec_ind + n
  }
  return(ret_vec)
} #checked and works (loose checking, but yeah seems to work)
#given the x_vars, ids, and the loc_dist_matrix, returns the table of calculated weighted average negihbor score for each x_var
neib_avg_scores = function(x_vars, ids, loc_dist_matrix, na.rm = TRUE){
  neib_matrix = data.frame(array(0, dim = c(nrow(x_vars), (length(x_vars) + 1))), stringsAsFactors = FALSE)
  colnames(neib_matrix) = c('GEOID', colnames(x_vars))
  neib_matrix$GEOID = ids
  loc_dist_matrix = loc_dist_matrix[rownames(loc_dist_matrix) %in% neib_matrix$GEOID, colnames(loc_dist_matrix) %in% neib_matrix$GEOID]
  for(x_var in colnames(x_vars)){
    neib_matrix[,x_var] = get_full_neib_average_vec(x_vars[,x_var], loc_dist_matrix, na.rm)
  }
  colnames(neib_matrix)[2:ncol(neib_matrix)] = paste0('neib_avg_', colnames(x_vars))
  
  if(!identical(neib_matrix$GEOID, ids)) message('not identical ids, something is wrong')
  
  return(neib_matrix)
  
}
#given the spdf, risk_vars, and the codebook, returns the independent vars for predicting
get_ind_vars_for_model = function(spdf, risk_vars, data_code_book){
  x_vars = spdf@data[data_code_book$Name[data_code_book$risk_factor_name %in% risk_vars]]
  #making sure all of the columns are numeric
  for(n in seq_len(ncol(x_vars))) x_vars[,n] = as.numeric(x_vars[,n])
  
  loc_dist_matrix = get_loc_dist_matrix(spdf, MAX_LOC_DIST)
  ids = spdf@data$GEOID
  ind_vars = data.frame(GEOID = ids, x_vars, stringsAsFactors = FALSE) #all the ind vars and GEOID id tag
  
  neib_matrix = neib_avg_scores(x_vars, ids, loc_dist_matrix, na.rm = TRUE)
  
  big_ind_dat = merge(ind_vars, neib_matrix, by = 'GEOID')
  
  return(big_ind_dat)
}
#given the full spdf hash, inputs list, risk_vars, and codebook, returns the 1) raw predicted scores, 2) pred score quantiles, and 3) labels for the pred map
get_predicted_scores_and_labels = function(city_all_spdf_hash, inputs, risk_vars, data_code_book, quantile_bins = 10){
  
  ind_vars = get_ind_vars_for_model(city_all_spdf_hash[[as.character(inputs$year_range[1])]], risk_vars, data_code_book)
  dep_dat = calculate_score(risk_vars, risk_weights, city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book)
  
  
  #min_max_scaling vars
  ind_vars[,2:ncol(ind_vars)] = sapply(ind_vars[,2:ncol(ind_vars)], min_max_vec)
  dep_dat[,2] = min_max_vec(vec = dep_dat[,2])
  
  if(identical(ind_vars[,1], dep_dat[,1])){
    model_dat = data.frame(score = dep_dat[,2], ind_vars[,-1])
  }else{warning("ids don't match up between dep_dat and ind_vars")}
  
  score.lm = lm(score ~ ., data = model_dat)
  
  predict_dat = get_ind_vars_for_model(city_all_spdf_hash[[as.character(inputs$year_range[2])]],
                                       risk_vars, data_code_book)[,-1] 
  predict_dat[,1:ncol(predict_dat)] = sapply(predict_dat[,1:ncol(predict_dat)], min_max_vec)
  
  pred_score = predict(score.lm, newdata = predict_dat)
  last_real_score = dep_dat[,2]
  
  #to have the overall risk metrics match up with the existing year's risk metrics, we scale the numbers such that they, on average, match up to the existing overall metrics
  division_factor = sum(pred_score, na.rm = TRUE)/sum(last_real_score, na.rm = TRUE)
  pred_score_fixed = pred_score/division_factor
  pred_score_fixed[pred_score_fixed < 0] = 0
  
  pred_score_quantile = get_quantile(pred_score_fixed, quantile_bins = quantile_bins)
  
  pred_score_label = paste0("Predicted overall risk metric 2020: <br/><b>", pred_score_quantile, "%ile</b>")
  
  #checking the absolute error rate. since the scores are between 0 and 1, this shows the %error of the scores
  print(summary(abs(last_real_score[!is.na(last_real_score)] - predict(score.lm, newdata = ind_vars[!is.na(last_real_score),]))))
  
  return(list(raw_score = pred_score_fixed, score_quantile = pred_score_quantile, label = pred_score_label))
}
#given present_spdf with future predictions & labels, past_spdf, inputs, pallette info for tracts, and quantile bins, returns a leaflet map
make_map = function(present_spdf, past_spdf, inputs, TRACT_PAL = 'RdYlGn', TRACT_OPACITY = 0.7, quantile_bins = 10){
  lon_med = mean(present_spdf@bbox[1,])
  lat_med = mean(present_spdf@bbox[2,])
  
  map <- leaflet(options = leafletOptions(minZoom = 10, zoomControl = FALSE)) %>% 
    # add ocean basemap
    # addProviderTiles(providers$Esri.OceanBasemap) %>%
    # add another layer with place names
    addProviderTiles(providers$Hydda.Full) %>%
    # focus map in a certain area / zoom level
    setView(lng = lon_med, lat = lat_med, zoom = 12) 
  
  TRACT_PAL = 'RdYlGn'
  TRACT_OPACITY = .7
  tract_color_vals = get_quantile(present_spdf@data$score, quantile_bins = quantile_bins)
  past_tract_color_vals = get_quantile(past_spdf@data$score, quantile_bins = quantile_bins)
  future_tract_color_vals = get_quantile(present_spdf@data$pred_score, quantile_bins = quantile_bins)
  
  tract_pal = colorFactor(
    palette = TRACT_PAL, 
    domain = tract_color_vals,
    reverse = TRUE
  )
  
  u_tract_color_vals = unique(tract_color_vals[!is.na(tract_color_vals)])
  legend_val = u_tract_color_vals[order(u_tract_color_vals)][c(1,length(u_tract_color_vals))]
  
  map_all = map %>% addMarkers(group = 'Clear', lng = 10, lat = 10) %>% 
    addMapPane('risk_tiles', zIndex = 410) %>%
    addPolygons(data = present_spdf, fillColor = ~tract_pal(tract_color_vals), popup = present_spdf@data$label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[2]),options = pathOptions(pane = "risk_tiles")) %>% 
    addPolygons(data = past_spdf, fillColor = ~tract_pal(past_tract_color_vals), popup = past_spdf@data$label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[1]), options = pathOptions(pane = "risk_tiles")) %>% 
    addPolygons(data = present_spdf, fillColor = ~tract_pal(future_tract_color_vals), popup = present_spdf@data$pred_label, stroke = T,
                fillOpacity = TRACT_OPACITY, , weight = 1, opacity = 1, color = 'white', dashArray = '3',
                highlightOptions = highlightOptions(color = 'white', weight = 2,
                                                    bringToFront = FALSE, dashArray = FALSE),
                group = as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])), options = pathOptions(pane = "risk_tiles")) %>% 
    addLegend(colors = tract_pal(legend_val[length(legend_val):1]), opacity = 0.7, position = 'bottomright',
              title = 'Risk factors level', labels = c('High (90%ile)', 'Low (10%ile)')) %>%
    addLayersControl(baseGroups = c('Clear', as.character(inputs$year_range[1]), as.character(inputs$year_range[2]), 
                                    as.character(inputs$year_range[2] + (inputs$year_range[2] - inputs$year_range[1])))) %>%
    showGroup(as.character(inputs$year_range[2])) %>% hideGroup('Clear')
  return(map_all)
}


city_all_dat_hash = hash::hash() 
for(year in inputs$year_range[1]:inputs$year_range[2]){
  acs_year = acs_hash[[as.character(year)]]
  acs_year = acs_year[acs_year$GEOID %in% city_tracts,]
  cdc_year = cdc_hash[[as.character(year)]]
  cdc_year = cdc_year[cdc_year$GEOID %in% city_tracts,]
  city_all_dat_hash[[as.character(year)]] = merge(cdc_year, acs_year, by = 'GEOID')
}

city_all_spdf_hash = hash::hash()
for(year in inputs$year_range[1]:inputs$year_range[2]){
  city_data = merge(tracts_map@data, city_all_dat_hash[[as.character(year)]], by = 'GEOID')
  city_spdf = tracts_map[tracts_map$GEOID %in% city_data$GEOID,]
  city_spdf = city_spdf[order(city_spdf$GEOID),]
  city_data = city_data[order(city_data$GEOID),]
  city_spdf@data = city_data
  city_all_spdf_hash[[as.character(year)]] = city_spdf
}

#in order - violence, health, economic, qol
data_factors = inputs[c('violence_factors', 'health_factors', 'economics_factors', 'qol_factors')] %>% values() %>% unlist()

#creating the scores
risk_vars = data_factors
risk_weights = rep(INITIAL_WEIGHTS, length(risk_vars))
spdf = city_all_spdf_hash[['2018']]
data_code_book = codebook[!duplicated(codebook$risk_factor_name),]
quantile_bins = QUANTILE_BINS




past_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[1])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS)
present_spdf = make_full_spdf(city_all_spdf_hash[[as.character(inputs$year_range[2])]], data_code_book, risk_vars, risk_weights, QUANTILE_BINS)
pred_list = get_predicted_scores_and_labels(city_all_spdf_hash, inputs, risk_vars, data_code_book, QUANTILE_BINS)
present_spdf@data$pred_score = pred_list$raw_score
present_spdf@data$pred_quantile = pred_list$score_quantile
present_spdf@data$pred_label = pred_list$label

initial_map = make_map(present_spdf, past_spdf, inputs, TRACT_PAL, TRACT_OPACITY, QUANTILE_BINS)

output$pageStub <- renderUI(tagList(
  fluidRow(
    column(5,
           HTML("<p>This is the home page of our excellent web site.</p>",
                "<p>There's a second page that displays data about Old Faithful.",
                "On that page you can move the slider to increase or decrease the",
                "number of bins in the histogram.</p>",
                "<p>The third link goes to a page that doesn't exist to demonstrate",
                "error handling for bad URLs.</p>")
    ),column(7,
             leaflet::leafletOutput('map', height = 500)
             )
  )
)
)

output$map <- renderLeaflet(initial_map)





