#map page

# AVAILABLE_CDC_YEARS = seq()


# if(grepl('shiny_web_app'))
setwd('shiny_web_app')

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
  if(is.null(spdf$GEOID)) return(data.frame(score = min_max_vec(score, na.rm = TRUE)))
  return(data.frame(GEOID = spdf$GEOID, score = min_max_vec(score, na.rm = TRUE)))
  
}

spdf@data = merge(spdf@data, calculate_score(risk_vars, risk_weights, spdf, data_code_book), by = 'GEOID')

#making sure the variables of interest are numeric
for(n in risk_vars){
  spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]] = 
    as.numeric(  spdf@data[,colnames(spdf@data) == data_code_book$Name[data_code_book$risk_factor_name == n]])
}


#making the label from this
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


output$pageStub <- renderUI(tagList(
  fluidRow(
    column(5,
           HTML("<p>This is the home page of our excellent web site.</p>",
                "<p>There's a second page that displays data about Old Faithful.",
                "On that page you can move the slider to increase or decrease the",
                "number of bins in the histogram.</p>",
                "<p>The third link goes to a page that doesn't exist to demonstrate",
                "error handling for bad URLs.</p>")
    )
  )
)
)





