######## Libraries #########

library(tidycensus)
library(tidyverse)
library(purrr)


######## Constants #########

ACS_YEAR = 2016

###### pulling the vars that are needed ##########

# test_profile = load_variables(ACS_YEAR, 'acs5/profile')
# test_subject = load_variables(ACS_YEAR, 'acs5/subject')
# test = load_variables(ACS_YEAR, 'acs5')


vars_needed = read.csv('variable_mapping.csv', stringsAsFactors = FALSE, header = TRUE)
acs_vars_needed = vars_needed[vars_needed$Dataset == 'ACS',]

acs_codes = unique(acs_vars_needed$var_name)

###### seting up the census api #########

census_key = readLines('C:\\Users\\gehami\\Documents\\API Keys\\census_api_key.txt')
census_api_key(key = census_key)


##### downloading the data ##########

#shout out to Kyle walker for this code snippet on grabbing every state's census tracts
#https://walkerke.github.io/2017/05/tidycensus-every-tract/
us <- unique(fips_codes$state)[1:51]

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
