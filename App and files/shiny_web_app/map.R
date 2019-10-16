#map page

# AVAILABLE_CDC_YEARS = seq()


# if(grepl('shiny_web_app'))
setwd('shiny_web_app')
#parse_data_requests = function(inputs, )

#reading in inputs from prior page
inputs = readRDS('inputs_outputs/home_inputs.rds')
print(inputs)

#reading in the cdc data
if(!exists("cdc_hash")){cdc_hash = hash()}
years = seq(inputs$year_range[1], inputs$year_range[2])
for(year in years){
  if(!(year %in% keys(cdc_hash))){
    cdc_hash[[as.character(year)]] = readRDS(paste0('data_tables/cdc_', as.character(year), '.rds'))
  }
}

#reading in the acs data. Note that the year of the data is actually the year before the key 
#(i.e. acs_hash[['2018']] actually stores 2017 acs data), becuase the acs data is one year behind the cdc data. 
if(!exists("acs_hash")){acs_hash = hash()}
years = seq(inputs$year_range[1], inputs$year_range[2])
for(year in years){
  if(!(year %in% keys(acs_hash))){
    acs_hash[[as.character(year)]] = readRDS(paste0('data_tables/all_acs_dat_', as.character(year-1), '.rds'))
  }
}

#reading in codebook to translate the names of the acs vars to their name in the dataset


#identifying which tracts to pull and 
require(tigris)
all_tracts_map = tigris::tracts(state = 'CA')


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





