# home page


county_list = toTitleCase(gsub("([^,]+)(,)([[:print:]]+)", "\\3 county, \\1", county.fips$polyname))

test_profile = load_variables(2017, 'acs5/profile')
test_subject = load_variables(2017, 'acs5/subject')
test = load_variables(2017, 'acs5')

# if this is the first time running this on the computer
acs_list = list()
i = 1
for(year in ACS_START_YEAR : ACS_END_YEAR){
  acs_dat <- get_acs(geography = 'tract',
                     variables = c(acs_vars[1,1]),
                     state = '006', #state fips code for CA
                     county = "085", #county fips code for santa clara
                     geometry = FALSE, year = year)
  colnames(acs_dat)[colnames(acs_dat) == 'estimate'] = acs_vars[1,2]
  for(var_i in 2:nrow(acs_vars)){
    var_name = acs_vars[var_i,1]
    new_var_table = get_acs(geography = 'tract',
                            variables = var_name,
                            state = '006',
                            county = '085',
                            geometry = FALSE, year = year)
    new_var_column = new_var_table$estimate
    acs_dat = cbind(acs_dat, new_var = new_var_column)
    colnames(acs_dat)[colnames(acs_dat) == 'new_var'] = acs_vars[var_i, 2]
  } #individually adding each ACS variable since the get_acs function only seems to be able to handle one variable at a time.
  acs_dat$variable = NULL
  acs_dat$moe = NULL
  acs_list[[i]] = list(year, acs_dat)
  i = i + 1
}


#these need to be converted into a list and saved as an rds file to be maintained throughout the journey
location = "New York NY"
acs_vars = c('Poverty level', 'Adults with HS diploma')
year_range = c(2014,2017)

output$pageStub <- renderUI(tagList(
  fluidRow(
    column(5,
           HTML("<p>This is the home page of our excellent web site.</p>",
                "<p>There's a second page that displays data about Old Faithful.",
                "On that page you can move the slider to increase or decrease the",
                "number of bins in the histogram.</p>",
                "<p>The third link goes to a page that doesn't exist to demonstrate",
                "error handling for bad URLs.</p>")
    ),
    column(7, 
           selectizeInput(
             'city', 'Select your city or county (US only)', choices = c(us.cities$name, county_list),
             options = list(
               placeholder = 'Enter City/County name',
               onInitialize = I(paste0('function() { this.setValue("',location,'"); }')),
               maxOptions = 10
             )
           )
           )
  ),
  fluidRow(column(5,
                  sliderInput('year_range', 'Which years should we look at?',
                              2013, 2018, value = year_range)
                  ),
           column(7,
                  checkboxGroupInput(
                    'acs_vars', 'Select the metrics of interest',
                    choices = c(#need to do some research on what variables can be selected. 
                                'Poverty level',
                                'Unemployment',
                                'Adults with HS diploma',
                                'Single parents',
                                'Foreign born',
                                'Recently entered US',
                                'Youth in poverty',
                                'Racial makeup'
                                ),
                    selected = acs_vars
                  )
                  )
           )
))



####### Experimenting with CDC data pulling ###########
#install.packages("RSocrata")
library(RSocrata)
#accesses the 2018 cdc data by census tract. Please note that you can filter the data by a value by 
#adding things like "?stateabbr=CA" after the ".json". any variable can be used to filter. 
#pulls data from these sources:
#2016: https://chronicdata.cdc.gov/500-Cities/500-Cities-City-level-Data-GIS-Friendly-Format-201/k56w-7tny
#2017: https://chronicdata.cdc.gov/500-Cities/500-Cities-City-level-Data-GIS-Friendly-Format-201/djk3-k3zs
#2018: https://chronicdata.cdc.gov/500-Cities/500-Cities-Census-Tract-level-Data-GIS-Friendly-Fo/k86t-wghb
#API urls:
#2016: https://chronicdata.cdc.gov/resource/a3kh-5fhs.json
#2017: https://chronicdata.cdc.gov/resource/kucs-wizg.json
#2018: https://chronicdata.cdc.gov/resource/k86t-wghb.json


# cdc_2016 <- read.socrata(
#   "https://chronicdata.cdc.gov/resource/a3kh-5fhs.json",
#   app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
#   email     = "gehami@alumni.stanford.edu",
#   password  = "albutt69!socrata"
# )
# cdc_2017 <- read.socrata(
#   "https://chronicdata.cdc.gov/resource/kucs-wizg.json",
#   app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
#   email     = "gehami@alumni.stanford.edu",
#   password  = "albutt69!socrata"
# )
# cdc_2018 <- read.socrata(
#   "https://chronicdata.cdc.gov/resource/k86t-wghb.json",
#   app_token = "2TJ9miraJvdQBDfA9fTD4QNZ6",
#   email     = "gehami@alumni.stanford.edu",
#   password  = "albutt69!socrata"
# )





