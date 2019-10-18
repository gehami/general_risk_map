# home page


# county_list = toTitleCase(gsub("([^,]+)(,)([[:print:]]+)", "\\3 county, \\1", county.fips$polyname))



####### Constants #########

VIOLENCE_CHOICES = c(#need to do some research on what variables can be selected. 
  'Rate of binge drinking', #measured by CDC binge drinking reports
  'Mental health diagnoses',
  'Low education (no high school diploma)',
  'Young single parents (<25)',
  'Children in poverty'
  # 'Unemployment rate',
  # 'Households under poverty line'
)
HEALTH_CHOICES = c(
  'Lack health insurance (18-64)',
  'Lack dental visits',
  'Regularly sleep <7 hours',
  'Mental health diagnoses',
  'No physical activity',
  'Obesity rate',
  'High blood pressure',
  'Diabetes',
  'High cholesterol',
  'Asthma',
  'Arthritis',
  'Heart Disease',
  'Cancer'
)
ECONOMIC_CHOICES = c(
  'Unemployment rate',
  'Adults out of labor force',
  'Households under poverty line',
  'Renter households'
)
QOL_CHOICES = c(
  'Speak little to no english',
  'Born outside the US',
  'Moved to county in past year',
  'Public transit to work',
  'Walk to work',
  'Households without a computer',
  'Households without broadband'
)

#Understanding the year range that should be available in the app
#since cdc data only goes back to 2016, we are cutting the year range off at 2016 minimum
YEAR_RANGE = c(2016,2018)


#loading one cdc data to know what cities we have cdc data on
cdc_2018 = readRDS('data_tables/cdc_2018.rds')
cities_cdc = paste0(cdc_2018$placename[!duplicated(cdc_2018$placename)], ' ', cdc_2018$stateabbr[!duplicated(cdc_2018$placename)])


######## custom JS ######
redirect_jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = '?map';});"

###### Pass-through parameters #########

#these need to be converted into a list and saved as an rds file to be maintained throughout the journey
#check if file exists
if(file.exists('inputs_outputs/home_inputs.rds')){
  inputs = readRDS('inputs_outputs/home_inputs.rds')
}else{
  inputs = hash()
  inputs[['cities']] <- ''
  inputs[['year_range']] <- YEAR_RANGE
  inputs[['violence_factors']] <- ''
  inputs[['health_factors']] <- ''
  inputs[['economics_factors']] <- ''
  inputs[['qol_factors']] <- ''
}

location = inputs[['cities']]
violence_risk_factors = inputs[['violence_factors']]
health_risk_factors = inputs[['health_factors']]
economic_factors = inputs[['economics_factors']]
qol_factors = inputs[['qol_factors']]
year_range = inputs[['year_range']]

# print(inputs)

##### UI ##########

output$pageStub <- renderUI(tagList(
  tags$head(tags$script(redirect_jscode)),
  includeCSS('www/sreen_size.css'),
  fluidRow(
    column(5,
           HTML("<p>Welcome to the city risk factors map. To see how to use this tool, click on <a href = 'https://www.google.com'> this link to the tutorial</a></p>",
                "<p>On this page you can select the risk factors you would like to explore.",
                "Risk factors cover topics on community health, economics, potential for community violence, and quality of life.",
                "All metrics are scored from low risk (0%) to high risk (100%), with a high-risk neighborhood displaying the most need for support based on our data.</p>"
                )
    ),
    column(7, 
           selectizeInput(
             'city', 'Select your city (US only)', choices = c(cities_cdc), multiple = TRUE,
             options = list(
               placeholder = 'Enter City name',
               onInitialize = I(paste0('function() { this.setValue("',paste(location, collapse = ','),'"); }')),
               maxOptions = 10
             )
           )
           )
  ),
  fluidRow(column(5),
           column(7,
                  h3('Select the metrics of interest'),

                  div(class = "factor_selector",
                      dropdownButton(
                        checkboxGroupInput(
                          'violence_factors', 'Risk factors for violence/delinqunecy',
                          choices = VIOLENCE_CHOICES, selected = violence_risk_factors
                        ),
                        label = 'Risk factors for violence/delinquency',
                        circle = FALSE
                  )),
                  div(class = "factor_selector", 
                    dropdownButton(
                    checkboxGroupInput(
                      'health_factors', 'Community health factors',
                      choices = HEALTH_CHOICES,
                      selected = health_risk_factors
                    ),
                    label = 'Community health factors',
                    circle = FALSE
                  )),
                  div(class = "factor_selector",
                    dropdownButton(
                    checkboxGroupInput(
                      'economic_factors', 'Economic factors',
                      choices = ECONOMIC_CHOICES,
                      selected = economic_factors
                    ),
                    label = 'Economic factors',
                    circle = FALSE
                  )),
                  div(class = "factor_selector",
                    dropdownButton(
                    checkboxGroupInput(
                      'qol_factors', 'Quality-of-life factors',
                      choices = QOL_CHOICES, 
                      selected = qol_factors
                    ),
                    label = 'Quality-of-life factors',
                    circle = FALSE
                  )),
                  # sliderInput('year_range', 'Which years should we look at?',
                  #             YEAR_RANGE[1], YEAR_RANGE[2], value = year_range),
                  actionBttn('map_it', 'Map it'),
                  uiOutput('input_warning')

                )
           )
))

observeEvent(input$year_range,{
  print(input$year_range)
})

observeEvent(input$map_it,{
  if(is.null(c(input$violence_factors, input$health_factors, input$economic_factors, input$qol_factors))){
    print("no factors present")
    output$input_warning <- renderUI(h5("Please select at least 1 risk factor from the 4 drop-down menus above"))
  }else{
    output$warn = NULL
    inputs = hash()
    inputs[['cities']] <- input$city
    # inputs[['year_range']] <- input$year_range
    inputs[['year_range']] <- YEAR_RANGE
    inputs[['violence_factors']] <- input$violence_factors
    inputs[['health_factors']] <- input$health_factors
    inputs[['economics_factors']] <- input$economic_factors
    inputs[['qol_factors']] <- input$qol_factors
    print(inputs)
    saveRDS(inputs, 'inputs_outputs/home_inputs.rds')
    session$sendCustomMessage("mymessage", "mymessage")
    
  }
})





