# home page


county_list = toTitleCase(gsub("([^,]+)(,)([[:print:]]+)", "\\3 county, \\1", county.fips$polyname))

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
