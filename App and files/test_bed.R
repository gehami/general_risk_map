library(leaflet)
library(shiny)

ui <- fluidPage(
  tags$div(id = "garbage"),  # Copy this disposal-div
  leafletOutput("map"),
  div(id = "Showcase")
)

server <- function(input, output, session) {
  
  # --- Just for Show ---
  
  output$popup1 <- renderUI({
    actionButton("Go1", "Go1")
  })
  
  observeEvent(input$Go1, {
    insertUI("#Showcase", where = "beforeEnd",
             div("Button 1 is fully reactive."))
  })
  
  output$popup2 <- renderUI({
    actionButton("Go2", "Go2")
  })
  
  observeEvent(input$Go2, {
    insertUI("#Showcase", where = "beforeEnd", div("Button 2 is fully reactive."))
  })
  
  output$popup3 <- renderUI({
    actionButton("Go3", "Go3")
  })
  
  observeEvent(input$Go3, {
    insertUI("#Showcase", where = "beforeEnd", div("Button 3 is fully reactive."))
  })
  
  # --- End: Just for show ---
  
  # popupMaker is just to lighten code. But here you can see how to insert the popup.
  popupMaker <- function(id) {
    as.character(uiOutput(id))
  }
  
  output$map <- renderLeaflet({
    input$aaa
    leaflet() %>%
      addTiles() %>%
      addMarkers(lat = c(10, 20, 30),
                 lng = c(10, 20, 30),
                 popup = lapply(paste0("popup", 1:3), popupMaker)) %>%
      
      # Copy this part - it initializes the popups after the map is initialized
      htmlwidgets::onRender(
        'function(el, x) {
        var target = document.querySelector(".leaflet-popup-pane");
        
        var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
        if(mutation.addedNodes.length > 0){
        Shiny.bindAll(".leaflet-popup-content");
        }
        if(mutation.removedNodes.length > 0){
        var popupNode = mutation.removedNodes[0];
        
        var garbageCan = document.getElementById("garbage");
        garbageCan.appendChild(popupNode);
        
        Shiny.unbindAll("#garbage");
        garbageCan.innerHTML = "";
        }
        }); 
        });
        
        var config = {childList: true};
        
        observer.observe(target, config);
  }')
  })
}

library("shiny")
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("pickerInput in dropdown"),
  br(),
  dropdown(
    
    tags$h3("List of Input"),
    
    pickerInput(inputId = 'xcol2',
                label = 'X Variable',
                choices = names(iris),
                options = list(`style` = "btn-info")),
    
    pickerInput(inputId = 'ycol2',
                label = 'Y Variable',
                choices = names(iris),
                selected = names(iris)[[2]],
                options = list(`style` = "btn-warning")),
    
    sliderInput(inputId = 'clusters2',
                label = 'Cluster count',
                value = 3,
                min = 1, max = 9),
    
    style = "unite", icon = icon("gear"),
    status = "danger", width = "300px",
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    )
  ),
  
  plotOutput(outputId = 'plot2')
)

server <- function(input, output, session) {
  
  selectedData2 <- reactive({
    iris[, c(input$xcol2, input$ycol2)]
  })
  
  clusters2 <- reactive({
    kmeans(selectedData2(), input$clusters2)
  })
  
  output$plot2 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A",
              "#984EA3", "#FF7F00", "#FFFF33",
              "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData2(),
         col = clusters2()$cluster,
         pch = 20, cex = 3)
    points(clusters2()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)





shinyApp(ui, server)