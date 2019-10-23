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

shinyApp(ui, server)