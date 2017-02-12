#server
library(shiny)

source("rstrtvis.R")

shinyServer(
  function(input, output) {
    
    output$plot <- renderPlot({
      
      
      rstrtvis(input$cuisine)
    })
    
    
  }
)
