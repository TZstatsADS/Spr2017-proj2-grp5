#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
library(shiny)
shinyServer(function(input, output) {
    
    output$barPlot <- renderPlot({
        
        selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
        ggplot(data=selected, aes(x=vio_code2)) +geom_bar(stat="count")
        
        
        
    })
    
})

