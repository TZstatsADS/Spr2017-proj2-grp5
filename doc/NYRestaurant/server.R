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
library(ggplot2)
library(plotly)
shinyServer(function(input, output) {
    
    output$barPlot <- renderPlot({
        
        selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
        df <- as.data.frame(table(selected$recode))
        ggplot(data=df, aes(reorder(Var1,Freq),Freq)) +geom_bar(stat="identity")+
          coord_flip()
        
        
        
    })
    output$barPlot1 <- renderPlot({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      selected_vio <- selected[(selected$recode==input$vio_type),]
      df1<- as.data.frame(table(selected_vio$VIOLATION.DESCRIPTION))
      df1 <-df1[df1$Freq!=0,]
      x <- c(1:nrow(df1))
      plot_ly(df1, x = ~reorder(x,Freq), y = ~Freq, type = 'bar', hoverinfo="text",text = as.vector(df1$Var1),
                   marker = list(color = 'rgb(158,202,225)',
                                 line = list(color = 'rgb(8,48,107)',
                                             width = 1))) %>%
        layout(title = input$vio_type,
               xaxis = list(title = ""),
               yaxis = list(title = ""))
      
      
    })
    
})

