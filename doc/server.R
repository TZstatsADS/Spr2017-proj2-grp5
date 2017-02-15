# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(ggplot2)
library(plotly)


shinyServer(function(input, output) {
  
  observe({
    
    output$barPlot <- renderPlot({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      df <- as.data.frame(table(selected$recode))
      ggplot(data=df,aes(reorder(Var1,Freq),Freq))+geom_bar(stat="identity")+coord_flip()
      })
    
    output$barPlot1 <- renderPlotly({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      selected<-selected[!is.na(selected$recode),]
      selected_vio <- selected[(selected$recode==input$vio_type),]
      df1<- as.data.frame(table(selected_vio$VIOLATION.DESCRIPTION))
      df1 <-df1[df1$Freq!=0,]
      df0<- unique(selected_vio[,c("VIOLATION.DESCRIPTION","vio_code2")])
      df2<- as.data.frame(merge(df0,df1,by.x = "VIOLATION.DESCRIPTION",by.y = "Var1",all.y = TRUE))
    
      plot_ly(df2, x = ~reorder(df2$vio_code2,Freq), y = ~Freq, type = 'bar',
              marker = list(color = 'rgb(158,202,225)',
                            line = list(color = 'rgb(8,148,47)',
                                        width = 1)))%>% 
       
      layout(
             xaxis = list(title = ""),
             yaxis = list(title = ""))
      
    })
    
    output$table <- renderDataTable({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      selected_vio <- selected[(selected$recode==input$vio_type),]
      df1<- as.data.frame(table(selected_vio$VIOLATION.DESCRIPTION))
      df1 <-df1[df1$Freq!=0,]
      df0<- unique(selected_vio[,c("VIOLATION.DESCRIPTION","vio_code2")])
      
      df2<- as.data.frame(merge(df0,df1,by.x = "VIOLATION.DESCRIPTION",by.y = "Var1",all.y = TRUE))
      df2<-df2[order(-df2$Freq),-3]
      df2$VIOLATION.DESCRIPTION<-gsub("\032","",df2$VIOLATION.DESCRIPTION,fixed = TRUE)
      df2
      })
    
  })
  })

