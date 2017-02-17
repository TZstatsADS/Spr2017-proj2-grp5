# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
shinyServer(function(input, output) {
  
  observe({
    
    output$barPlot <- renderPlot({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      df <- as.data.frame(table(selected$recode))
      ggplot(data=df,aes(reorder(Var1,Freq),Freq,fill=Freq))+geom_bar(stat="identity",fill='#9CCC65')+coord_flip()
      })
    
    output$barPlot1 <- renderPlotly({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      selected<-selected[!is.na(selected$recode),]
      selected_vio <- selected[(selected$recode==input$vio_type),]
      if(nrow(selected_vio)==0){plot_ly()}else{
      selected_vio$one <-rep(1,nrow(selected_vio))
      group<-c("VIOLATION.DESCRIPTION","season")
      dat<-c("one")
      df_1 <- ddply(selected_vio,group,function(x)colSums(x[dat]))
      df1<- as.data.frame(table(selected_vio$VIOLATION.DESCRIPTION))
      df1 <-df1[df1$Freq!=0,]
      df0<- unique(selected_vio[,c("VIOLATION.DESCRIPTION","vio_code2")])
      df2<- as.data.frame(merge(df0,df_1,by.x = "VIOLATION.DESCRIPTION",by.y = "VIOLATION.DESCRIPTION",all.y = TRUE))
      df2<-reshape(df2, idvar = c("VIOLATION.DESCRIPTION","vio_code2"), timevar = "season", direction = "wide")
      df2[is.na(df2)] <- 0
      if(ncol(df2)!=6){
        new.var<-c("one.1","one.2","one.3","one.4")[!(c("one.1","one.2","one.3","one.4")%in%colnames(df2)[-c(1,2)])]
        for (i in 1:length(new.var)){
          df2<-data.frame(df2,rep(0,nrow(df2)))
        }
        
        colnames(df2)[(ncol(df2)+1-length(new.var)):ncol(df2)]<-new.var}
      df2$Freq <-rowSums(df2[,-c(1:2)])
      plot_ly(df2, x = ~reorder(df2$vio_code2,Freq), y = ~one.1,type = 'bar', name="spring",marker = list(color = 'rgb(159,168,213)'))%>%
        add_trace(y = ~one.2,name = 'summer',marker = list(color = 'rgb(153,204,204)'))%>%
        
        add_trace(y = ~one.3, name = 'fall',marker = list(color = 'rgb(178,235,242)'))%>%
        add_trace(y = ~one.4, name = 'winter',marker = list(color = 'rgb(165,214,167)'))%>%layout(yaxis = list(title = 'Count'), barmode = 'stack')}
      
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
  observe({
    # outputdata <- function(cuisine=tb$Var1,zipcode=unique(geo_1617$ZIPCODE)){
    #   selectData1 <- geo_1617[(geo_1617$CUISINE.DESCRIPTION%in%input$cuisine)&(geo_1617$ZIPCODE%in%input$zipcode),]
    #   selectData2<-selectData1[with(selectData1,order(address,INSPECTION.DATE)),]
    #   selectData3 <- aggregate(selectData2,by=list(selectData2$DBA),FUN = last)
    #   return(selectData3)
    # }
    # data <- outputdata(input$cuisine,input$zipcode)
    output$map <- renderLeaflet({
      data<-outputdata(input$cuisine,input$zipcode)
      
      leaflet(data = data) %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>% addCircles(data$lon,data$lat,radius=50,popup=paste(data$DBA,br(),data$address),color = (data$GRADE1))%>%
        setView(lng = -74.00301, lat = 40.72545, zoom = 11)})
  })
  })

