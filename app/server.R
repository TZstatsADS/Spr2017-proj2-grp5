library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(DT)
library(dplyr)
library(ggmap)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)

tb <- readRDS("tb.Rdata")
tb1 <- readRDS("tb1.Rdata")
tb_vio <-readRDS("tb_vio.Rdata")
orig_1617 <- readRDS("orig_1617.Rdata")
geo_1617 <- readRDS("geo_1617.Rdata")
geo_16 <- readRDS("geo_16.Rdata")




#function for geomap
zip_upmht <- c(10021,10028,10044,10065,10075,10128,10023:10025)
outputdata <- function(cuisine,zipcode=zip_upmht){
  if(is.null(cuisine)){cuisine=geo_16$CUISINE.DESCRIPTION}else{cuisine = cuisine}
  selectData1 <- geo_16[(geo_16$CUISINE.DESCRIPTION%in%cuisine)&(geo_16$ZIPCODE%in%zipcode),]
  selectData2<-selectData1[with(selectData1,order(address1,INSPECTION.DATE)),]
  selectData3 <- aggregate(selectData2,by=list(selectData2$DBA),FUN = last)
  return(selectData3)
}

outterdata<- function(cuisine="Chinese",zipcode=10025){
  if (zipcode==""){outputdata(cuisine)}
  else{outputdata(cuisine,zipcode)}
}






shinyServer(function(input, output) {
  
  observe({
    
    output$barPlot <- renderPlot({
      selected <- orig_1617[(orig_1617$CUISINE.DESCRIPTION==input$type),]
      df <- as.data.frame(table(selected$recode))
      df$Prop <- df$Freq/sum(df$Freq)
      df_sum <- as.data.frame(table(orig_1617$recode))
      df_sum$Prop <- df_sum$Freq/sum(df_sum$Freq)
      ggplot(data=df,aes(reorder(Var1,Freq),Prop))+geom_bar(stat="identity",fill='#9CCC65')+geom_point(data = df_sum,aes(reorder(Var1,Freq),Prop))+ggtitle("")+theme(text = element_text(size = 15))+xlab("Top Violations")+ylab("Frequency")+coord_flip()
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
          add_trace(y = ~one.4, name = 'winter',marker = list(color = 'rgb(165,214,167)'))%>%layout(xaxis=list(title="Violation Sub-Codes"),yaxis = list(title = 'Count'), barmode = 'stack')}
      
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
      colnames(df2)[1:2] <-c("Sub-Code Descriptions","Sub-Code")
      datatable(df2,options=list(searching = F,lengthChange=F,paging=F),rownames=F)
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
    # 
    
    output$map <- renderLeaflet({
      input$goButton
      input$goButton1
      cuisine <- isolate(input$cuisine)
      name <-isolate(input$name)
      zip <- isolate(input$zipcode)
      if (name==""){
        data<-outterdata(cuisine,zip)
      } else{
        data<- geo_16[geo_16$DBA==toupper(name),]
      }
      data <- data[order(data$SCORE),]
      if(nrow(data)<=3)
      {data$GRADE1 <-"#F1C40F"}
      else{data$GRADE1[1:3] <-"#F1C40F"}
      # data<-outterdata(input$cuisine,zip)
      # data1<- geo_16[geo_16$DBA==input$name,]
      
      
      
      #define url
      content1 <- paste(sep="","<b><a href=\'",data$url,"\'>",data$DBA,"</a></b>")
      content2 <-paste(sep="<br/>",content1,data$CUISINE.DESCRIPTION,data$address1,data$PHONE)
      
      
      leaflet(data = data) %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>% addCircles(data$lon,data$lat,radius=27,popup=content2,fillColor = (data$GRADE1),color = (data$GRADE1),stroke=FALSE,fillOpacity=0.8)%>%
        addLegend("bottomright", colors= c("#27AE60","#3399FF","#E74C3C","#F1C40F","#99A3A4"), labels=c("A","B","C","Recommendation","not graded"), title="Grade Catagory")%>%
        setView(lng = median(data$lon), lat = median(data$lat), zoom = 14)}) 
    
    output$reco <- renderDataTable({
      input$goButton
      zip <- isolate(input$zipcode)
      data<-outterdata(input$cuisine,zip)
      colnames(data)[1]<-"RESTAURANT NAME"
      datatable(data[order(data$SCORE)[1:3],c("RESTAURANT NAME","SCORE")],options=list(searching = F,lengthChange=F,paging=F),rownames=F)
      
      
      
    })
    
    
    
    
  })
})