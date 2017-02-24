library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(leaflet)
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







shinyUI(navbarPage("CLEAN DINING WITH US   ",
                   tabPanel("Home",
                            sidebarLayout(
                              div(class="side", sidebarPanel(width=0)),
                              mainPanel(width=12,
                                        img(src="featured2.png", style="width:100%")
                              )
                            )
                            
                   ),
                   tabPanel("Map Explorer",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Find Your Dining Place"),
                                              h4("Please enter a restaurant name or select cuisine type and zipcode  "),
                                              textInput("name", label = h3("Restaurant"), 
                                                        value = "",placeholder = "If you know the name..."),
                                              actionButton("goButton1","Enter"),
                                              selectInput("cuisine", h3("Cuisine"),choices = tb1$Var1,selected ="Chinese", multiple = T),
                                              # selectInput("month", h3("Month"),choices = sort(unique(geo_1617$month)),selected = "02"),
                                              textInput("zipcode", label = h3("Zipcode"), 
                                                        value = ""),
                                              
                                              actionButton("goButton","Enter"),
                                              # selectInput("", "Size", vars, selected = "adultpop"),
                                              conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                               # Only prompt for threshold when coloring or sizing by superzip
                                                               numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                              ),
                                              br(),
                                              h3("Our recommendation "),
                                              
                                              DT::dataTableOutput("reco")
                                              # plotOutput("histCentile", height = 200),
                                              # plotOutput("scatterCollegeIncome", height = 250)
                                )
                                
                                
                            )
                   ),
                   tabPanel("Violation Tracker",
                            fluidRow(                           
                              column(6,  selectInput(
                                "type",label="See Top Violations in:", choices=tb$Var1)#orig_1617$CUISINE.DESCRIPTION
                              ),
                              column(6, selectInput("vio_type",label="See More Detailed Violations in:", choices=tb_vio$Var1)#orig_1617$CUISINE.DESCRIPTION
                                     
                              )),
                            
                            br(),
                            br(),
                            fluidRow(
                              column(7,
                                     plotOutput("barPlot")
                              ),                              
                              column(5,
                                     plotlyOutput("barPlot1")      
                              ),
                              column(12,
                                     DT::dataTableOutput("table"))
                            )
                            
                            
                   )
)
)