# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(leaflet)
shinyUI(navbarPage("Restaurant Violation",
                   tabPanel("Violation Tracker",
                            fluidRow(                           
                              column(6,  selectInput(
                              "type",label="Cuisine Type", choices=tb$Var1)#orig_1617$CUISINE.DESCRIPTION
                              ),
                              column(6, selectInput("vio_type",label="Violation Type", choices=tb_vio$Var1)#orig_1617$CUISINE.DESCRIPTION
  
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
                                     dataTableOutput("table"))
                            )
                                                  
                            
                   ),tabPanel("Interactive map",
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

                                                h2("ZIP explorer"),

                                                selectInput("cuisine", h3("Cuisine Type"),choices = tb$Var1,selected = "Chinese"),
                                                textInput("zipcode", label = h3("Zipcode"), 
                                                          value = 10025),
                                                # selectInput("", "Size", vars, selected = "adultpop"),
                                                conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                                 # Only prompt for threshold when coloring or sizing by superzip
                                                                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                                )
                                                # 
                                                # plotOutput("histCentile", height = 200),
                                                # plotOutput("scatterCollegeIncome", height = 250)
                                  )
                                  
                                  
                              )
                   )
                   )
)