# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)

shinyUI(navbarPage("",
                   tabPanel("Visualize Features",
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
                                                  
                            
                   )
                   )
)