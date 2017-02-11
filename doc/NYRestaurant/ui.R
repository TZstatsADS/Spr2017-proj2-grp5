#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
fluidPage(
titlePanel("What problem you might face?"),

sidebarLayout(
selectInput("type",label="Cuisine Type", choices=orig_1617$CUISINE.DESCRIPTION)
),

mainPanel(
plotOutput("barPlot") #hist function in server
)
))
