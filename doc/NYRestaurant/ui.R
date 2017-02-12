#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(
fluidPage(
titlePanel("What problem you might face?"),

sidebarPanel(
selectInput("type",label="Cuisine Type", choices=c("Chinese","American")),
selectInput("vio_type",label="Violation Type", choices=c("Food Protection","Personal Hygiene"))
),

mainPanel(
  fluidRow(plotOutput("barPlot"),plotOutput("barPlot1"))
)
))
