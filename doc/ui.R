#ui
library(shiny)
fluidPage(
    titlePanel("shiny"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Choropleth"),
        
        selectInput("cuisine", 
                    label = "Choose a cuisine to display",
                    choices = list("Chinese","American","Italian","Mexican","Japanese","French"),
                    selected = "Chinese")
        ),
      
      mainPanel(imageOutput("plot"))
    )
)
  
  
  