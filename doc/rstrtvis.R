
if (!require("DT")) install.packages('DT')
if (!require("dtplyr")) install.packages('dtplyr')
if(!require("lubridate")) install.packages('lubridate')

library(dtplyr)
library(dplyr)
library(DT)
library(lubridate)
library(choroplethrZip)
library(shiny)
library(devtools)

if (!require("choroplethr")) install.packages("choroplethr")
if (!require("devtools")) install.packages("devtools")
if (!require("choroplethrZip")) 
  devtools::install_github('arilamstein/choroplethrZip@v1.5.0')
if (!require("ggplot2")) devtools::install_github("hadley/ggplot2")
if (!require("ggmap")) devtools::install_github("dkahle/ggmap")  



rstrt=read.csv(file="rstrt2016.csv")
#datatable(sample_n(rstrt, 50))
head(rstrt)

rstrtvis <- function(type)
{
  rstrtvis <- rstrt[rstrt$CUISINE.DESCRIPTION==type,]
  
  rstrtzipvis=
    rstrtvis%>%
    filter(ZIPCODE>0)%>%
    mutate(region=as.character(ZIPCODE))
  
  count.dfvis=rstrtzipvis%>%
    group_by(region)%>%
    dplyr::summarise(
      value=n()
    )

  nyc_fips = c(36005, 36047, 36061, 36081, 36085)
  zip_choropleth(count.dfvis,
                 county_zoom = nyc_fips,
                 title       = "NYC restaurants distribution",
                 legend      = "number of restaurants")
}

