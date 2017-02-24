library(dplyr)
library(ggmap)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)

#write.csv(orig_1617,"orig_1617.csv")
orig_1617<-read.csv("orig_1617.csv")[-1]

#restaurant name
tb<-as.data.frame(table(orig_1617$CUISINE.DESCRIPTION))
tb<-tb[order(-tb$Freq),]
tb <- tb[tb$Freq>=378,]
tbname <- as.character(tb$Var1)
tbname[5] <- "Cafe/Coffee/Tea"
tb$Var1 <- tbname
#
tb1 <- tb[order(tb$Var1),]
#saveRDS(tb1,"tb1.Rdata")

tb_vio<-as.data.frame(table(orig_1617$recode))
tb_vio<-tb_vio[order(-tb_vio$Freq),]
#saveRDS(tb_vio,"tb_vio.Rdata")
#Selecting address
orig_1617$address <- paste(orig_1617$BUILDING,orig_1617$STREET)
orig_1617$address1<-paste(orig_1617$BUILDING,orig_1617$STREET,orig_1617$ZIPCODE,sep=" ")
add <- unique(orig_1617$address1)

#saveRDS(orig_1617,"orig_1617.Rdata")
#add lonlat & GRADE1 for markers
addlonlat1<-read.csv("addlonlat.csv")
geo_1617 <- merge(orig_1617,addlonlat1[,-1],by.x = "address1",by.y="address1",all.x = TRUE)
geo_1617$GRADE1 <- ifelse(is.na(geo_1617$SCORE),"#99A3A4",ifelse(geo_1617$SCORE<=13,'#27AE60',ifelse(geo_1617$SCORE<=27,"#3399FF","#E74C3C")))

#saveRDS(geo_1617,"geo_1617.Rdata")


## scraping Yelp data
#lops for getting html page
# link_source <-c()
# 
# t <-unique(page %>%html_nodes("#super-container .js-analytics-click") %>% html_attr('href'))
# link_source<- c(link_source,t)

#url
# url_query <- geo_1617[,c("DBA","ZIPCODE")]
# url_query$DBA <- gsub("& ","",url_query$DBA)
# url_query$DBA <- gsub(" ","\\+",url_query$DBA) 
# url_query <-unique(url_query)
# url_query$DBA <-gsub("\032","",url_query$DBA)

#read in url data
page7500<-read.csv("page1-7500.csv")
page15000<-read.csv("page7501-15000.csv")
page22401<-read.csv("page15001-22401.csv")
page_all<-c(as.character(page7500$x),as.character(page15000$x),as.character(page22401$x))

url_DBAZIP <- unique(orig_1617[,c("DBA","ZIPCODE")])
url_DBAZIP$url <- page_all
url_DBAZIP$url <- paste("https://www.yelp.com",url_DBAZIP$url,sep="")
geo_16 <- merge(geo_1617,url_DBAZIP,by.x = c('DBA','ZIPCODE'), by.y = c('DBA','ZIPCODE'), all.x = TRUE)
geo_1617<-geo_16
geo_16 <- geo_1617[,-c(4,6,7,11,12,13,14,18:26)]
#saveRDS(geo_16,"geo_16.Rdata")

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

