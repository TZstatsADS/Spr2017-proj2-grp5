library(dplyr)
library(ggmap)
library(data.table)
library(rvest)
library(stringr)
library(tidyr)
setwd("/Users/yingxinzhang/Desktop/Spr2017-proj2-grp5-master/data")

orig_data <- read.csv("rstrt.csv",na.strings = c(""," "))
#seperate year from the date
orig_data$year <-substr(as.character(orig_data$INSPECTION.DATE), 7,10)
#create season
orig_data$month <-substr(as.character(orig_data$INSPECTION.DATE), 1,2)
orig_data$season <- ifelse(orig_data$month%in%c("01","02","03"),1,ifelse(orig_data$month%in%c("04","05","06"),2,ifelse(orig_data$month%in%c("07","08","09"),3,ifelse(orig_data$month%in%c("10","11","12"),4,NA))))
#seperate code into two
orig_data$vio_code1 <-substr(as.character(orig_data$VIOLATION.CODE),1,2)
orig_data$vio_code2 <-substr(as.character(orig_data$VIOLATION.CODE),3,3)
#select data from 2016
orig_1617 <-orig_data[((orig_data$year=="2016")),]
#adding descriptions for each category of violation
#must copy original vio_code1 before recode
orig_1617$recode <-orig_1617$vio_code1
orig_1617$recode <-recode_factor(orig_1617$recode,"02"="Food Temperature","03"="Food Source","04"="Food Protection","05"="Facility Design","06"="Personal Hygiene","07"="Other Criticals","08"="Vermin/Garbage","09"="Food Source II","10"="Facility Maintenance","15"="Tobacco Related","16"="Artificial Transfat","18"="Admin and Documentation","20"="Signage","22"="Miscellaneous")

tb<-as.data.frame(table(orig_1617$CUISINE.DESCRIPTION))
tb<-tb[order(-tb$Freq),]
tb <- tb[tb$Freq>=378,]
tbname <- as.character(tb$Var1)
tbname[5] <- "Cafe/Coffee/Tea"
tb$Var1 <- tbname

tb_vio<-as.data.frame(table(orig_1617$recode))
tb_vio<-tb_vio[order(-tb_vio$Freq),]


orig_1617$address1<-paste(orig_1617$BUILDING,orig_1617$STREET,orig_1617$ZIPCODE,sep=" ")
add <- unique(orig_1617$address1)
################This is the code for generating longitude and latitude ################

# orig_1617$address1<-paste(orig_1617$BUILDING,orig_1617$STREET,orig_1617$ZIPCODE,sep=" ")

# add_1<-add[2401:4900]
# lonlat <-geocode(add[2401:4900])
# lonlat$X <- c(1:2500)
# add_latlon <-cbind(add_1,lonlat)
# geo_1617 <- merge(orig_1617,add_latlon,by.x = "address",by.y="add_1",all.x = TRUE)
# geo_2500 <- geo_1617[which(!is.na(geo_1617$lat)),]
# selectData<-geo_2500[geo_2500$CUISINE.DESCRIPTION=="Indian",]
# selectData<-selectData[with(selectData,order(address,INSPECTION.DATE)),]
# selectData <- aggregate(selectData,by=list(selectData$DBA),FUN = last)
# selectData<-selectData[with(selectData,order(SCORE)),]

################The data has been saved as "lonlatx-x.csv" files in /data folder###################


# read in latlon data


lonlat_1 <- read.csv("lonlat1-2500.csv")
lonlat_2501 <- read.csv("lonlat2501-5000.csv")
lonlat_5001 <- read.csv("lonlat5001-7500.csv")
lonlat_7501 <- read.csv("lonlat7501-10000.csv")
lonlat_10001 <- read.csv("lonlat10001-12500.csv")
lonlat_12501 <- read.csv("lonlat12501-15000.csv")
lonlat_15001 <- read.csv("lonlat15001-17500.csv")
lonlat_17501 <- read.csv("lonlat17501-20000.csv")
lonlat_20001 <- read.csv("lonlat20001-21718.csv")

lonlat_all <- rbind(lonlat_1,lonlat_2501,lonlat_5001,lonlat_7501,lonlat_10001,lonlat_12501,lonlat_15001,lonlat_17501,lonlat_20001)
addlonlat <- data.frame(add,lonlat_all)[-2]
#write.csv(lonlat_all,"lonlat_all")
colnames(addlonlat)[1] <- "address1"
geo_1617 <- merge(orig_1617,addlonlat,by = "address1")
#geo_1617 <- merge(orig_1617,addlonlat,by.x = "address1",by.y="address1",all.x = TRUE)

#drop NA?
##geo_1617 <- geo_1617[which(!is.na(geo_1617$lat)),]

geo_1617$GRADE1 <- ifelse(is.na(geo_1617$SCORE),"#99A3A4",ifelse(geo_1617$SCORE<=13,'#27AE60',ifelse(geo_1617$SCORE<=27,"#9B59B6","#E74C3C")))
# zip <-unique(geo_1617$ZIPCODE)




###############################This is the code for scraping Yelp data##########################################

#lops for getting html page
# link_source <-c()
# t <-unique(page %>%html_nodes("#super-container .js-analytics-click") %>% html_attr('href'))
# link_source<- c(link_source,t)
# #url
# url_query <- geo_1617[,c("DBA","ZIPCODE")]
# url_query$DBA <- gsub("& ","",url_query$DBA)
# url_query$DBA <- gsub(" ","\\+",url_query$DBA) 
# url_query <-unique(url_query)
# url_query$DBA <-gsub("\032","",url_query$DBA)
# page1 <- c(rep(NA,nrow(url_query)))
# #add NA to row 5405 of page1
# page2 <-c(rep(NA,8360))
# for (i in 5406:13765){
#   page=read_html(paste("https://www.yelp.com/search?find_desc=",url_query$DBA[i],"&find_loc=",url_query$ZIPCODE[i],sep = ""))
#   page2[i-5405]<-(page %>%html_nodes(".indexed-biz-name .js-analytics-click") %>% html_attr('href'))[1]
# }

##########################The data has been saved as "page x-x.csv" files in data folder######################



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


#function for geomap
outputdata <- function(cuisine,zipcode=10025){
  if(is.null(cuisine)){cuisine=geo_16$CUISINE.DESCRIPTION}else{cuisine = cuisine}
  selectData1 <- geo_16[(geo_16$CUISINE.DESCRIPTION%in%cuisine)&(geo_16$ZIPCODE%in%zipcode),]
  selectData2<-selectData1[with(selectData1,order(address1,INSPECTION.DATE)),]
  selectData3 <- aggregate(selectData2,by=list(selectData2$DBA),FUN = last)
  return(selectData3)
}



outterdata<- function(cuisine="Chinese",zipcode){
  if (zipcode==""){outputdata(cuisine)}
  else{outputdata(cuisine,zipcode)}
}

