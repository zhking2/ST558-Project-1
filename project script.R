library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(readr)
library(lubridate)


censusKey<-readRDS("../censusKey.rds")
base_url <- "https://api.census.gov/data/2024/acs/acs1/pums"



woah<-apiHelper(censusKey,2021,AGEP=TRUE,GRPIP=T,JWAP=T,JWDP=T,JWMNP=T, 
  SEX=T, HHL=T, SCH=T, SCHL=T, GEOG="ST",GSUBSET="01")
dawg<-censusAPIMultiyear(censusKey,T,T,F,F,AGEP=TRUE,GRPIP=F,JWAP=F,JWDP=F,JWMNP=F, 
     SEX=T, HHL=F, SCH=F, SCHL=F, GEOG="STATE",GSUBSET="01")


KEY=censusKey;YEAR=2024;AGEP=TRUE;GASP=FALSE;GRPIP=FALSE;JWAP=FALSE;JWDP=FALSE;JWMNP=FALSE;SEX = TRUE;FER=FALSE; HHL=FALSE; SCH=FALSE; SCHL=FALSE;GEOG="STATE";GSUBSET="01"

geogUpper <- "REGION"
  YEAR<-2023
  
  
if(geogUpper == "REGION"){
  apiGeog <- "region"
  validCodes <- geogCodes$regionCodes}
  
if(geogUpper == "REGION"){
  apiGeog <- "division"
  validCodes <- geogCodes$divisionCodes}
if(geogUpper %in% c("ST","STATE")){
  validcodes <- geogCodes$stateCodes
  if(YEAR %in% c(2021,2022)){
    apiGeog<-"st"
    
  }
  else{apiGeog<-"state"}
}