library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(readr)
library(lubridate)


census_key<-readRDS("../censusKey.rds")
base_url <- "https://api.census.gov/data/2024/acs/acs1/pums"



woah<-api_helper(censusKey,2021,AGEP=TRUE,GRPIP=T,JWAP=T,JWDP=T,JWMNP=T, 
  SEX=T, HHL=T, SCH=T, SCHL=T, GEOG="ST",GSUBSET=99999)