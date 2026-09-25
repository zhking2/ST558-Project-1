library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(readr)


census_key<-readRDS("../censusKey.rds")
base_url <- "https://api.census.gov/data/2024/acs/acs1/pums"

api_ex <- GET(
  url = base_url,
  query = list(
    get = "SEX,PWGTP,MAR",
    `for` = "state:*",
    SCHL = "24",
    key = census_key
  )
)
raw_stats <- content(api_ex, "text", encoding = "UTF-8")
parsed_stats <- fromJSON(raw_stats)
example_stats <- as_tibble(parsed_stats[-1, , drop = FALSE])
remove(api_ex,raw_stats,parsed_stats)






api_helper <- function(YEAR,AGEP=TRUE,GRPIP=FALSE,JWAP=FALSE,JWDP=FALSE,JWMNP=FALSE, 
                       SEX=TRUE, HHL=FALSE, SCH=FALSE, SCHL=FALSE, GEOG="ST",GSUBSET="NULL"){
  
  if(!is.integer(vec) | !is.logical(c(AGEP,GRPIP,JWAP,JWDP,JWMNP,HHL,SCH,SCHL))){
    stop("Wrong Kind of Input")}
  if(!YEAR %in% c(2021,2022,2023,2024)) {
    stop("Year Not in Range")}
  if(!YEAR %in% c(2023,2024) & GEOG == "ST"){
    GEOG <- "STATE"}
  if(!YEAR %in% c(2021,2022) & GEOG == "STATE"){
    GEOG <- "ST"}
  if(!GEOG %in% c("ST","STATE","REGION","DIVISION")){
    stop("Not a Geography Option")}
  if(GSUBSET == "NULL"){
    ifelse(GEOG %in% c("ST","STATE"),GSUBSET<-"Alabama/AL",
           ifelse(GEOG == "REGION",,
                  ifelse(GEOG == "DIVISION",)))
  }
  
  baseURL <- paste0("https://api.census.gov/data/",as.character(YEAR),"/acs/acs1/pums")
  
  baseVars <- c("PWGTP","GASP","FER")
  specVars <-c(AGEP = "AGEP", GRPIP = "GRPIP", JWAP = "JWAP", JWDP = "JWDP", JWMNP = "JWMNP", 
               SEX = "SEX", HHL="HHL", SCH="SCH", SCHL="SCHL")
  selectedSpecVars <- specVars[c(AGEP, GRPIP, JWAP, JWDP, JWMNP,SEX,HHL,SCH,SCHL)]
  realVars <- paste(c(baseVars, selected_specVars), collapse = ",")
  
  request <- GET(
    url = base_url,
    query = list(
      get = realVars,
      `for` = GEOG,
      key = census_key
    )
  )
  
}


check<-var_lists[[1]]
states<-check$ST[4]
values<-states$values
stateCodes<-unlist(values$item)
item<-values$item

stateCodes<-as_tibble(stateCodes)|>
  arrange()
