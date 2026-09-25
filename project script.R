library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(readr)


census_key<-readRDS("../censusKey.rds")
base_url <- "https://api.census.gov/data/2024/acs/acs1/pums"



if(!is.integer(GSUBSET) | !is.logical(c(AGEP,GRPIP,JWAP,JWDP,JWMNP,HHL,SCH,SCHL) | !is.character(c(GEOG,KEY)) )){
  stop("Wrong Kind of Input")}


api_helper <- function(KEY,YEAR,AGEP=TRUE,GRPIP=FALSE,JWAP=FALSE,JWDP=FALSE,JWMNP=FALSE, 
                       SEX=TRUE, HHL=FALSE, SCH=FALSE, SCHL=FALSE, GEOG="ST",GSUBSET=99999){
  
  # First Step is to make sure that all of the inputs are correct.
  
  if (!is.numeric(GSUBSET) || 
      !all(is.logical(c(AGEP, GRPIP, JWAP, JWDP, JWMNP, SEX, HHL, SCH, SCHL))) || 
      !all(is.character(c(GEOG, KEY)))) {
    stop("Wrong Kind of Input")
  }
  if(!YEAR %in% c(2021,2022,2023,2024)) {
    stop("Year Not in Range")}
  if(!GEOG %in% c("ST","STATE","REGION","DIVISION")){
    stop("Not a Geography Option")}
  
  # Next, I need to fix the whole ST/STATE issue.
  
  if(!YEAR %in% c(2023,2024) & GEOG == "ST"){
    GEOG <- "STATE"}
  if(!YEAR %in% c(2021,2022) & GEOG == "STATE"){
    GEOG <- "ST"}
  
  # Here, I'm checking to make sure all of the geography sub-setting lines up.
  
  GSUBSET <- as.character(GSUBSET)
  if(GEOG %in% c("ST","STATE") & !GSUBSET %in% codes$stateCodes){
    GSUBSET<-"01"}
  if(GEOG %in% c("REGION") & !GSUBSET %in% codes$regionCodes){
    GSUBSET<-"1"}
  if(GEOG %in% c("DIVISION") & !GSUBSET %in% codes$divisionCodes){
    GSUBSET<-"1"}
  
  # This is the baseline URL without anything applied yet.

  baseURL <- paste0("https://api.census.gov/data/",as.character(YEAR),"/acs/acs1/pums")
  
  # Now I'm setting up the default variables, and adding the selected variables to the pile.
  
  baseVars <- c("PWGTP","GASP","FER")
  specVars <-c(AGEP = "AGEP", GRPIP = "GRPIP", JWAP = "JWAP", JWDP = "JWDP", JWMNP = "JWMNP", 
               SEX = "SEX", HHL="HHL", SCH="SCH", SCHL="SCHL")
  selectedSpecVars <- specVars[c(AGEP, GRPIP, JWAP, JWDP, JWMNP,SEX,HHL,SCH,SCHL)]
  realVars <- paste(c(baseVars, selectedSpecVars), collapse = ",")
  
  # Here's where I'm making the geography line:
  
  realGeog <- paste0(tolower(GEOG),":",GSUBSET)
  
  # We should now finally be ready to actually call the API
  
  request <- GET(
    url = base_url,
    query = list(
      get = realVars,
      `for` = realGeog,
      key = KEY
    )
  )
  raw_stats <- content(request, "text", encoding = "UTF-8")
  parsed_stats <- fromJSON(raw_stats)
  example_stats <- as_tibble(parsed_stats)
  
}


woah<-api_helper(census_key,2021, GEOG="ST",GSUBSET=99999)

