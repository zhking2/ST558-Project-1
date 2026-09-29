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


KEY=censusKey;YEAR=2021;AGEP=TRUE;GASP=FALSE;GRPIP=FALSE;JWAP=FALSE;JWDP=FALSE;JWMNP=FALSE;SEX = TRUE;FER=FALSE; HHL=FALSE; SCH=FALSE; SCHL=FALSE;GEOG="STATE";GSUBSET="01"

  
  # this if clause is checking if the year is one of the ones we're working with
  if(!YEAR %in% c(2021,2022,2023,2024)) {
    print("Year Not in Range")}
  
  # this is checking that the other inputs are the correct type, and at least 1 numerical and 1 categorical are selected
  numVars <- c(AGEP = AGEP, GASP = GASP, GRPIP = GRPIP, JWAP = JWAP, JWDP = JWDP, JWMNP = JWMNP)
  catVars <- c(SEX = SEX, FER = FER, HHL = HHL, SCH = SCH, SCHL = SCHL)
  if (!any(numVars)) {
    print("Choose at least one numeric variable.")}
  if (!any(catVars)) {
    print("Choose at least one categorical variable.")
  }
  if (!all(is.logical(c(numVars,catVars))) || 
      !all(is.character(c(GEOG,KEY,GSUBSET)))) {
    print("Wrong Kind of Input, variable selection is binary, and GEOG, KEY, and GSUBSET are strings.")}
  
  # time to deal with geography stuff:
  
  # first, lets call our geography values helper function
  geogUpper <- toupper(GEOG)
  
  # now, we check to make sure that geog is in one of the options
  if (!geogUpper %in% c("ST", "STATE", "REGION", "DIVISION")) {
    print("Geography must be STATE, REGION, or DIVISION")
  }
  
  # next, we have to fix the whole st/state issue between years, and then 
  if(geogUpper == "REGION"){
    apiGeog <- "region"
    validCodes <- geogCodes$regionCodes}
  if(geogUpper == "DIVISION"){
    apiGeog <- "division"
    validCodes <- geogCodes$divisionCodes}
  if(geogUpper %in% c("ST","STATE")){
    validCodes <- geogCodes$stateCodes
    if(YEAR %in% c(2021,2022)){
      apiGeog<-"st"}
    else{apiGeog<-"state"}}
  
  # Construct realGeog string
  if (GSUBSET == "All" || !GSUBSET %in% validCodes) {
    realGeog <- paste0(apiGeog,":",paste(validCodes, collapse = ","))
  } else {
    realGeog <- paste0(apiGeog, ":", GSUBSET)
  }
  
  # This is the baseline URL without anything applied yet.
  
  baseURL <- paste0("https://api.census.gov/data/",as.character(YEAR),"/acs/acs1/pums")
  
  # Now I'm setting up the default variables, and adding the selected variables to the pile.
  selectedNums <- names(numVars)[numVars]
  selectedCats <- names(catVars)[catVars]
  allRequested <- unique(c("PWGTP", selectedNums, selectedCats))
  
  # Finally, I'm creating the list of variables that will be called on.
  realVars <- paste(allRequested, collapse = ",")
  
  # We should now finally be ready to actually call the API, and turn the results into a tibble. 
  
  finalURL<-paste0(
      "https://api.census.gov/data/2024/acs/acs1/pums","?get=",
      realVars,
      "&for=",
      realGeog,
      "37",
      "&key=", censusKey)
  
  
  rawStats <- content(request, "text", encoding = "UTF-8")
  parsedStats <- fromJSON(rawStats)
  unStats <- as_tibble(parsedStats)
  
  # fixing the column names
  colnames(unStats) <- unStats[1, ]
  unStats <- unStats[-1, , drop = FALSE]
  
  # lets call the helper functions from earlier to make the jwap/jwdp values
  
  JWAPVals<-getJWAPPED()
  JWDPVals<-getJWDPPED()
  
  # Here I'm joining the output with my JWAPvals tibble, so that I can swap out the time codes for the actual times
  if ("JWAP" %in% names(unStats)) {
    unStats <- unStats |>
      left_join(JWAPvals, by = "JWAP") |> 
      mutate(JWAP = times) |> # here's where I do that swap
      select(-times)}    # now I'm removing the leftovers of the join
  
  # and now I repeat the process for JWDP, which unfortunately has different values from JWAP
  if ("JWDP" %in% names(unStats)) {
    unStats <- unStats |>
      left_join(JWDPvals, by = "JWDP") |>
      mutate(JWDP = times) |>
      select(-times)}
  
  # Here's where I'm editing the output to ensure all of the variables are the right types.
  
  unStats <- unStats |>
    mutate(across(any_of(c("PWGTP", "GASP", "AGEP", "GRPIP", "JWMNP")), as.double)) # this is setting the numerical variables to doubles
  
  yearKey <- paste0("y",YEAR)
  yearMeta <- varLists[[yearKey]]
  
  for (catVar in selectedCats) {
    if (catVar %in% names(unStats) && catVar %in% names(yearMeta)) {
      valItems <- yearMeta[[catVar]]$values$item
      if (!is.null(valItems)) {
        levelsVec <- names(valItems)
        labelsVec <- unname(unlist(valItems))
        unStats[[catVar]] <- factor(unStats[[catVar]], levels = levelsVec, labels = labelsVec)}}}
  
  
  class(unStats) <- c("census", class(unStats))
  
  return(unStats)                        
  
}
