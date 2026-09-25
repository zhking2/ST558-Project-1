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






api_helper <- function(YEAR,AGEP=TRUE,GRPIP=FALSE,JWAP=FALSE,JWDP=FALSE,JWMNP=FALSE, SEX=TRUE, HHL=FALSE, SCH=FALSE, SCHL=FALSE){
  if(!is.integer(vec) | !is.logical(c(AGEP,GRPIP,JWAP,JWDP,JWMNP,HHL,SCH,SCHL))){
    stop("Wrong Kind of Input")
  }
  if(!YEAR %in% c(2021,2022,2023,2024)) {
    stop("Year Not in Range")
  }
  baseURL <- paste0("https://api.census.gov/data/",as.character(YEAR),"/acs/acs1/pums")
  baseVARS <- c("PWGTP","GASP","FER")
  specVARS <-c(AGEP = "AGEP", GRPIP = "GRPIP", JWAP = "JWAP", JWDP = "JWDP", JWMNP = "JWMNP", SEX = "SEX", HHL="HHL", SCH="SCH", SCHL="SCHL")
  selected_specVARS <- specVARS[c(AGEP, GRPIP, JWAP, JWDP, JWMNP,SEX,HHL,SCH,SCHL)]
  
  request <- GET(
    url = base_url,
    query = list(
      get = baseVARS,
      `for` = "state:*",
      SCHL = "24",
      key = census_key
    )
  )
  
}

AGEP=T
GRPIP=T
JWAP=T
JWDP=T
JWMNP=T






