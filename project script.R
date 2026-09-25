library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(readr)


l21<-"https://api.census.gov/data/2021/acs/acs1/pums/variables.json"
l22<-"https://api.census.gov/data/2022/acs/acs1/pums/variables.json"
l23<-"https://api.census.gov/data/2023/acs/acs1/pums/variables.json"
l24<-"https://api.census.gov/data/2024/acs/acs1/pums/variables.json"

get_vars <- function(link){
  var_return <- httr::GET(link)
  var_parsed <- fromJSON(rawToChar(var_return$content))
  vars_llist <- var_parsed$variables
  vars_fin <- vars_llist[c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP", "FER", "HHL", "SCH", "SCHL", "SEX", "ST","STATE", "REGION", "DIVISION")]
  vars_fin[!sapply(vars_fin,is.null)]
  
}

everything <- httr::GET(l21)
everything<-fromJSON(rawToChar(everything$content))
  

var_list_21 <- get_vars(l21)
var_list_22 <- get_vars(l22)
var_list_23 <- get_vars(l23)
var_list_24 <- get_vars(l24)
var_lists <- list(var_list_21,var_list_22,var_list_23,var_list_24)

saveRDS(var_lists, file = "data/var_lists.rds")
var_lists_check<-readRDS("data/var_lists.rds")
remove(l21,l22,l23,l24,get_vars,var_lists_check)


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

















