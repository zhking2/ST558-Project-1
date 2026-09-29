
grabCodes<-function(){
  check<-varLists[[1]]
  regions<-check$REGION
  values<-regions$values
  regionCodes<-values$item
  states<-check$ST
  values<-states$values
  stateCodes<-values$item
  divisions<-check$DIVISION
  values<-divisions$values
  divisionCodes<-values$item
  geogCodes<-list("regionCodes"=names(regionCodes),"stateCodes"=names(stateCodes),"divisionCodes"=names(divisionCodes))
  return(geogCodes)
}

geogCodes <- grabCodes()

get_time_midpoint <- function(time_ranges, format_12hr = TRUE) {
  # 1. Standardize "p.m." -> "PM" and "a.m." -> "AM"
  cleaned <- gsub("p\\.m\\.", "PM", time_ranges, ignore.case = TRUE)
  cleaned <- gsub("a\\.m\\.", "AM", cleaned, ignore.case = TRUE)
  
  # 2. Split start and end
  split_times <- strsplit(cleaned, "\\s+to\\s+")
  start_str <- sapply(split_times, `[`, 1)
  end_str   <- sapply(split_times, `[`, 2)
  
  # 3. Parse with anchored dummy date
  today <- Sys.Date()
  start_time <- as.POSIXct(paste(today, start_str), format = "%Y-%m-%d %I:%M %p")
  end_time   <- as.POSIXct(paste(today, end_str),   format = "%Y-%m-%d %I:%M %p")
  
  # 4. Handle overnight boundary crossover
  overnight <- !is.na(start_time) & !is.na(end_time) & (end_time < start_time)
  end_time[overnight] <- end_time[overnight] + (24 * 3600)
  
  # 5. Compute midpoint
  midpoint <- start_time + (end_time - start_time) / 2
  
  # 6. Format output (12-hour AM/PM vs 24-hour)
  if (format_12hr) {
    return(format(midpoint, "%I:%M %p")) # Returns e.g. "06:37 PM"
  } else {
    return(format(midpoint, "%H:%M"))    # Returns e.g. "18:37"
  }
}

getJWAPPED<-function(){
  check<-varLists[[1]]
  jwap1<-check$JWAP
  values<-jwap1$values
  JWAP<-unlist(values$item)
  ids<-names(JWAP)
  JWAP<-cbind("JWAP"=ids,"times"=JWAP)
  JWAP[,2]<-get_time_midpoint(JWAP[,2])
  JWAP<-as_tibble(JWAP)
  return(JWAP)
}

getJWDPPED<-function(){
  check<-varLists[[1]]
  jwdp1<-check$JWDP
  values<-jwdp1$values
  JWDP<-unlist(values$item)
  ids<-names(JWDP)
  JWDP<-cbind("JWDP"=ids,"times"=JWDP)
  JWDP[,2]<-get_time_midpoint(JWDP[,2])
  JWDP<-as_tibble(JWDP)
  return(JWDP)
}

allStates <- paste(geogCodes$stateCodes, collapse = ",")
allRegions <- paste(geogCodes$regionCodes, collapse = ",")
allDivisions <- paste(geogCodes$divisionCodes, collapse = ",")

matchInputsSimple <- function(key,year,agep,gasp,grpip,jwap,jwdp,jwmnp,sex,fer,hhl,sch,schl,geog,gsubset){
  if(!all(is.logical(c(agep,gasp,grpip,jwap,jwdp,jwmnp,sex,fer,hhl,sch,schl)))){
    stop("Each of the non-filtering variables takes a T/F value")
  }
  if(!is.character(key)){
    stop("The census key must be entered as a string")
  }
  geog <- tolower(geog)
  if(!geog %in% c("st","state","region","division")){
    stop("The geography options are st, state, region, and division.")
  }
}

numVars <- c(agep=agep, gasp=gasp,grpip=grpip,jwap=jwap,jwdp=jwdp,jwmnp=jwmnp)
catVars <- c(sex=sex,fer=fer,hhl=hhl,sch=sch,schl=schl)

atLeastOne <- function(numVars,catVars){
  if (!any(numVars)) {
    stop("Choose at least one numeric variable.")}
  if (!any(catVars)) {
    stop("Choose at least one categorical variable.")
  }
}

setVarString <- function(numVars,catVars){
  selectedNums <- names(numVars)[numVars]
  selectedCats <- names(catVars)[catVars]
  allRequested <- unique(c("PWGTP", selectedNums, selectedCats))
  realVars <- toupper(paste(allRequested, collapse = ","))
}

varString <- toupper(setVarString(numVars,catVars))

matchGeogSubsets <- function(geog,gsubset){if(geog == "region"){
  options<-geogCodes$regionCodes
  options[[6]]<-"All"}
  if(geog == "division"){
    options<-geogCodes$divisionCodes
    options[[11]]<-"All"}
  if(geog %in% c("st","state")){
    options<-geogCodes$stateCodes
    options[[53]]<-"All"}
  
  if(!gsubset %in% options){
    stop(paste0("The ", geog," code selected does not exist, choose from: ",paste(options,collapse = ",")))}}

fixDumbSwitch <- function(year,geog){
  if(geog %in% c("st","state")){
    ifelse(year < 2023,geog<-"state",geog <- "state")
  }
}

geog<-fixDumbSwitch(year,geog)

allowForAll <- function(geog,gsubset){
  if (gsubset == "All"){
    if(geog == "region"){gsubset <-allRegions}
    else if (geog == "division"){gsubset <- allDivisions}
    else if (geog %in% c("st","state")){gsubset <- allStates}
  }
  else {gsubset <- gsubset}
}

gsubset<-allowForAll(geog,gsubset)

setGeogString <- function(geog,gsubset){
  geogString <- paste0(geog,":",gsubset)
}

geogString <- setGeogString(geog,gsubset)

baseUrl <-paste0("https://api.census.gov/data/",year,"/acs/acs1/pums")

censusUrl <- paste0(
  "https://api.census.gov/data/", year, "/acs/acs1/pums",
  "?get=", varString,
  "&for=", geog,":",gsubset,
  "&key=", key
)




request <- GET(
  url = censusUrl
)
rawStats <- content(request, "text", encoding = "UTF-8")
parsedStats <- fromJSON(rawStats)
censusPull <- as_tibble(parsedStats)
colnames(exampleStats) <- censusPull[1, ]
censusPull <- censusPull[-1, , drop = FALSE]

apiHarmer <- function(key,year=2024,agep=T,gasp=F,grpip=F,jwap=F,jwdp=F,jwmnp=F,sex=T,fer=F,hhl=F,sch=F,schl=F,geog="region",gsubset="2"){
  # First: let's verify all of the inputs are correct
  
  
  
  # Next: let's create the helpers:

  
  # 
}
  

