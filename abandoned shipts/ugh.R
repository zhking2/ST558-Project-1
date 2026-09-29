
apiHarmer <- function(key,year=2024,agep=T,gasp=F,grpip=F,JWAP=F,JWDP=F,jwmnp=F,sex=T,fer=F,hhl=F,sch=F,schl=F,geog="region",gsubset="2"){
  
  # We need a bunch of helper functions before we can get to the real deal.
  
  # First, we need a function that checks if the basic variable inputs make sense.
  
  matchInputsSimple <- function(key,year,agep,gasp,grpip,JWAP,JWDP,jwmnp,sex,fer,hhl,sch,schl,geog,gsubset){
    if(!all(is.logical(c(agep,gasp,grpip,JWAP,JWDP,jwmnp,sex,fer,hhl,sch,schl)))){
      stop("Each of the non-filtering variables takes a T/F value")
    }
    if(!is.character(key)){
      stop("The census key must be entered as a string")
    }
    geog <- tolower(geog)
    if(!geog %in% c("state","region","division")){
      stop("The geography options are state, region, and division.")
    }
  }
  
  # Next, we need to check if the user has selected at least one numeric and one categorical variable.
  
  atLeastOne <- function(numVars,catVars){
    if (!any(numVars)) {
      stop("Choose at least one numeric variable.")}
    if (!any(catVars)) {
      stop("Choose at least one categorical variable.")
    }
  }
  
  #Now we'll create the string for the selected (and prescribed) variables.

  setVarString <- function(numVars,catVars){
    selectedNums <- names(numVars)[numVars]
    selectedCats <- names(catVars)[catVars]
    allRequested <- unique(c("PWGTP", selectedNums, selectedCats))
    realVars <- toupper(paste(allRequested, collapse=","))
  }
  
  
    
  #Now we need to deal with the geography. 
  #The rubric implies a difference between the years 2021/2022 and 2023/2024 in terms of the st/state variable, 
  # however when I attempted to account for this I discovered that it was not the case on my end. 
  # If you go back through my commits you can see when I figured this out.
  
  # Anyway, let's start by getting a list of the codes for the geography levels.

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
  
  # I'm running this here to make sure it doesn't impact all that follows.
  
  geogCodes <- grabCodes()
  
 # Moving on, let's check to make sure that the inputted geography level matches the setting.

  matchGeogSubsets <- function(geog,gsubset){if(geog == "region"){
    options<-geogCodes$regionCodes
    options[[6]]<-"All"}
    if(geog == "division"){
      options<-geogCodes$divisionCodes
      options[[11]]<-"All"}
    if(geog == "state"){
      options<-geogCodes$stateCodes
      options[[53]]<-"All"}
    
    if(!gsubset %in% options){
      stop(paste0("The ", geog," code selected does not exist, choose from: ",paste(options,collapse=",")))}}
  
  # Now we need to account for the "all" option. I think you can do this with the asterisk, but in my fevered attempts to fix the st/state issue, I changed it to this method, and it works, so I'm not changing it again.

  allowForAll <- function(geog,gsubset){
    if (gsubset == "All"){
      if(geog == "region"){gsubset <-allRegions}
      else if (geog == "division"){gsubset <- allDivisions}
      else if (geog == "state"){gsubset <- allStates}
    }
    else {gsubset <- gsubset}
  }
  
  # Now we'll set up the string for the geography settings.

  setGeogString <- function(geog,gsubset){
    geogString <- paste0(geog,":",gsubset)
  }
  
  #
  
  allStates <- paste(geogCodes$stateCodes, collapse=",")
  allRegions <- paste(geogCodes$regionCodes, collapse=",")
  allDivisions <- paste(geogCodes$divisionCodes, collapse=",")
  
  numVars <- c(agep=agep, gasp=gasp,grpip=grpip,JWAP=JWAP,JWDP=JWDP,jwmnp=jwmnp)
  catVars <- c(sex=sex,fer=fer,hhl=hhl,sch=sch,schl=schl)
  
  matchInputsSimple(key,year,agep,gasp,grpip,JWAP,JWDP,jwmnp,sex,fer,hhl,sch,schl,geog,gsubset)
  
  atLeastOne(numVars,catVars)
  
  varString <- setVarString(numVars,catVars)
  
  matchGeogSubsets(geog,gsubset)
  
  gsubset<-allowForAll(geog,gsubset)
  
  geogString <- setGeogString(geog,gsubset)
  
  censusUrl <- paste0(
    "https://api.census.gov/data/", year, "/acs/acs1/pums",
    "?get=", varString,
    "&for=", geogString,
    "&key=", key
  )
  
  request <- GET(
    url=censusUrl
  )
  rawStats <- content(request, "text", encoding="UTF-8")
  parsedStats <- fromJSON(rawStats)
  censusPull <- as_tibble(parsedStats)
  colnames(censusPull) <- censusPull[1, ]
  censusPull <- censusPull[-1, , drop=FALSE]
  
  # Here I'm joining the output with my JWAPVals tibble, so that I can swap out the time codes for the actual times
  if ("JWAP" %in% names(censusPull)) {
    censusPull <- censusPull |>
      left_join(JWAPVals, by="JWAP") |> 
      mutate(JWAP=times) |> # here's where I do that swap
      select(-times)}    # now I'm removing the leftovers of the join
  
  # and now I repeat the process for JWDP, which unfortunately has different values from JWAP
  if ("JWDP" %in% names(censusPull)) {
    censusPull <- censusPull |>
      left_join(JWDPVals, by="JWDP") |>
      mutate(JWDP=times) |>
      select(-times)}
  
  censusPull <- censusPull |>
    mutate(across(any_of(c("PWGTP", "GASP", "AGEP", "GRPIP", "JWMNP")), as.double))
  
  yearKey <- paste0("y",year)
  yearMeta <- varLists[[yearKey]]
  
  selectedCats <- names(catVars)[catVars]
  
  for (cat_var in selectedCats) {
    cat_upper <- toupper(cat_var)
    
    meta_key <- if (cat_upper %in% names(yearMeta)) cat_upper else if (cat_var %in% names(yearMeta)) cat_var else NULL
    
    if (cat_upper %in% names(censusPull) && !is.null(meta_key)) {
      valItems <- yearMeta[[meta_key]]$values$item
      if (!is.null(valItems)) {
        levelsVec <- as.character(names(valItems))
        labelsVec <- unname(unlist(valItems))
        
        censusPull <- censusPull |>
          mutate(!!cat_upper := factor(.data[[cat_upper]], levels=levelsVec, labels=labelsVec))
      }
    }
  }
  
  class(censusPull) <- c("census", class(censusPull))
  
  return(censusPull)
  
}