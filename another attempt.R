
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

matchIn <- function(key,year,agep,gasp,grpip,jwap,jwdp,jwmnp,sex,fer,hhl,schf,schl,geog,gsubset){
  if(!all(is.logical(c(agep,gasp,grpip,jwap,jwdp,jwmnp,sex,fer,hhl,schf,schl)))){
    stop("Each of the non-filtering variables takes a T/F value")
  }
  if(!is.character(key)){
    stop("The census key must be entered as a string")
  }
  geog <- tolower(geog)
  if(!geog %in% c("st","state","region","division")){
    stop("The geography options are st, state, region, and division.")
  }
  if(geog == "region"){
    if(!gsubset %in% geogCodes$regionCodes){
      stop(paste0("The region code selected does not exist, choose from: ",allRegions))}
  }
  if(geog == "division"){
    if(!gsubset %in% geogCodes$divisionCodes){
      stop(paste0("The division code selected does not exist, choose from: ",allDivisions))}
  }
  if(geog %in% c("st","state")){
    if(!gsubset %in% geogCodes$stateCodes){
      stop(paste0("The state code selected does not exist, choose from: ",allStates))
    }
  }
}

matchGCodes <- function(year,geog,gsubset){
  if(geog %in% c("st","state"))
}


apiHarmer <- function(key,year=2024,agep=T,gasp=F,grpip=F,jwap=F,jwdp=F,jwmnp=F,sex=T,fer=F,hhl=F,sch=F,schl=F,geog="region",gsubset="2"){
  # First: let's verify all of the inputs are correct
  if(!all(is.character(c(key,geog,))))
  
  
  
  # Next: let's create the helpers:

  
  # 
}
  

