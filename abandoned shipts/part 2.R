
  dawg<-apiHarmer(key=censusKey,year=2021,
                         agep=T,gasp=T,grpip=F,JWAP=T,JWDP=T,jwmnp=F,
                         sex=T,fer=T,hhl=F,sch=F,schl=F,geog="state",gsubset="02")
  numerics <- c("PWGTP","AGEP","GASP","JWDP","DUMMY")
  categorics <- c("SEX","HHL","FER","DUMMY")
  

  matchInputs2 <- function(census,numerics,categorics){
    if(!class(census)[1] == "census"){
      stop("Census object must be of the Census class.")
    }
    if(!is.atomic(numerics)){
      stop("The numeric variables must be in a atomic vector")
    }
    if(!is.atomic(categorics)){
      stop("The categorical variables must be in a atomic vector")
    }
    if (!all(sapply(numerics, is.character))){
      stop("The items of the numeric variable list must be the string names of the variables.")
    }
    if (!all(sapply(categorics, is.character))){
      stop("The items of the cateogrical variable list must be the string names of the variables.")
    }
    
  }
  matchInputs2(census,numerics,categorics)
  
  varsInCensus <- function(census,numerics,categorics){
    potVars <- colnames(census)
    allPotVars <- c("AGEP","GASP","GRPIP","JWMNP","SEX","FER","HHL","SCH","SCHL")
    potVars <- potVars[potVars %in% allPotVars]
    numerics <- numerics[numerics %in% potVars]
    categorics <- categorics[categorics %in% potVars]
    if(is_empty(numerics) & is_empty(categorics)){
      stop("No valid variables selected")
    }
    endVars <- list("num"=numerics,"cat"=categorics)
    return(endVars)}

  endVars<-varsInCensus(census,numerics,categorics)  
  numerics<-endVars$num
  categorics<-endVars$cat
  
  numCensus <- select(census,all_of(numerics))
  catCensus <- select(census,all_of(categorics))
  
  numSum <- data.frame(
    variable = names(numCensus),
    mean = sapply(numCensus, mean, na.rm = TRUE),
    sd = sapply(numCensus, sd, na.rm = TRUE),
    row.names = NULL)
  
  catSum <- do.call(rbind, lapply(names(catCensus), 
                                  function(var) {
                                    counts <- table(catCensus[[var]], useNA = "ifany")
                                    data.frame(
                                      variable = var,
                                      level = names(counts),
                                      count = as.vector(counts),
                                      row.names = NULL)}))
  
  finSum <- list(numSum,catSum)
  