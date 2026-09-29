
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
  matchInputs2(dawg,numerics,categorics)
  
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
    return(endVars)
    }

  endVars<-varsInCensus(dawg,numerics,categorics)  
  numerics<-endVars$num
  categorics<-endVars$cat
  
  numCensus <- select(dawg,all_of(numerics))
  catCensus <- select(dawg,all_of(categorics))
  
  numsum <- data.frame(
    variable = names(doog),
    mean     = sapply(doog, mean, na.rm = TRUE),
    sd       = sapply(doog, sd, na.rm = TRUE),
    row.names = NULL
  )
  
  
  
  