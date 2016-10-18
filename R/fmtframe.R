
#we're going to try and write a little function that will help us with the problem of
#factor levels not accepting new value that aren't in their levels

fmt.frame <- function(df, to = "character", form = "data.frame"){

  #safegaurds
  if(!(class(df) %in% c("data.frame","matrix"))){
    stop("df must be a data frame or matrix")
  }

  if(!(class(to) %in% c("character","factor"))){
    stop("to paramater must be character or factor")
  }

  #let's take care of the case when we want a matrix because it will
  #need to force character type because matrices cannot have different
  #data types in the columns
  if(form == "matrix"){
    df = as.matrix(df)
    return(df)
  }


  #now if we've selected data frame it's trickier
  #function to replace as.character to stop it from converting numerics
  to.char <- function(x){
    if(!(class(x) %in% c("numeric","integer"))){
      x = as.character(x)
    }
    return(x)
  }

  #if character, we turn all the factor variables into character
  if(to == "character"){

    if(form == "data.frame"){
      #make a data frame to store the columns as we loop
      bogus = rep(NA,times = nrow(df))
      temp = data.frame(bogus)
      for(i in 1:ncol(df)){
        temp = cbind(temp,to.char(df[,i]))

      }
    }
    df = as.data.frame(temp, stringsAsFactors = FALSE)
    #cut that dummy column off
    df = select(df,-bogus)
    return(df)
  }

  to.fact <- function(x){
    if(!(class(x) %in% c("numeric","integer"))){
      x = as.factor(x)
    }

  }

  #if we want factors, we
  if(to == "factor"){
    df = as.data.frame(apply(X = df, MARGIN = 2, FUN = to.fact))



    return(df)
  }


}
