
#we're going to try and write a little function that will help us with the problem of
#factor levels not accepting new value that aren't in their levels


fmt.frame <- function(df, character = TRUE, matrix = FALSE){

  #safegaurds
  if(class(df) != "data.frame"){
    stop("df must be a data frame")
  }


  #let's take care of the case when we want a matrix because it will
  #need to force character type because matrices cannot have different
  #data types in the columns
  if(matrix){
    df = as.matrix(df)
    return(df)
  }

  #now if we've selected data frame it's trickier
  #function to replace as.character to stop it from converting numerics
  to.char <- function(x){
    if(!(class(x) %in% c("numeric","integer","double"))){
      x = as.character(x)
    }
    return(x)
  }

  to.fact <- function(x){
    if(!(class(x) %in% c("numeric","integer","double"))){
      x = factor(x)
    }
    return(x)
  }

  #Now apply them across the columns of df and return
  if(character){
    df = as.data.frame(lapply(X = df, FUN = to.char),stringsAsFactors = FALSE)
    return(df)
  }

  if(!character){
    df = as.data.frame(lapply(X = df, FUN = to.fact))
    return(df)
  }


}
