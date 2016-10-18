stdchar <- function(x){

  #check the data type
  types = c("character","factor")
  if(!(class(x) %in% types)){
    return(x)
  }

  #now we clean
  else{
    newvec = as.character(x)
    #trim white space
    newvec <- trimws(newvec,which = "both")
    #nenewvect we'll replace any internal blanks with dashes
    newvec <- gsub(pattern=" +",replacement = "-",x=newvec)
    newvec <- gsub(pattern=",+",replacement = "",x=newvec)
    newvec <- gsub(pattern="--",replacement = "-",x=newvec)

    #make sure everything is capitalized
    newvec <- as.character(toupper(newvec))

    return(newvec)

  }
}
