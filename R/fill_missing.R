# The goal of this function is to take one vector and fill it with the values
# of another vector in the case that the first is NA or ""

fill_missing <- function(x, with){

  if(!is.atomic(x) & !is.list(x)){
    stop("x be a list or atomic")
  }
  if(!is.atomic(with) & !is.list(with)){
    stop("with be a list or atomic")
  }

  if(typeof(x) == "character"){

    x[is.na(x) | x == ""] = with[is.na(x) | x == ""]
  }
  else{
    x[is.na(x)] = with[is.na(x)]
  }
  return(x)
}
