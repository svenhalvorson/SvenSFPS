# a little function to do the two directional regex
# both arguments should be single characters
double.regex <- function(a, b, ignore.case = FALSE){

  #protect against NA
  if(is.na(a) | is.na(b)){
    return(FALSE)
  }

  # must be atomic
  if(!is.character(a) | !is.character(b)){
    stop("a and b must be characters")
  }

  #can only take in length 1
  if(length(a) != 1 | length(b) != 1){

    stop("Arguments must have length 1")
  }

  #protect against NA or ""
  rejects = c("",NA)
  if(nchar(a)==0 | nchar(b)==0){
    judgement = FALSE
  }
  else if((a %in% rejects) | (b %in% rejects)){
    judgement = FALSE
  }
  else{
    judgement = (grepl(pattern = a,x = b,ignore.case=ignore.case) | grepl(pattern = b, x = a,ignore.case=ignore.case))
  }
  return(judgement)


}
