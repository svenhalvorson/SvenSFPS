
#function to apply the double rowwise to a couple of columns
regex.cols <- function(a,b,ignore.case = FALSE){

  #verify that they're both factors or characters
  if(!(class(a) %in% c("factor","character") & class(b) %in% c("factor","character"))){
     stop("a and b must be factors or characters")
   }

  #verify that both vectors are the same length
  if(!(length(a)==length(b))){
    stop("a and b must be the same length")
  }

  mat = as.data.frame(cbind(a,b))

  ret = apply(X = mat, MARGIN = 1, FUN = double.regex(mat[,1],mat[,2]))


}
