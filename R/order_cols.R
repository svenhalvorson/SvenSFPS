# We're here to write a function that reorders columns of a data frame
# I would really like to do this with NSE as it would be cool to have it
# look and interact with dplyr functions cleanly but I'm not sure I'm
# at that level yet. Maybe we'll re-write it soon!

# The arguments will be:
#   df : a data frame to be re-ordered
#   first : character vector of columns to be brought to the front
#   last : character vector of columns to be brought to the back

order_cols <- function(df, first = NA, last = NA){

  library("dplyr")
  # SAFEGAURDS --------------------------------------------------------------

  # Checks I'm thinking about right now:
  #  1. df should be a data frame
  #  2. at least one of c(first, last) should not be NA
  #  3. elements of first and last should be in colnames(df)
  #  4. first and last should not overlap

  # I think these functions will help. We want to allow for either first/last to be
  # character vector or single NA
  simple_NA = function(x){
    if(length(x) != 1){
      return(FALSE)
    }
    if(is.atomic(x) & is.na(x)){
      return(TRUE)
    }
    else{
      FALSE
    }
  }
  chk <- function(x){
    simple_NA(x) | (is.atomic(x) & is.character(x) & length(x) > 0)
  }

  if(!"data.frame" %in% class(df)){
    stop("df must be a data.frame")
  }

  if(!suppressWarnings(chk(first)) | !suppressWarnings(chk(last))){
    stop("First and last must be character vectors or NA")
  }

  if((length(first) == 1 & sum(is.na(first))>0) & (length(last) == 1 & sum(is.na(last))>0)){
    stop("At least one of c(first, last) must be specified")
  }

  if((length(first)>1 & sum(is.na(first))>0) | (length(last)>1 & sum(is.na(last))>0)){
    stop("first and last may not be named and contain NAs")
  }

  if(!simple_NA(first) & mean(first %in% colnames(df)) < 1){
    missing = setdiff(first,colnames(df))
    stop(paste0("Values in first not found among colnames(df) : ",paste(missing, collapse = ", ")))
  }

  if(!simple_NA(last) & mean(last %in% colnames(df)) < 1){
    missing = setdiff(last,colnames(df))
    stop(paste0("Values in last not found among colnames(df) : ",paste(missing, collapse = ", ")))
  }

  if(length(setdiff(first, last))<length(first) | length(setdiff(last, first))<length(last)){

    stop("first and last may not overlap")
  }

  # Separate df by columns --------------------------------------------------

  # Now we'll just pick apart df by first last and middle and then glue together
  extra = setdiff(setdiff(colnames(df),first),last)
  # if all columns are specified by first and last
  if(length(extra)==0){
    if(simple_NA(first)){
      df = df[last]
      return(df)
    }
    if(simple_NA(last)){
      df = df[first]
      return(df)
    }
    else{
      df = bind_cols(df[first],df[last])
      return(df)
    }
  }

  # If there are leftover columns
  else if(simple_NA(first)){
    df = bind_cols(df[extra], df[last])
    return(df)
  }
  else if(simple_NA(last)){
    df = bind_cols(df[first],df[extra])
    return(df)
  }
  # if both are specified and there are leftover columns
  else{

    df = bind_cols(df[first], df[extra], df[last])
    return(df)
  }
}

