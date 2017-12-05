# We're here to write a function that reorders columns of a data frame
# I'm improving on the original function now by adding NSE.
# As a result, we have to change the arguments a bit

# The arguments will be:
#   df : a data frame to be re-ordered
#   front: logical saying, whether to add the specified colomns to the front or back of df

order_cols <- function(df, ..., front = TRUE){

  library("dplyr")

  # SAFEGAURDS --------------------------------------------------------------

  # Checks I'm thinking about right now:
  #  1. df should be a data frame
  if(!is.data.frame(df)){
    stop("df must be a data.frame")
  }
  #  2. elements of first and last should be in colnames(df)
  cols <- as.character(substitute(list(...))[-1])
  if(min(cols %in% colnames(df)) == 0){
    missing_cols = cols[!cols %in% colnames(df)]
    stop(paste("Columns not found in df:", paste(missing_cols, collapse = ", ")))
  }

  # WORK --------------------------------------------------------------------
  # Now we have to get the leftover columns
  leftovers = setdiff(colnames(df), cols)

  # if every column was specified
  if(length(leftovers) == 0){
    return(df[cols])
  }

  # if there are leftovers we treat them differently depending on front
  if(front){cols = c(cols, leftovers)}
  else {cols = c(leftovers, cols)}
  df[cols]

}

