# So one thing I often end up doing is trying to pick out a
# set of particular observations from a data set that don't
# have a clear theme. Often I end up writing a lot of regular
# expression statements. The aim of this function is to allow me
# to supply two character vectors and pick out all the instances in
# one where one of the strings in the second is regex match
# I'll also include a parameter to determine if all the elements
# of the patterns list have to be in any

grepl_multi <- function(patterns, x, all = FALSE, ignore.case = FALSE,
                        perl = FALSE, fixed = FALSE, useBytes = FALSE){

  # SAFEGAURDS --------------------------------------------------------------

  # I think we'll just rely on the errors that grepl will use for the most part
  # but we do need to worry that all is logical and that patterns is character vector

  if(!is.logical(all) | length(all) != 1){
    stop("all arugment must be either TRUE or FALSE")
  }
  if(!is.vector(patterns) | !is.character(patterns)){
    stop("patterns argument must be a character vector")
  }

  # ACTION ------------------------------------------------------------------

  # Now we'll loop through the values of patterns and perform a grepl and then
  # apply min/max to the data frame formed of the grepls
  patterns = as.list(patterns)
  matchmat = lapply(X = patterns, FUN = grepl, x = x, ignore.case = ignore.case,
                    perl = perl, fixed = fixed, useBytes = useBytes)
  matchmat = as.data.frame(matchmat)

  # Next we're going to take a min or a max across the rows of matchmat
  # depending on the all argument
  if(all == TRUE){
    return(as.logical(apply(X = matchmat, MARGIN = 1, FUN = min)))
  }
  else{
    return(as.logical(apply(X = matchmat, MARGIN = 1, FUN = max)))
  }


}

