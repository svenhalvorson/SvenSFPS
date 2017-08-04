# We're back for another exciting custom function.
# Many of the problems I encounter while doing data analysis come from
# not having a grasp on what the data looks like. Often duplicates,
# missing data, and unexpected data types foul me up. Let's make a function that
# will quickly let me see some diagnostics for a data frame.

# take in a data frame and produce a summary data frame

diagnostic <- function(df, blank = TRUE){
  library("lubridate")
  # safegaurds --------------------------------------------------------------
  if(!"data.frame" %in% class(df)){
    stop("df must be a data frame")
  }

  if(ncol(df)<1 | nrow(df)<1){
    stop("df must have at least one row and one column")
  }

  # Measures ----------------------------------------------------------------

  # start the output data frame
  output <- data.frame("col_#" = 1:ncol(df),
                       Variable = colnames(df))

  # Definitely want the data type. Unfortunately, mode() will report 'integer'
  # for a factor but detecting this difference is important to me
  mode2 <- function(x){
    if(is.factor(x)) return("factor")
    if(is.Date(x)) return("date")
    else mode(x)
  }
  types = lapply(X = df, FUN = mode2)
  output[["mode"]] = unlist(types)

  # We want % unique, number of unique values, and percent NA
  nunique = lapply(X = df, FUN = function(x){length(unique(x))})
  output[["#_Unique"]] = unlist(nunique)
  rows = nrow(df)
  punique <- function(x){
    tab = table(table(x))
    if("1" %in% names(tab)){
      return(round(100*tab[["1"]]/rows,1))
    }
    else{
      return(0)
    }
  }
  output[["%_Unique"]] = unlist(lapply(X = df, FUN = punique))

  # Percent NA will also include "" if the blank parameter is TRUE
  new_NA = function(x){
    if(blank & mode(x) == "character"){
      ret = sum(is.na(x) | x == "")/rows
    }
    else{
      ret = sum(is.na(x))/rows}
    round(100*ret,1)
  }
  output[["%_Missing"]] = unlist(lapply(X = df, FUN = new_NA))

  output
}

# random_blanks <- function(x){
#   n_blanks = sample(x = 0:floor(length(x)/4),size = 1)
#   if(n_blanks != 0){
#     indices = sample(x = 1:length(x), size = n_blanks, replace = FALSE)
#     x[indices] = NA
#   }
#   x
# }
# rr = as.data.frame(lapply(X = mtcars, FUN = random_blanks))
#


