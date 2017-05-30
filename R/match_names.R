# We're back to for another attempt at the name matcher to find
# duplicates in the SIS

# The arguments here are 
# df1, df2 : the two data frames to be merged
# fixed : a vector of the column names thought to be 'true'. 
# partials : vector of columns from df1 and df2 to be fuzzy matched
library("tidyverse")
match_names <- function(df1, df2, fixed = NA, partials = NA,
                        edits = TRUE, regex = TRUE){
  
  # Safegaurds --------------------------------------------------------------
  
  # Both df1 and df2 must be data frames
  if(!("data.frame" %in% class(df1)) | !("data.frame" %in% class(df2))){
    stop("df1 and df2 must be data frames.")
  }
  
  # if fixed or partials are not defined
  if(sum(is.na(fixed))>0){
    stop("fixed argument not defined")
  }
  if(sum(is.na(partials))>0){
    stop("partials argument not defined")
  }
  # if fixed isn't in both data frames columns
  if(length(setdiff(fixed, colnames(df1))) > 0){
    stop("Values from fixed not found in df1")
  }
  if(length(setdiff(fixed, colnames(df2))) > 0){
    stop("Values from fixed not found in df2")
  }
  # if partial isn't in both data frames columns
  if(length(setdiff(partials, colnames(df1))) > 0){
    stop("Values from partial not found in df1")
  }
  if(length(setdiff(partials, colnames(df2))) > 0){
    stop("Values from partial not found in df2")
  }
  
  # Maybe the partial and fixed arguemnts overlap
  if(length(intersect(partials, fixed)) > 0){
    stop("partials and fixed arguments overlap")
  }
  


  # Merge & Exact -------------------------------------------------------------------

  merged <- merge(df1, df2, by = fixed)
  
  # Now we want a way to reference the paired columns formed by partials
  colMat = matrix(nrow = length(partials), ncol = 2)
  colMat[,1] = paste0(partials,".x")
  colMat[,2] = paste0(partials,".y")

  # And then we'll create the matrix to store agreement between the partials columns
  matchMat = matrix(nrow = nrow(merged), ncol = length(partials))
  colnames(matchMat) = paste0(partials,"_match")
  
  for(i in 1:length(partials)){
    # subset down to the columns in that row of colMat and ask if those
    # columns are equal
    temp = merged[,colMat[i,]]
    matchMat[,i] = (temp[,1] == temp[,2])
  }
  
  # Now tack on the exact matches column
  merged$exact_match = as.logical(apply(X = matchMat, MARGIN = 1, FUN = min))
  rm(matchMat)
  
  # Edit Distances ---------------------------------------------------------
  if(edits){
    # Now we'll use this function to compute the mean number of characters 
    # across the two columns
    mean_char <- function(x){
      mean(nchar(x))
    }
    
    for(i in 1:length(partials)){
      # compute the approximate string distance
      col = paste0(partials[i],"_count")
      # browser()
      # merged[,col] = diag(adist(x = merged[,colMat[i,1]],
      #                           y = merged[,colMat[i,2]])
      # )
      merged[,col] = NA
      for(j in 1:nrow(merged)){
        merged[j,col] = adist(x = merged[j,colMat[i,1]],
                              y = merged[j,colMat[i,2]])
      }
      # Compute the average number of characters between both columns
      characters = apply(X = merged[,colMat[i,]], MARGIN = 1, FUN = mean_char)
      col2 = paste0(partials[i],"_prop")
      merged[,col2] = round(merged[,col]/characters,3)
    }
    # Lastly, we'll compute mean counts and percents if there were more than
    # one partial
    if(length(partials)>1){
      merged[,"mean_count"] = apply(X = merged[,paste0(partials, "_count")],
                                    MARGIN = 1,
                                    FUN = mean)
      merged[,"mean_prop"] = apply(X = merged[,paste0(partials, "_prop")],
                                   MARGIN = 1,
                                   FUN = mean)
    }
  }
  
  # Regex -------------------------------------------------------------------
  
  # We'll use the strategy we used in the old match.names here as well. The idea is to cycle 
  # throug the rows of the two parial columns and see if either segment of the variables 
  # are subsets of eachother
  sub_name <- function(x,y){
    verdict = FALSE
    std_x = unlist(strsplit(x = stdchar(x), split = "-"))
    std_y = unlist(strsplit(x = stdchar(y), split = "-"))

    for(nam in std_x){
      if(is.na(nam)){
        next()
      }
      if(sum(grepl(pattern = nam, x = std_y)) > 0){
        return(TRUE)
      }
    }
    verdict
   }
  
  # Now we'll loop through the partials
  for(i in 1:length(partials)){
    # Take that part of the merged frame
    subset = merged[,colMat[i,]]
    merged[,paste0(partials[i],"_regex")] = NA
    # Since the subnames function is not symmetric, let's take a max of both ways.
    for(j in 1:nrow(merged)){
      merged[j,paste0(partials[i],"_regex")] = max(sub_name(subset[j,1], subset[j,2]),
                                                   sub_name(subset[j,2], subset[j,1]))
    }
    
  }
  
  return(merged)
}




