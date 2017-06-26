#okie we're gonna try to make a STATA style merge for R
#since its merge function doesn't give very good description of what happened

# main points are:
# We should print the following things:
#  number of items matched
#  number of items not matched from left and right
#  total number of elements in new data set

#Also want to have a column that notes which of the three
#categories that obs is in


#wrap it up

#let's just start with the arguments from 'merge'
smerge <- function(x, y, by = intersect(names(x), names(y)),
                   by.x = by, by.y = by, all = TRUE, all.x = all, all.y = all,
                   sort = TRUE, suffixes = c(".x",".y"),
                   detail = TRUE){
  suppressMessages(library("dplyr"))
  suppressMessages(library("stringr"))

  #so now we're gonna attach indicators to help us count and mark the two sets
  #pick really weird names thare are unlikely to be in the data frame
  x$LHS__ = -1
  y$RHS__ = 1

  mer = merge(x, y, by,by.x, by.y, all, all.x, all.y, sort, suffixes)

  #The NA's in the LHS and RHS columns now represent the number of unmatched elements
  onlyx = sum(is.na(mer$RHS_))
  onlyy = sum(is.na(mer$LHS_))
  # onlyx_per = round(100*onlyx/nrow(x), digits = 0)
  # onlyy_per = round(100*onlyy/nrow(y), digits = 0)

  #now fill in zeroes for the pieces that didnt match
  mer$LHS__[is.na(mer$LHS__)] = 0
  mer$RHS__[is.na(mer$RHS__)] = 0
  mer = mutate(mer,merge_ = RHS__ + LHS__)

  #The total notmatched and not matched
  notmatched = onlyx + onlyy
  matches = nrow(mer) - notmatched

  #formatting this output might be a little tough, here's a standard line of 32 characters
  line = "--------------------------------"

  #now make labels for interpretation
  mer$merge_ = factor(mer$merge)
  mer = select(mer,-LHS__,-RHS__)
  
  # I want to add in some additional output here and control it with the parameter detail
  extra_text = ""
  if(detail){
    
    # Compute the % unique for each half
    unique_x = round(100*nrow(unique(x[,by]))/nrow(x), digits = 1)
    unique_y = round(100*nrow(unique(y[,by]))/nrow(y), digits = 1)
    unique_x = as.character(unique_x)
    unique_y = as.character(unique_y)
    if(!grepl(pattern = "\\.", x = unique_x)){
      unique_x = paste0(unique_x,".0")
    }
    if(!grepl(pattern = "\\.", x = unique_y)){
      unique_y = paste0(unique_y,".0")
    }
    
    # # I also wanna attach average and max number of connections for each side
    # # to get a feel for how the match went and if there are any outliers in
    # # how many connections were made
    # browser()
    # temp = mer[mer$merge_ == 0,]
    # # We'll extract the unique values from temp and inner join with x and y for the denominator
    # unique_matches = unique(temp[,by])
    # matched_x = nrow(inner_join(unique_matches,x, by = by))
    # matched_y = nrow(inner_join(unique_matches,y, by = by))
    # ratio_x = round(nrow(temp)/matched_x, 1)
    # ratio_y = round(nrow(temp)/matched_y, 1)
    
    extra_text = paste0("\nDetail:\n% of unique rows in x:",
                        str_dup(" ",times=32-nchar("% of unique rows in x:")-nchar(unique_x)),
                        unique_x,
                        "\n% of unique rows in y:",
                        str_dup(" ",times=32-nchar("% of unique rows in y:")-nchar(unique_y)),
                        unique_y
                        # "\nAvg. # matches in x:",str_dup(" ",times=32-nchar("Avg. # matches in x:")-nchar(ratio_x)), 
                        # "\nAvg. # matches in y:",str_dup(" ",times=32-nchar("Avg. # matches in x:")-nchar(ratio_y))
                        
                        
                        )
  }
  writeLines(
    paste0("Merging by",paste(by,collapse=", "),
    "\n\nResults\t\t\t  # of obs\n",
    line,
    "\nNot matched",str_dup(" ",times=32-nchar("Not matched")-nchar(notmatched)),notmatched,
    "\n   LHS Only",str_dup(" ",times=29-nchar("LHS Only")-nchar(onlyx)),onlyx,"  _merge == -1",
    "\n   RHS Only",str_dup(" ",times=29-nchar("RHS Only")-nchar(onlyy)),onlyy,"  _merge ==  1",
    "\n\nMatched",str_dup(" ",times=32-nchar("Matched")-nchar(matches)),matches,"  _merge ==  0","\n",line,extra_text)
    )

  return(mer)
}



# test1 = as.data.frame(c("A","B","C","D"))
# colnames(test1) = "letter"
# test1$num = 1:4
# test1$pork = c("bacon","carnitas","ham","chops")
# 
# test2 = as.data.frame(c("B","C","E","A"))
# colnames(test2) = "letter"
# test2$num = c(2,3,4,5)
# test2$chicken = c("wings","sandwich","fried","drumstick")