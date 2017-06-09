# Alright so I often encounter a scenarios where I want to fill 
# in values of a column with the last non-NA value above it
# so it should turn c(1,NA,NA,3,NA,2,NA to c(1,1,1,3,3,2,2)
# Often this happens with people's excel sheets having split cells
# string parameter denotes whether we want to treat "" like NA

fill_down <- function(x, string = FALSE){
  
  if(!is.atomic(x)){
    stop("x must be atomic")
  }
  if(!string){
    # Store the most recent non NA value and loop through the vector
    temp = NA
    for(i in 1:length(x)){
      
      # First case is the first entry is NA, just keep going
      if(is.na(temp) & is.na(x[i])){
        next()
      }
      
      # If we hit a real value, save it
      if(!is.na(x[i])){
        temp = x[i]
        next()
      }
      
      # Finally, if we have a value stored and x[i] is NA
      if(!is.na(temp) & is.na(x[i])){
        x[i] = temp
      }
      
    }
  }
  # If we do want to consider blans the same as NA:
  else{
    temp = NA
    for(i in 1:length(x)){
      #browser()
      # First case is the first entry is NA, just keep going
      if(is.na(temp) & ((is.na(x[i])) | x[i] == "")){
        next()
      }
      
      # If we hit a real value, save it
      if(!is.na(x[i]) & !(x[i] == "")){
        temp = x[i]
        next()
      }
      
      # Finally, if we have a value stored and x[i] is NA
      if((!is.na(temp) | temp == "") & (is.na(x[i]) | x[i] == "")){
        x[i] = temp
      }
      
    }
    
  }
  return(x)
}


