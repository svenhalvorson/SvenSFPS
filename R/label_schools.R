# Time to re-write this function.
# We want the functionality to be basically the same but it was silly
# to accept a data frame as an argument and return the data frame
# with the names I chose. Let's make it take in a schooltor or column
# and return the matching column of new values
# It also should cache the matches it finds so that it's faster than re-doing
# all the regular expression matching

# ARGUMENTS
# school : the values to be transformed
# to : the form to convert to
# from : the form school is in
# current : should current schools be used only (DVMS -> MMS)

label_schools <- function(school, to = "abbr", from = NA, current = TRUE){

  library("dplyr")


  # SAFEGAURDS --------------------------------------------------------------
  # school should be an atomic schooltor
  # from should be one of abbr, name, or num.
  # current should be logical

  if(!is.atomic(school)){
    stop("school must be atomic")
  }

  if(!(to %in% c("abbr", "name", "num"))){
    stop("to must be one of abbr, name, or num)")
  }
  if(!is.na(from)){
    if(!(from %in% c("abbr", "name", "num"))){
      stop("from must be one of abbr, name, or num)")
    }
  }
  if(!is.atomic(current) | length(current)>1 | !(current %in% c(TRUE, FALSE))){
    stop("current must be TRUE or FALSE")
  }
  message(paste0(sum(is.na(school) | school == ""), " blank entries detected in school"))

  # Make a copy of school to play with
  school = suppressWarnings(as.character(school))
  # and invoke the conversion table and set it up for whatever value of current
  ref = convtable
  if(!current){
    convtable = select(convtable, -num2, -abbr2)
  }

  # DETECT FROM -------------------------------------------------------------
  # determine which form school is in
  if(is.na(from)){
    # detect num
    if(1.1 * sum(is.na(school)) >= sum(is.na(suppressWarnings(as.numeric(school))))){
      from = "num"
    }
    else if(mean(nchar(school))>3 & mean(nchar(school)) <= 4){
      from = "abbr"
    }
    else if(mean(nchar(school))>4){
      from = "name"
    }
    if(is.na(from)){
      stop("Unable to determine from argument. Specify manually")
    }


  }

  # Let's condense the code by using this function to retrieve output:
  output <- function(to, current){
    suffix = ""
    if(current){
      suffix = "2"
    }
    return(school[,paste0(to,suffix)])
  }

  # from == "num" -------------------------------------------------------------
  if(from == "num"){
    school = data.frame(num = as.numeric(school))
    school = left_join(school, convtable, by = "num")

    return(output(to, current))

  }


  # from == "abbr" ----------------------------------------------------------
  if(from == "abbr"){

    school = data.frame(abbr = school, stringsAsFactors = FALSE)
    school = left_join(school, convtable, by = "abbr")

    return(output(to, current))
  }

  # from == "name" ----------------------------------------------------------
  # alright, now here's the hard part
  #let's first define a table to store the matches we find
  if(from == "name"){
    matches = matrix(data = NA, nrow = length(unique(school)), ncol = 2)
    matches[,1] = unique(school)

    # now this function will find the corresponding entry on the conv table
    infrags <- function(name, current){
      for(i in 1:length(ref$name)){
        if(grepl(pattern = convtable$frag1[i], x = name, ignore.case = TRUE) |
           grepl(pattern = convtable$frag2[i], x = name, ignore.case = TRUE) |
           grepl(pattern = convtable$frag3[i], x = name, ignore.case = TRUE) |
           grepl(pattern = convtable$frag4[i], x = name, ignore.case = TRUE) |
           grepl(pattern = convtable$frag5[i], x = name, ignore.case = TRUE)){

          return(convtable$name[i])
        }
      }
      return(NA)

    }




    # Now we go through the matches matrix and fill the second column with infrags
    matches[,2] = apply(X = as.matrix(matches[,1]), MARGIN = 1, FUN = infrags, current = current)
    # now we merge onto school
    colnames(matches) = c("School","name")
    school = data.frame(School = school, stringsAsFactors = FALSE)
    school = suppressMessages(left_join(school, as.data.frame(matches, stringsAsFactors = FALSE)))

    school = suppressMessages(left_join(school,ref))
    out = output(to,current)
    return(out)
  }
}




