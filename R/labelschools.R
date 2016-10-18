#we're gonna write a script to give standardized names and numbers
#to the schools at SFPS
#going to call the output 'vec'

label.schools <- function(df, school = "school", from = "num", to = "abbr"){

  if(type(df) != "data.frame"){
    stop("df must be a data frame")
  }

  #check to see that parameters specified correctly
  if(!(from %in% c("num","abbr","name"))){
    stop("from argument must be in num, abbr, name")
  }

  if(!(to %in% c("num","abbr","name"))){
    stop("to argument must be in num, abbr, name")
  }

  if((from == "num" & to == "num") | (from == "abbr" & to == "abrr")){
    stop("from and to arguments should be different or both 'name'")
  }

  #check if there exactly one column matching the school argument
  if(sum(colnames(df) == school) == 0){
    stop("school argument not found among columns")
  }

  if(sum(colnames(df) == school) > 1){
    stop("multiple columns matching school argument")
  }



  #load the conversion table
  ref <- convtable

  #okay let's divide the work into sections based on the from argument because it's hardest

  #######
  # NUM #
  #######

  #assuming we have from == "num"
  if(from == "num"){

    #make sure argument is numeric and the columns match for merging
    df$num = as.numeric(df[,school])

    #cut out that old school code
    df[which(df[,"num"] == 174),school] = 11

    if(to == "abbr"){
      ref = select(ref,num,abbr)
      ref = rename(ref,school.abbr = abbr)
    }

    if(to == "name"){
      ref = select(ref,num,name)
      ref = rename(ref,school.name = num)
    }

    #now join to the reference table
    df = left_join(df, ref, by = "num")
    df = select(df,-num)
    return(df)
  }

  #######
  # ABBR#
  #######

  if(from == "abbr"){

    df$abbr = df[,school]

    #cut out that old school code
    df[which(df[,"abbr"] %in% c("ECRA","AFES")),"abbr"] = "ECCS"

    if(to == "num"){
      ref = select(ref,abbr,num)
      ref = rename(ref,school.num = num)
    }

    if(to == "name"){
      ref = select(ref,abbr,name)
      ref = rename(ref,school.name = name)
    }

    #now join to the reference table
    df = suppressWarnings(left_join(df, ref, by = "abbr"))
    df = select(df,-abbr)
    return(df)
  }

  #######
  # NAME#
  #######

  #okay now for the hard part. We gotta figure out how to do this framgenting match

  #let's write a function to do the name matching
  infrags <- function(nam){
    for(i in 1:nrow(convtable)){

      #we're creating a list of whether the character name matches the fragments
      matches = c(grepl(x = nam,convtable[i,"frag1"],ignore.case = TRUE),
                  grepl(x = nam,convtable[i,"frag2"],ignore.case = TRUE),
                  grepl(x = nam,convtable[i,"frag3"],ignore.case = TRUE),
                  grepl(x = nam,convtable[i,"frag4"],ignore.case = TRUE))
      #if it matches one or more fragments, we look up  that value in the convtable
      #and return the value from the 'to' column
      if(sum(matches)>0){
          return(convtable[i,to])
      }



    }
    return(NA)

  }




  if(from == "name"){
    #make a blank  vector to store the output
    df$tmp = NA

    #run through all the values in the schol variable, run the infrags function
    #store in tmp
    for(i in 1:nrow(df)){
     df$tmp[i] = infrags(df[i,school])
    }

    #rename based on the 'to' argument
    if(to == "num"){
      df = rename(df,school.num = tmp)
    }
    if(to == "abbr"){
      df = rename(df,school.abbr = tmp)
    }
    if(to == "name"){
      df = rename(df,school.name = tmp)
    }


    return(df)
  }

}
