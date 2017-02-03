#we're gonna write a script to give standardized names and numbers
#to the schools at SFPS
#going to call the output 'vec'

label.schools <- function(df, school = "school", to = "abbr", from = NA, current = FALSE){

  library("dplyr")

  if(!is.data.frame(df)){
    stop("df must be a data frame")
  }

  #check to see that parameters specified correctly
  if(!(from %in% c("num","abbr","name", NA))){
    stop("from argument must be num, abbr, name, or left blank")
  }

  if(!(to %in% c("num","abbr","name"))){
    stop("to argument must be in num, abbr, name")
  }

  if(!is.na(from)){
    if((from == "num" & to == "num") | (from == "abbr" & to == "abrr")){
      stop("from and to arguments should be different or both 'name'")
    }
  }

  #check if there exactly one column matching the school argument
  if(sum(colnames(df) == school) == 0){
    stop("school argument not found among columns")
  }

  if(sum(colnames(df) == school) > 1){
    stop("multiple columns matching school argument")
  }

  #okay we're gonna try a different approach
  #let's making a copy of the school variable and use it as our working vector
  #and take only the non missing
  vec = as.character(df[!is.na(df[,school]) & df[,school] != "",school])

  #now try to detect the type of the from argument
  if(is.na(from)){
    #maybe it's clearly numeric, I guess we could accept some null values
    #but maybe if <20% are NA when coerced we would say it's probably numeric
    numtest = suppressWarnings(as.numeric(vec))
    likelynum = sum(is.na(numtest))/length(numtest) < .2
    if(likelynum){
      from = "num"
    }
  }

  #detect 4 character code
  if(is.na(from)){
    #let's try to work off the mean number of characters being between 3 and 4
    numchar = nchar(vec)

    if((mean(numchar)>3) & (mean(numchar)<=4)){
      from = "abbr"
    }


  }

  #detect long name form
  if(is.na(from)){
    #probably the name average name should be longer than 4 characters
    numchar = nchar(vec)
    if(mean(numchar)>4){
      from = "name"
    }
  }

  #if it's still NA let's stop
  if(is.na(from)){
    stop("Unable to detect unspecified 'from' argument")
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
    df$num = df[,school]
    if(is.character(df$num)){
      df$num = as.numeric(df$num)
    }
    if(is.factor(df$num)){
      df$num = as.numeric(as.character(df$num))
    }

    #cut out that old school code
    df$num[which(df$num == 174)] = 11

    if(to == "abbr"){
      ref = select(ref,num,abbr)
      ref = rename(ref,school.abbr = abbr)
    }

    if(to == "name"){
      ref = select(ref,num,name)
      ref = rename(ref,school.name = name)
    }

    #now join to the reference table
    df = suppressWarnings(left_join(df, ref, by = "num"))
    df = select(df,-num)
  }

  #######
  # ABBR#
  #######

  if(from == "abbr"){

    df$abbr = as.character(df[,school])

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
  }

  #######
  # NAME#
  #######
  #progress! Now we just gotta fix this part
  #okay now for the hard part. We gotta figure out how to do this framgenting match

  #let's write a function to do the name matching
  infrags2 <- function(nam,type){

    #loop through the rows of convtable
    for(i in 1:nrow(convtable)){

      #loop through the fragments
      for(j in c("frag1","frag2","frag3","frag4")){

        #check if the input or the fragment are subsets of eachother
        if(double.regex(nam,convtable[i,j],ignore.case = TRUE)){
          return(convtable[i,type])
        }
      }
    }
    #if we don't hit return a NA
    return(NA)

  }



  if(from == "name"){
    #make a blank  vector to store the output
    df$tmp = ""


    #run through all the values in the schol variable, run the infrags function
    #store in tmp
    for(i in 1:nrow(df)){
     df$tmp[i] = infrags2(vec[i],type = to)

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




  }

  #Now we're gonna tag on another option to return a vector saying if the school is one of our 'normal'
    #schools. If current = TRUE, we'll return an extra vector

    active.schools.num = c(141 ,54 ,12 ,8 ,24 ,33 ,20 ,188 ,5 ,22 ,146 ,9 ,7 ,
                186 ,11 ,166 ,135 ,99 ,169 ,57 ,70 ,173 ,145 ,170 ,
                168 ,110 ,100 ,23 ,52 ,143 ,165 ,130 ,34 ,160 ,78 ,
                176 ,53,7)

    active.schools.abbr = c("ABCS","ACCS","ALHS","AMES","ATC","ATES","CAHS",
                         "CAMS","CCES","CGES","CHES","DC","DRS","DVMS",
                         "ECCS","ECO","EDCS","EJES","ENHS","GOCS","KEES",
                         "MIMS","NAES","NOCS","NYEB","ORMS","PIES","RTES",
                         "RTH","SAES","SFHS","SWES","SWPK","TEES","TEP",
                         "WGES","ZBS")

    if(current){
      browser()
      if(to == "abbr"){
        df$current.school = df$school.abbr %in% active.schools.abbr
      }
      if(to == "num"){
        df$current.school = df$school.num %in% active.schools.num
      }

      if(to == "name"){
        if(from == "abbr"){
          df$current.school = df[,school] %in% active.schools.abbr
        }
        if(from == "num"){
          df$current.school = df[,school] %in% active.schools.num
        }
      }
    }



  return(df)

}



