#okay the goal here is to create a matching program that will identify
#common entries in two data frames by name. This is a common problem for
#me as sometimes we have data sets without student numbers.


#function takes in two dataframes or matrices
match.names <- function(df1,df2){

  ##############
  # SAFEGAURDS #
  ##############

  #load in our favorite stuffs
  svenpacks()

  #ensure that the arguments we received are the appropriate types
  typ = c("matrix","data.frame")
  if(!(class(df1) %in% typ & class(df2) %in% typ)){

    stop("df1 and df2 must be data frames or matrices")
  }

  #ensure that they both have the DOB, gender, first.name, and last.name
  #variables. We'll use a list of possible variable names so it's not
  #too specific

  #let's check that we can directly identify the the gender, dob, first, and last
  if(sum(grepl(x = colnames(df1),pattern="gender",ignore.case = TRUE)) !=1
     | sum(grepl(x = colnames(df2),pattern="gender",ignore.case = TRUE)) !=1){
    stop("Ambiguous or unidentified gender variable")
  }

  if((sum(grepl(x = colnames(df1),pattern="dob",ignore.case = TRUE)) !=1
     & sum(grepl(x = colnames(df1),pattern="birth",ignore.case = TRUE)) !=1)
     | (sum(grepl(x = colnames(df2),pattern="dob",ignore.case = TRUE)) !=1
     & sum(grepl(x = colnames(df2),pattern="birth",ignore.case = TRUE)) !=1)
     ){
    stop("Ambiguous or unidentified date of birth variable")
  }

  if(sum(grepl(x = colnames(df1),pattern="first",ignore.case = TRUE)) !=1
     | sum(grepl(x = colnames(df2),pattern="first",ignore.case = TRUE)) !=1){
    stop("Ambiguous or unidentified first name variable")
  }

  if(sum(grepl(x = colnames(df1),pattern="last",ignore.case = TRUE)) !=1
     | sum(grepl(x = colnames(df2),pattern="last",ignore.case = TRUE)) !=1){
    stop("Ambiguous or unidentified first name variable")
  }

  ##############
  ## PREPWORK ##
  ##############

  #Now that we've determined that there are appropriate columns in both dataframes
  #we need to identify those 4 variables in each data frame

  #Because I'm going to add new columns in, I want to save the original column names
  #so that I return the data frames with their original column names
  colnames(df1) <- paste(colnames(df1),".1", sep = "")
  colnames(df2) <- paste(colnames(df2),".2", sep = "")


  #short function to locate columns with matching colnames
  mcols <- function(df,exp){
    ret = grepl(x = colnames(df),pattern = exp,ignore.case = TRUE)
    return(ret)

  }


  #now let's create new columns to use in the algorithm. I will suffix with .q to
  #try to avoid any name conflicts
  df1$gen.q = df1[,mcols(df1,"gender")]
  df2$gen.q = df2[,mcols(df2,"gender")]
  df1$first.q = df1[,mcols(df1,"first")]
  df2$first.q = df2[,mcols(df2,"first")]
  df1$last.q = df1[,mcols(df1,"last")]
  df2$last.q = df2[,mcols(df2,"last")]

  #since we're allowing either a column with birth or dob in the name, we'll do some more iffing
  if(sum(mcols(df1,"dob")) == 1){
    df1$dob.q = df1[,mcols(df1,"dob")]
  } else{
    df1$dob.q = df1[,mcols(df1,"birth")]
  }

  if(sum(mcols(df2,"dob")) == 1){
    df2$dob.q = df2[,mcols(df2,"dob")]
  } else{
    df2$dob.q = df2[,mcols(df2,"birth")]
  }

  #let's also make sure that the genders are uppercase
  df1$gen.q = toupper(df1$gen.q)
  df2$gen.q = toupper(df2$gen.q)

  #Now merge the two together on the basis of dob and gender
  merged <- merge(x = df1, y = df2, by = c("gen.q","dob.q"), all = TRUE)
  #yay the merge seems to be working

  #make dat schist characters
  merged$first.q.x = as.character(merged$first.q.x)
  merged$first.q.y = as.character(merged$first.q.y)
  merged$last.q.x = as.character(merged$last.q.x)
  merged$last.q.y = as.character(merged$last.q.y)

  ##############
  ## MATCHING ##
  ##############

  #let's make a match column now to denote how likely we think it is to be a match
  merged$match = NA


  merged$match[which(is.na(merged$first.q.x) | is.na(merged$first.q.y))] <-"Not Merged"

  #if they're exact matches, let's mark them as such
  merged$match[which((merged$first.q.x == merged$first.q.y) & (merged$last.q.x == merged$last.q.y))] = "Exact"

  #now this set will mess around with the data and then connect back to the unambiguous
  to.match <- as.data.frame(apply(X = merged,MARGIN=2,FUN=stdchar),stringsAsFactors = FALSE)

  #set them aside
  unambiguous = merged[which(!is.na(merged$match)),]
  merged = merged[which(is.na(merged$match)),]

  #now this set will mess around with the data and then connect back to the unambiguous
  to.match <- as.data.frame(apply(X = merged,MARGIN=2,FUN=stdchar),stringsAsFactors = FALSE)

  fnXsplit <- str_split_fixed(to.match$first.q.x,"-",n=3)
  fnYsplit <- str_split_fixed(to.match$first.q.y,"-",n=3)
  lnXsplit <- str_split_fixed(to.match$last.q.x,"-",n=3)
  lnYsplit <- str_split_fixed(to.match$last.q.y,"-",n=3)

  allsplits <- as.data.frame(cbind(fnXsplit,fnYsplit,lnXsplit,lnYsplit),stringsAsFactors = FALSE)


  #need colnames
  names = c("first","last")
  splits = 1:3
  sets = c("X","Y")
  cols = c()
  for(nam in names){
    for(set in sets){
      for(split in splits){
        cols = c(cols,paste(nam,set,"_sp",split,sep=""))
      }

    }

  }
  colnames(allsplits) = cols

  #We'll apply this across the rows of the allsplits data frame
  candidate <- function(row, partial = FALSE){

    row = as.matrix(row)
    #now we make to matrices, one of the first names one of the lasts
    firsts = matrix(nrow=3,ncol=3)
    rownames(firsts)=row[1:3]
    colnames(firsts)=row[4:6]

    lasts = matrix(nrow=3,ncol=3)
    rownames(lasts)=row[7:9]
    colnames(lasts)=row[10:12]

    #now we'll loop over the rows and columns of matrices and apply the doubleRegex function
    for(i in 1:dim(firsts)[1]){
      for(j in 1:dim(firsts)[2]){
        firsts[i,j] = double.regex(colnames(firsts)[i], rownames(firsts)[j])
        lasts[i,j] = double.regex(colnames(lasts)[i], rownames(lasts)[j])
      }
    }

    #now take a maximum across two matrices and return that
    if(!partial){
      result = (max(firsts) & max(lasts))
      if(result){
       result = "Partial 2 names"
      }
      else{
        result = NA
      }
    }

    #if we're looking for onely one name match
    if(partial){
      result = (max(firsts) | max(lasts))
      if(result){
        result = "Partial 1 name"
      }
      else{
        result = NA
      }
    }

    return(result)
  }

  #do partial and full regexing
  to.match$match <- apply(X = allsplits, MARGIN = 1,FUN = candidate)
  singles <- apply(X = allsplits, MARGIN = 1,FUN = candidate, partial = TRUE)
  to.match$match[is.na(to.match$match)] = singles[which(is.na(to.match$match))]

  #now replace missing values with not a match
  to.match$match[is.na(to.match$match)] ="Not a match"

  #glue it back together and drop the uneccessary stuff
  merged$match = to.match$match
  merged = rbind(merged,unambiguous)

  #here's what we'll dro
  drops = c("gen.q","dob.q","first.q.x","last.q.x","first.q.y","last.q.y")

  merged = merged[,!(names(merged) %in% drops)]

  return(merged)

}


















