#Let's see if we can write a duplicates tagger similar to the one in STATA
#Goal is to take in a data frame or matrix and return it with a new column
#called dupes.count which will tell the number of rows that have that exact
#covariate pattern

dupes.tag <- function(df,...){

  svenpacks()
  ###########
  #safegaurds
  ###########

  #df must be a data frame
  if(class(df) != "data.frame"){
    stop("df must be a data frame")
  }


  #probably don't want ... to be empty
  cols = list(...)
  if(length(cols)==0){
    stop("One or more arguments required in ...")
  }

  #now verify that all the arguments of ... are in colnames(df)
  indf = cols %in% colnames(df)
  if(min(indf) == 0){
    #the stop message works with no seperator so let's make one
    missing = cols[!indf]
    txt = paste(" ", missing[1])
    for(i in 2:length(missing)){
      txt = paste(txt,missing[i], sep = ", ")
    }
    stop("Some arguments were not found in the columns of df:\n",txt)

  }

  ###########
  #agg +merge
  ###########

  #I feel like there is a much simpler, more beautiful way to do this but we're
  #having problems with our data types in the aggregate function.
  #We must produce the columns of df specified by ... however to get those columns
  #we need a character vector

  cols.char = c()
  for(i in 1:length(cols)){
    cols.char = c(cols.char,as.character(cols[i]))
  }

  #we gonna create a dummy variable
  one = rep(x = 1, times = nrow(df))
  #make a sum of the one column across the columns selected
  agg = aggregate(one, as.list(as.data.frame(df[,cols.char])), FUN = sum)

  #rename
  colnames(agg) = c(as.character(cols),"dupes")
  agg$dupes = agg$dupes - 1

  #now join back to df
  df = left_join(df,agg)
  return(df)


}




########
#testing
########

#test.set = data.frame(sample(x = c("A","B","C"),size = 20, replace = TRUE))
#colnames(test.set) = "v1"
#test.set$v2 = sample(x = 1:2,size = 20, replace = TRUE)
#test.set$v3 = sample(x = c("foo", "bar"),size = 20, replace = TRUE)

#
#fruf = function(...){
#  return(as.character(...))
#
#}
