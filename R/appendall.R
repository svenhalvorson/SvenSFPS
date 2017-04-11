#gonna write a program to do some all purpose appending


append.all <- function(type=c("xslx","csv","tab"), folder=NULL, patt="",  head = TRUE, stringsAsFactors = FALSE){

  require("tidyverse")
  require("dplyr")

  #use a different variable to avoid confusion
  factors = stringsAsFactors

  #if the user does not specify a directory, use the currentwd
  if(is.null(folder)){
    folder = getwd()
  }


  #list the files in the directory
  setwd(dir=folder)
  files <- list.files(pattern=patt)

  #reject if there aren't any files in that directory with that patter
  if(length(files) == 0){
    stop("No files fit that pattern in that directory")

  }


  #Now we will take in the first file
  if(type == "xlsx"){
    df = read_excel(path = files[1], sheet = 1)
  }
  if(type == "csv"){
    df = read.csv(file=files[1], header = head, stringsAsFactors = factors)
  }
  if(type == "tab"){
    df = read.table(file = files[1], header=head, stringsAsFactors = factors)
  }

  #Also want to attache the file name to it so we can identify which obs come from which set
  j = 1
  found = 0
  while(found == 0){
    if(nrow(df)>0){
      print(paste0("Loading ",files[1]))
      df$source.file = files[j]
      found = 1
    }
    if(j > length(files)){
      stop("No observations found")
      found == 1
    }
    j = j+1
  }
  #next we'll loop through the rest of the files and append them
  for(i in (j+1):length(files)){
    print(paste0("Loading ",files[i]))
    #make a temp file
    if(type == "xlsx"){
      temp = read.xlsx(file = files[i], as.data.frame = TRUE, header = head, sheetIndex = 1, stringsAsFactors = factors)
    }
    if(type == "csv"){
      temp = read.csv(file=files[i], header = head, stringsAsFactors = factors)
    }
    if(type == "tab"){
      temp = read.table(file = files[i], header=head, sep = "/t", stringsAsFactors = factors)
    }

    #Now bind it to the running total
    if(nrow(temp) !=0){
      temp$source.file = files[i]
      df = bind_rows(df,temp)
    }
  }

  return(df)
}
