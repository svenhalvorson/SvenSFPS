#gonna write a program to do some all purpose appending


append.all <- function(type=c("xslx","csv","tab"), folder=NULL, patt="",  head = TRUE){

  require("xlsx")
  #if the user does not specify a directory, let them manually choose
  if(is.null(folder)){
    folder = choose.dir()
  }


  #list the files in the directory
  setwd(dir=folder)
  files <- list.files(pattern=patt)

  #reject if there aren't any files in that directory with that patter
  if(length(files) == 0){
    print("No files fit that pattern in that directory")
    return(NA)
  }

  #Now we will take in the first file
  if(type == "xlsx"){
    df = read.xlsx(file = files[1], as.data.frame = TRUE, header = head, sheetIndex = 1)
  }
  if(type == "csv"){
    df = read.csv(file=files[1], header = head)
  }
  if(type == "tab"){
    df = read.table(file = files[1], header=head)
  }

  #Also want to attache the file name to it so we can identify which obs come from which set
  df$source.file = files[1]

  #next we'll loop through the rest of the files and append them
  for(i in 2:length(files)){
    #make a temp file
    if(type == "xlsx"){
      temp = read.xlsx(file = files[i], as.data.frame = TRUE, header = head, sheetIndex = 1)
    }
    if(type == "csv"){
      temp = read.csv(file=files[i], header = head)
    }
    if(type == "tab"){
      temp = read.table(file = files[i], header=head, sep = "/t")
    }

    #Now bind it to the running total
    if(dim(temp)[1] !=0){
      temp$source.file = files[i]
      df = rbind(df,temp)
    }
  }

  return(df)
}
