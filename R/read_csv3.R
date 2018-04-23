# Let's write a little wrapper for read.csv that defaults
# to stinrgsAsFactors = FALSE and creates a column with the filepath

read.csv3 <- function(file, header = TRUE, sep = ",", quote = "\"",
                      dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE ){

  temp = read.csv(file, header = header, sep = sep, quote = quote,
                  dec = dec, fill = fill, comment.char = comment.char, stringsAsFactors = stringsAsFactors )
  # make a naming conventions for the path
  colname = "path"
  switch = 0
  while(switch == 0){
    if(!colname %in% colnames(temp)){
      switch = 1
    }
    else{
      colname = paste0(colname,"_")
    }
  }
  temp[colname] = file
  temp


}
