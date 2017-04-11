#let's just make a script to load all the packages I like

svenpacks <- function(work = TRUE, ext =""){

  if(work){
    di = paste("C:/Users/shalvorson/Documents/R files","/",ext, sep= "")
    setwd(di)
  }
  library("tidyverse")
  library("devtools")
  library("reshape2")
  library("knitr")
  library("lubridate")
}
