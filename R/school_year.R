#We're gonna write a very short routine to create school years for
#an arbitrary date vector. The output should be a character vector with
#the following format: The school year starting in september of 2016 should
#be labeled 16-17

school_year <- function(date){

  library("lubridate")

  #now check if it's actually a date object
  if(!is.Date(date)){
    stop("date must be a date object")
  }

  #now we decide the month is at least 8 for the month
  month.flag = month(date)<8

  #numeric vector of years
  year = year(date)
  year = year %% 100


  #now we take the flag from it
  year = year - month.flag
  year2 = as.character((year+1))

  #deal with the case in which we're crossing a century
  year2[year2=="100"] = "00"

  #okie so the cases when we're talking about 2009 gives a 9 not 09
  year[nchar(year) == 1 & !is.na(year)] = paste0("0", year[nchar(year) == 1 & !is.na(year)])
  year2[nchar(year2) == 1 & !is.na(year2)] = paste0("0", year2[nchar(year2) == 1 & !is.na(year2)])

  #paste and return
  school.year = paste(year, year2, sep="-")


  return(school.year)
}
