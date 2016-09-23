#let's write a function to take in a data frame and export it by a variable


export.by <- function(df, by, form = c("sheet", "file")) {
  #verify that we have a data frame
  if (!(class(df) %in% c("data.frame", "matrix"))) {
    return(warning("df must be a data frame or matrix"))
  }

  #verify that 'by' is one of the columns
  if (!(by %in% colnames(df))) {
    return(warning(paste(by, "not found in colnames(df)")))
  }
  #verify that form is specified correctly
  if(!(form %in% c("sheet", "file"))){
    return(warning("form argument must be \"sheet\" or \"file\""))
  }


  #make the by variable a factor
  lev <- levels(as.factor(df[, by]))

  #label the object
  obj <- deparse(substitute(df))

  #if we're in sheet mode, we want a master list
  if (form == "sheet") {
    path = paste(getwd(), "/", obj, "_sheets.xlsx", sep = "")
    write.xlsx(x = df,
               file = path,
               sheetName = "Master",
               row.names = FALSE)

  }

  #I guess we want a master list in both cases
  if (form == "file") {
    path = paste(getwd(), "/", obj, "_master.xlsx", sep = "")
    write.xlsx(x = df,
               file = path,
               row.names = FALSE)

  }

  #now let's loop through thte levels of the factor
  for (l in lev) {
    temp = df[which(df[, by] == l), ]

    #if we've chosen the file mode make seperate
    if (form == "file") {
      path = paste(getwd(), "/", obj, "_", l, ".xlsx", sep = "")
      write.xlsx(x = temp, file = path, row.names = FALSE)
    }

    if (form == "sheet") {
      path = paste(getwd(), "/", obj, "_sheets.xlsx", sep = "")
      write.xlsx(x = temp,
                 file = path,
                 sheetName = l,
                 append = TRUE,
                 row.names = FALSE)


    }
  }
}
