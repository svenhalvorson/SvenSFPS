# this function will read and relable an ada/adm file

read_ada <- function(path){
  
  df <- read.delim(file = path, header = FALSE, stringsAsFactors = FALSE)
  df = df[,3:ncol(df)]
  colnames(df)[1:9] = c("student_number", "name", "grade", "school_number",
                       "school_abbr", "entry_date", "exit_date", "membership", "attendance")
  df$grade = trimws(df$grade)
  df
}