\name{export.by}
\alias{export.by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export by a Factor
}
\description{
\code{export.by} takes in a data frame and the name of one of its columns. It then groups the data by the levels of the chosen column and produces xlsx files accordingly.
.xlsx files
}
\usage{
export.by(df, by, form = c("sheet", "file"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\code{df}}{
  A data frame or matrix
}
  \item{by}{
  The name of one of the columns in df. This is the variable exports are groupd by.
}
  \item{form}{
  If "sheet" is selected, export.by will produce a single file with a sheet for each level of 'by' as well as a master list. If "file" is selected, a seperate file for each level of 'by' is created.
}

}
\details{
\code{export.by} uses your current directory so set it before running. Files are named according to the data frame you pass it. In sheets mode, the name of the file will be appended with "_sheets.xlsx". In file mode, each file will be appended with the level and a master list will be produced too.
}

\author{
Sven Halvorson (svenedmail@gmail.com)
}

\examples{
export.by(df = mtcars, by = "cyl", form = "sheet")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~export }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~xlsx }% __ONLY ONE__ keyword per line
