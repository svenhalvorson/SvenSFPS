\name{append.all}
\alias{append.all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Append All
}
\description{
Appends all files of a similar type in a single directory.
}
\usage{
append.all(type=c("xslx","csv","tab"),
            folder=NULL, patt="",  head = TRUE)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{
The file type. XLSX, CSV, and tab delimited.
}
  \item{folder}{
The directory to look in. If not specified you can use a popup to select the directory
}
  \item{patt}{
A regular expression to be matched within the file names
}
  \item{head}{
Whether or not the files have a header
}
}
\details{
This function takes in all the files in the directory and uses rbind() to glue them all together. Requires all the files to have the same delimites/file type. It will also create a column called 'source_file' that
}
\value{
Returns a composite data frame of all the files. Bananaphone.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
append.all(type = "csv", dir = "C:/Users/shalvorson/Documents", pat = "new.csv")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~append }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~files }% __ONLY ONE__ keyword per line
