\name{diagnostic}
\alias{diagnostic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Frame Diagnostics
}
\description{
Scan a data frame for data types, uniqueness, and missing data.
}
\usage{
diagnostic(df, blank = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame
}
  \item{blank}{
  if \code{TRUE}, empty strings (\code{""}) will be treated as \code{NA}
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a data frame of column diagnostics with print statements
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sven Halvorson (svenedmail@gmail.com)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

testdata = data.frame(x = sample(x = c("Lion", "Crystal Maiden", "Void", NA), size = 10, replace = TRUE),
                      y = sample(x = c(1:3, NA), size = 10, replace = TRUE),
                      z = sample(x = c(letters[1:2]), size = 10, replace = TRUE))
testdata
diagnostic(testdata)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SvenR }% use one of  RShowDoc("KEYWORDS")
