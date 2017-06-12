\name{convtable}
\alias{convtable}
\docType{data}
\title{
SFPS name and code conversions
}
\description{
Table with conversions between number codes, 4 letter abbreviations, and character names
}
\usage{convtable}
\format{
  A data frame with 51 observations on the following 11 variables.
  \describe{
    \item{\code{num}}{numerical school code}
    \item{\code{abbr}}{4 letter code}
    \item{\code{name}}{long form name}
    \item{\code{frag}}{likely fragments of the name}
    \item{\code{'2'}}{copies of name, num, and abbr for use of label_schools when \code{current == TRUE}}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
df = convtable

}
\keyword{datasets}
\keyword{SvenR}
