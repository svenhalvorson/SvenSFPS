\name{double.regex}
\alias{double.regex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Double Regular Expressions
}
\description{
\code{double.regex} takes in two characters and determines if either is a subset of the other. This function uses
grepl so multiple instances will catch.
}
\usage{
double.regex(a, b, ignore.case = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a, b}{
character vectors of length 1
}
\item{ignore.case}{
if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
}
}

\value{
Returns a logical of length 1
}

\examples{
  double.regex("apples","oranges")
  double.regex("SUP WICHU GIRL","WICHU")
  double.regex("My name is Inigo Montoya...","MONTOYA",ignore.case=TRUE)
}
\keyword{ ~regex }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~grepl }% __ONLY ONE__ keyword per line
