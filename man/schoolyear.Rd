\name{school.year}
\alias{school.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate school years
}
\description{
Converts date objects to school years.
}
\usage{
school.year(date)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{
a date object
}
}
\details{
\code{school.year} takes in a date object and returns a character vector denoting the school year for which that date falls within. It considers Aug 1st the first day of that school year.
}
\value{
A character vector
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
library("lubridate")
d = c("10232015","02042016","08211999")
d = mdy(d)
school.year(d)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{~date}% use one of  RShowDoc("KEYWORDS")
\keyword{~SvenR}% __ONLY ONE__ keyword per line
