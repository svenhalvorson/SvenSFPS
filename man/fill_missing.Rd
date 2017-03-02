\name{fill_missing}
\alias{fill_missing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill missing entries
}
\description{
\code{fill_missing} takes in two atomic vectors or lists, \code{x} and \code{with}, and returns \code{x} with the values of \code{with} in the cases where x is NA or an empty string.
}
\usage{
fill_missing(x, with)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, with}{
atomic vectors or lists
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a modified version of \code{x}
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
one = c(1:3,NA,4:6,NA)
two = -8:-1
fill_missing(one,two)

three = c(letters[1:3],NA,"","SILLY PEANUT BUTTAH","")
four = 1:7
fill_missing(three, four)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SvenR }% use one of  RShowDoc("KEYWORDS")
\keyword{ missing }% __ONLY ONE__ keyword per line
