\name{fill_down}
\alias{fill_down}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fill Missing Values Down
}
\description{
\code{fill_down} runs through a vector and replaces missing values with the last nonmissing value above it. It can be set to consider only \code{NA} or also include empty strings.
}
\usage{
fill_down(x, string = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An atomic vector. Often the column of a data frame.
}
  \item{string}{when \code{TRUE}, \code{fill_down} considers \code{""} as missing and is replaced by the last non \code{NA} or \code{""} value.
}
}
\details{
This function is especially useful for filling in values from split cells from an excel file.}
\value{
An atomic vector
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
test = c(NA, "", NA, NA, "TULAROSA", NA, "RUIDOSO", "", "", NA, "CORONA", NA)
fill_down(test)
fill_down(test, string = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~SvenR }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~missing }% __ONLY ONE__ keyword per line
