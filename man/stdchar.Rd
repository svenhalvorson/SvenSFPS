\name{stdchar}
\alias{stdchar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardize Character Vector
}
\description{
This function takes a character vector, removes spaces, seperates words with "-", removes commas, and makes it upper case.
The idea is that if we want to match names, it's nice to have it clean up the character vectors first.
}
\usage{

stdchar(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a character vector to be standardized
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If the argument is a character or factor vector, stchar returns a modified version. Otherwise, it returns the argument
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
names = c(" joe schmo","pereda, LUpe")
stdchar(names)
}
