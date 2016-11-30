\name{smerge}
\alias{smerge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
STATA Style Merge
}
\description{
A wrapper function for the base function \code{merge}. Inherits all arguments from \code{merge} and provides an additional flag column labeled \code{_merge}. \code{smerge} also outputs counts of matched and unmatched observations from each data frame. Refer to \code{\link{merge}} for argument descriptions.
}
\usage{
smerge <- function(x, y, by = intersect(names(x), names(y)),
                   by.x = by, by.y = by, all = TRUE, all.x = all, all.y = all,
                   sort = TRUE, suffixes = c(".x",".y"))
}

\details{
Unlike \code{merge}, \code{smerge} defaults to \code{all = TRUE}. The numeric codes for the merge column are also different from those used in STATA. -1 and 1 denote unmatched observations from \code{x} and \code{y} respectively. 0 denotes a matched observation
}
\value{
A data frame formed by merging \code{x} and \code{y} with an additional column labeled \code{_merge}
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
test1 = data.frame(letter = c("A","B","C","D"),
                   num = 1:4,
                   pork = c("bacon","carnitas","ham","chops"))



test2 = data.frame(letter = c("B","C","E","A"),
                   num = c(2,3,4,5),
                   chicken = c("wings","sammich","fried","drumstick"))


smerge(test1,test2)
}
\keyword{merge}% use one of  RShowDoc("KEYWORDS")
\keyword{SvenR}% __ONLY ONE__ keyword per line
