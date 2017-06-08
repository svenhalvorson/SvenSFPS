\name{dupes_tag}
\alias{dupes_tag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tabulate Covariate Patterns
}
\description{
\code{dupes_tag} is similar to the duplicates tag function in STATA. It takes in a data frame and returns that data frame with a an additional column labeled 'dupes'. Dupes is the count of all \bold{other} observations that have the same covariate pattern in the columns listed in \code{...}
}
\usage{
dupes_tag(df, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{a data frame}
  \item{...}{any number of columns of df. Must be listed in quotes}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ \code{df} with a new column labeled 'dupes'
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

library("dplyr")
#create a little test set

test.set = data.frame(sample(x = c("A","B","C"),size = 20, replace = TRUE))
colnames(test.set) = "v1"
test.set$v2 = sample(x = 1:2, size = 20, replace = TRUE)
test.set$v3 = sample(x = c("foo", "bar"), size = 20, replace = TRUE)
test.set = arrange(test.set,v1,v2,v3)

dupes_tag(test.set,"v1","v2","v3")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ duplicates }% use one of  RShowDoc("KEYWORDS")
\keyword{ SvenR }% __ONLY ONE__ keyword per line
