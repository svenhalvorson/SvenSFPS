\name{order_cols}
\alias{order_cols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Re-order columns of a data frame
}
\description{
\code{order_cols} takes in a data frame and places the columns specified by \code{first} at the front and those by \code{last} at the end. If one of \code{first} or \code{last} are left as NA, they will be ignored.
}
\usage{
\code{order_cols(df, first = NA, last = NA)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame
}
  \item{first, last}{
  character vectors or NA
  }
}
\details{
All ellements of \code{first} and \code{last} must be in the column names of df and not be \code{NA}. \code{first} and \code{last} may not overlap.
}
\value{
\code{df} with its columns reordered
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
# We'll demonstrate reorder here
df = data.frame(a = 1:4,
                b = letters[4:7],
                c = rnorm(n = 4),
                d = rchisq(n = 4, df = 3),
                e = c("Salty", "Sweet", "Subtle", "Sweaty"))
order_cols(df, first = "d", last = c("a","e"))

order_cols(df, last = c("c", "a"))

library("dplyr")
df %>%
  filter(a <3) %>%
  order_cols(c("c","d"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reorder }% use one of  RShowDoc("KEYWORDS")
\keyword{ SvenR }% __ONLY ONE__ keyword per line
