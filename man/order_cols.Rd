\name{order_cols}
\alias{order_cols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Re-order columns of a data frame
}
\description{
\code{order_cols} takes in a data frame and reorders the columns specified by \code{...}. If \code{front} is \code{TRUE} specified columns are placed before unspecified ones, otherwise they are after unspecified columms.
}
\usage{
\code{order_cols(df, ..., front = TRUE)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame
}
  \item{...}{
  unquoted column names in \code{df}
  }
  \item{front}{
  a logical value. Should ... be placed before unspecified columns?
  }
}
\details{
If all columns in df are specified, df is returned with columns in that order.
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
See \code{order_cols_q} for a quoted version.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# We'll demonstrate reorder here
library("dplyr")
df = data.frame(a = 1:4,
                b = letters[4:7],
                c = rnorm(n = 4),
                d = rchisq(n = 4, df = 3),
                e = c("Salty", "Sweet", "Subtle", "Sweaty"),
                stringsAsFactors = FALSE)
df
order_cols(df, d, e)
order_cols(df, a, b, front = FALSE)

# Use with pipes
library("dplyr")
df %>%
  filter(nchar(e)>5) %>%
  order_cols(e,a)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ reorder }% use one of  RShowDoc("KEYWORDS")
\keyword{ SvenR }% __ONLY ONE__ keyword per line
