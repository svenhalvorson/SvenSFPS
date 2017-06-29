\name{grepl_multi}
\alias{grepl_multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiple Pattern Matching
}
\description{
\code{grepl_multi} is a wrapper for \code{grepl} that allows multiple inputs to the \code{pattern} argument. Depending on the \code{all} argument, it either returns whether each element of \code{x} matches all or at least one of the patterns in \code{patterns}. \code{grepl_multi} inherits all the other parameters of \code{grepl}.
}
\usage{
grepl_multi <- function(patterns, x, all = FALSE,
                        ignore.case = FALSE, perl = FALSE,
                        fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{patterns}{ A character vector of regular expressions (or strings if \code{fixed == TRUE}) to be matched onto x \code{x}.
  }
  \item{x}{ A character vector where matches are sought.
  }
  \item{all}{ if \code{TRUE} then all values in \code{patterns} must match. Otherwise, at least one one match among \code{patterns} is required to return \code{TRUE}

  }
  \item{...}{ refer to \code{?grepl} for inherited arguments

  }

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a logical vector of the same length as \code{x}
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

\seealso{}
\examples{
pat = letters[1:3]
strings = c("foo", "bar", "cabbie", "green chile")

# First we ask, which one of these stings have at least one of "a", "b", or "c"
cbind(strings,grepl_multi(patterns = pat, x = strings))

# Or we can ask, which strings have all three of "a", "b", and "c"
cbind(strings,grepl_multi(patterns = pat, x = strings, all = TRUE))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{SvenR}% use one of  RShowDoc("KEYWORDS")
\keyword{grepl}% __ONLY ONE__ keyword per line
