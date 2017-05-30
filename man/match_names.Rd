\name{match_names}
\alias{match_names}
\title{
Fuzzy matcher
}
\description{
\code{match_names} takes in two data frames, merges them based on the values in \code{fixed}, and computes some measures of agreement for the columns in \code{partials}
}
\usage{
match_names(df1, df2, fixed = NA, partials = NA,
                        edits = TRUE, regex = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1,df2}{
data frames to be merged
}
\item{fixed}{
character vector of columns to be merged on
}
\item{partials}{
columns to fuzzy match on
}
\item{edits}{
if \code{TRUE} edit distances are computed
}
\item{regex}{
if \code{TRUE} partial regular expression matches returned
}
}
\details{
\code{match_names} is a function designed to help find duplicates within a data set or find matches between simliar data sets. Often you will want to determine the \code{fixed} by which values are most likely to match (like DOB). Then use the function and sort by some of the measures. A small edit distance or proportion indicate a likely match.
}
\value{
a data frame composed of \code{df1} and \code{df2} merged. Additional columns may include \code{_count} which are edit distances, \code{_prop} variables are the ratio of the edit distance to the mean number of characters, and \code{_regex} columns which indicate whether a subset of one name matches the other.
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
df1 = data.frame(x = c(1,1,2,2,3),
                  y = c("tricycle","bicycle", "triplane","double triplane", "triceratops"),
                  stringsAsFactors = FALSE)
df2 = data.frame(x = c(2,3,2,1,1),
                  y = c("tritip","biceratops", "triplane", "tripline", "tricycle" ),
                  stringsAsFactors = FALSE)
df3 = match_names(df1, df2, fixed = "x", partials = "y")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{~SvenR}% use one of  RShowDoc("KEYWORDS")
