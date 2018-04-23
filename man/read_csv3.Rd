\name{read_csv3}
\alias{read_csv3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read CSVs with path names
}
\description{
Basically the same as read.csv but defaults to \code{stringsAsFactors = FALSE} and creates a column with the path of the file read.
}
\usage{
read_csv3(file, header = TRUE, sep = ",", quote = "\"",
                      dec = ".", fill = TRUE, comment.char = "", stringsAsFactors = FALSE ))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
A file path.
}
}
\details{
It might not inherit all the possible arguments for read.csv because I'm lazy
}
\value{
a data frame
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~SvenR }% use one of  RShowDoc("KEYWORDS")
