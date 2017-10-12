\name{read_ada}
\alias{read_ada}
\title{
Read an ADA/ADM file
}
\description{
\code{read_ada} reads and relables a tab delimited file created from Powerschool's ADA/ADM report. To create
these files, cut/paste from the 1. (first row number) to the bottom right value.
}
\usage{
read_ada(path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
a path in the same style as \code{read.delim} uses.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a formatted \code{data.frame}
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
df <- read_ada("SWES_091317.txt")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SvenR }% use one of  RShowDoc("KEYWORDS")
\keyword{ ADA }% __ONLY ONE__ keyword per line
