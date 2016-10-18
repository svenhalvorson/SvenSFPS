\name{fmt.frame}
\alias{fmt.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reformat Data Frames
}
\description{
Modify the string variables and type of a factor
}
\usage{
fmt.frame(df, to = c("character", "matrix"), form = c("data.frame", "matrix"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  A data frame
}
 \item{to}{
 Specify \code{character} or \code{factor} for the string variables to be set as
}
 \item{form}{
 The form of the output. Choose \code{data.frame} or \code{matrix}. Note that matrices cannot have factor variables.
}
}
\details{
Scans non numeric columns of df and changes them to the type specified by \code{to}
}
\value{
A data frame or matrix
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

data = iris
data = fmt.frame(data)
class(data[,"Species"])
class(data[,"Sepal.Length"])


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ class }% use one of  RShowDoc("KEYWORDS")
\keyword{ SvenR }% __ONLY ONE__ keyword per line
