\name{fmt.frame}
\alias{fmt.frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format Data Frame or matrix
}
\description{
\code{fmt.frame} takes in a data frame and makes some alterations to the data types. It can change the non-numeric variables of \code{df} to character or factors and drop empty factor levels, or convert to matrix.
}
\usage{
fmt.frame(df, character = TRUE, matrix = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{a data frame}
  \item{character}{if TRUE, factors will be coerced as characters. If not, all non-numeric variables will be factors without empty levels}
  \item{matrix}{if TRUE, a matrix will be returned}
}
\details{
When producing a matrix, a character matrix will be returned if any of the variables are nonumeric
}
\value{
A data frame or matrix of equal dimensions to \code{df}
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

#make a data frame
chr = c("MONGOLS","ARE","COMING")
num = 100:102
fac = c("hug","in the", "shade")
dat = data.frame(cbind(chr,num,fac))
dat$chr = as.character(dat$chr)
dat$num = as.numeric(dat$num)

#what are the types right now?
lapply(dat, FUN = typeof)

#turn into character
lapply(fmt.frame(dat), FUN = typeof)

#see that the levels are reduced
levels(dat[c(1,3),][,3])
levels(fmt.frame(dat[c(1,3),], character = FALSE)[,3])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data.frame }% use one of  RShowDoc("KEYWORDS")
\keyword{ SvenR }% __ONLY ONE__ keyword per line
