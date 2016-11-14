\name{svenpacks}
\alias{svenpacks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load my favorite packages
}
\description{
It  calls \code{library()} for the following packages
\itemize{
  \item{devtools}
  \item{dplyr}
  \item{ggplot2}
  \item{knitr}
  \item{reshape2}
  \item{stringr}
  \item{readxl}
  \item{xlsx}
  
}
Additionally, it sets the wd as my home folder at work. \code{ext} can specify a subdirectory

}

\usage{
svenpacks(work = TRUE, ext = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

\item{work}{if TRUE, sets to "C:/Users/shalvorson/Documents/R files"}
\item{ext}{used to specify a subdirectory}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
\code{svenpacks(work = TRUE, ext = "SvenR/R")}
}

\keyword{ ~packages }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~load }% __ONLY ONE__ keyword per line
\keyword{ ~SvenR }
