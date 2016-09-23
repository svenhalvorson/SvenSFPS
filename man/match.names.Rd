\name{match.names}
\alias{match.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Match by Name
}
\description{
\code{match.names} is a function that takes in two data frames and merges them on DOB and
gender. It then tries to decide which of the observations in df1 and df2 are the same by. It does this by using exact and regular expression matching on first and last name.
}
\usage{
match.names(df1, df2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df1,df2}{
Two data frames. They each must have columns that contain the strings
    \itemize{
    \item{"gender"}
    \item{"dob" or "birth"}
    \item{"first"}
    \item{"last"}
    }

}

}
\details{
\code{match.names} returns a single data set that has all of the columns in df1 and df2 as well as a column labeled 'match'. Column names will be appended with a 1 or a 2 depending on whether they came from df1 or df2. The match column gives a determination whether the two observations are the same. The match field can contain the following values
\itemize{
\item{\bold{Not Merged: }}
  {No record in the other data set were found with that DOB and gender}
\item{\bold{Exact: }}
  {The first and last names matched exactly}
\item{\bold{Partial 2 names: }}
  {A a partial match on first and last name was found}
\item{\bold{Partial 1 name: }}
  {A partial match was found on first or last but not the other}}

}
\value{
Returns a matrix of df1 and df2 merged on dob and gender with a new column, 'matched'.
}

\author{
Sven Halvorson (svenedmail@gmail.com)
}


\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~matching }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~grepl }% __ONLY ONE__ keyword per line
