\name{label.schools}
\alias{label.schools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert School Labels for SFPS
}
\description{
\code{label.schools} attaches a column to an existing data set containing a different label for the schools in SFPS. It can create a numeric ("num"), abbreviation ("abbr"), or long form name ("name").

EX: 8, "AMES", "Acequia Madre Elementary"
}
\usage{
\code{label.schools(df, school = "school",
                        to = "abbr",
                        from = NA,
                        current = FALSE)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
a data frame
}
  \item{school}{
the column representing the school variable. Must be a column in df.
}
  \item{from}{
The existing form that \code{school} is in. You can select from \code{c("num","abbr","name")} or leave \code{from = NA} and let label.schools guess which form it's in. If \code{from == "name"}, \code{label.schools} will attempt to match \code{school} to fragments of the long name.
}
  \item{to}{
the form of the new column of df. Choose from \code{c("num","abbr","name")}.
}
  \item{current}{
if \code{TRUE}, \code{label.schools} will also return a logical indicating whether or not the school is one of our active schools
}
}
\details{\code{label.schools} uses the data set convtable included in SvenR. The old codes for El Camino Real are coerced to \code{num} = 11 or \code{abbr} = "ECCS" from 174 and c("ECRA","AFES").}

\value{
df with an additional column denoted school.\code{to}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sven Halvorson (svenedmail@gmail.com)%%  ~~who you are~~
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
  data = data.frame(c("madre elem","Agua Fria","Capital"))
  colnames(data) = "esquela"
  data = label.schools(df = data, school = "esquela", to = "name")
  data = label.schools(df = data, school = "school.name", to = "abbr")
  data

  #Bojangles! We did it!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ label }% use one of  RShowDoc("KEYWORDS")
\keyword{ school }% __ONLY ONE__ keyword per line
\keyword{ SvenR }
