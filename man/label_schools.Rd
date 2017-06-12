\name{label_schools}
\alias{label_schools}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert School Labels for SFPS
}
\description{
\code{label_schools} creates a vector containing a different label for the schools in SFPS. It can create a numeric ("num"), abbreviation ("abbr"), or long form name ("name").

EX: 8, "AMES", "Acequia Madre Elementary"
}
\usage{
\code{label_schools(school,
              to = "abbr",
              from = NA,
              current = TRUE)}
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{school}{
an atomic vector of the schools to be relabeled.
}
  \item{from}{
The existing form that \code{school} is in. You can select from \code{c("num","abbr","name")} or leave \code{from = NA} and let label_schools guess which form it's in. If \code{from == "name"}, \code{label_schools} will attempt to match \code{school} to fragments of the long name.
}
  \item{to}{
the form of the output. Choose from \code{c("num","abbr","name")}.
}
  \item{current}{
if \code{TRUE}, \code{label_schools} returns the current name of the school, not the one that was listed. Ex: DVMS --> MMS
}
}
\details{\code{label_schools} uses the data set convtable included in \code{SvenR}. The old codes for El Camino Real are coerced to \code{num} = 11 or \code{abbr} = "ECCS" from 174 and c("ECRA","AFES").}

\value{
a vector of the relabeled values of \code{school}
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
  data = data.frame(c("madre elem","Agua Fria", "Gilberto University","Capital", "Capshawdawg"))
  colnames(data) = "esquela"
  data$newname = label_schools(school = data$esquela, to = "name")
  data$newname2 = label_schools(school = data$esquela, to = "name", current = FALSE)
  data$num = label_schools(school = data$newname, to = "num")
  View(data)

  #Bojangles! We did it!
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ label }% use one of  RShowDoc("KEYWORDS")
\keyword{ school }% __ONLY ONE__ keyword per line
\keyword{ SvenR }
