\name{clear}
\alias{clear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Clears Environment and Console
}
\description{
Removes all objects from the environment specified by \code{env}. Adds a blank page of text to the console.
}
\usage{
clear(env = globalenv())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{env}{
Specify an environment to clear the objects from.
}
}

\value{
none
}

\author{
Sven Halvorson (svenedmail@gmail.com)
}

\examples{
e = new.env()
e$a = 1
ls.str(e)
clear(e)
}
\keyword{ SvenR }% use one of  RShowDoc("KEYWORDS")
