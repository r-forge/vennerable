\name{Venn-class}
\Rdversion{1.1}
\docType{class}
\alias{Venn-class}
\alias{[,Venn-method}
\alias{NumberOfSets,Venn-method}
\alias{plot,Venn,missing-method}
\alias{SetNames,Venn-method}
\alias{show,Venn-method}

\title{Class "Venn"}
\description{A class for intersecting multiple sets together}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Venn", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{IndicatorWeight}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{IntersectionSets}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "Venn")}: ... }
    \item{NumberOfSets}{\code{signature(object = "Venn")}: ... }
    \item{plot}{\code{signature(x = "Venn", y = "missing")}: ... }
    \item{SetNames}{\code{signature(object = "Venn")}: ... }
    \item{show}{\code{signature(object = "Venn")}: ... }
	 }
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jonathan Swinton jonathan@swintons.net }
\note{ ~~further notes~~ }

\examples{
showClass("Venn")
}
\keyword{classes}
