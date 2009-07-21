\name{SetLabelPositions-methods}
\docType{methods}
\alias{SetLabelPositions-methods}
\alias{SetLabelPositions,CircleDrawing-method}
\alias{SetLabelPositions,SquareDrawing-method}
\alias{SetLabelPositions,VennDrawing-method}
\title{Suggested locations for plotting labels for set boundaries}
\description{
 These methods all return a data.frame containing, with one row for each Set,
a label text, a position and horizontal and vertical justification.
The \code{CircleDrawing} and \code{SquareDrawing} classes are partly defined
as inheriting from the \code{VennDrawing} class so they can override this function.
This technique of defining a new class and overriding the display function
is the only current user-level mechanism to modify the position of set labels.

The algorithms used are fairly crude. 
}
\section{Methods}{
\describe{
\item{object = "CircleDrawing"}{Above the highest point of each circle. (Actually the class override is only done so this can be computed from the centre and radius alone).   }
\item{object = "SquareDrawing"}{For 2 sets, left justify the label of the left-most set, and right-justify that of the right-most }
\item{object = "VennDrawing"}{ Above the highest point of each set boundary. }
}}
\keyword{methods}
