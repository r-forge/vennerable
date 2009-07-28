\name{VennThemes}
\Rdversion{1.1}
\alias{VennThemes}
\title{
Create lists of graphical parameters for Venn diagrams
}
\description{
Given a \code{VennDrawing} object, which it consults to find the names of each of the sets and faces 
in the drawing, returns a list suitable as the \code{gp} argument in a subsequent call
to the \code{VennDrawing} method for \code{plot}.
}
\usage{
VennThemes(drawing, colourAlgorithm)
}
\arguments{
  \item{drawing}{
An object of class \code{VennDrawing}
}
  \item{colourAlgorithm}{
Missing or one of \code{signature},\code{binary},\code{sequential}. 
}
}
\details{
Set boundary colours  are taken from the \code{\link[RColorBrewer]{RColorBrewer}} \code{Set1} palette.
If \code{colourAlgorithm="signature"}, face fill colours are taken frome the  \code{RColorBrewer} \code{YlOrRed} 
palette based on the number of sets represented in the face, so eg all the faces corresponding to membership of a single set are pale yellow while the face corresponding to all the intersections is dark red.
If \code{colourAlgorithm="binary"}, faces are blue if they correspond to an odd number of intersections and white otherwise.
If \code{colourAlorithm="sequential"}, each face is given a different colour from the 
\code{RColorBrewer} \code{Set3} palette, although this is repeated if necessary if there are more faces than the length of this palette (9).
Different faces with the same signature will be given the same colour. 
If not specified, \code{sequential} is used if there are less than 9 faces, otherwise
\code{signature}.


}
\value{
A list with four elements
  \item{Face }{Named list of graphical parameters which will be applied to faces with corresponding names }
  \item{FaceText }{Named list of graphical parameters which will be applied to annotation in faces with corresponding names }
  \item{Set }{Named list of graphical parameters which will be applied to sets with corresponding names }
  \item{FaceText }{Named list of graphical parameters which will be applied to annotation in sets with corresponding names }
}
\author{
Jonathan Swinton (jonathan@swintons.net)
}
\seealso{
See also \code{\link[RColorBrewer]{RColorBrewer}}
}
\examples{
# change SetNames to large red text
C2 <- compute.Venn(Venn(n=2))
gp <- VennThemes(C2)
gp[["SetText"]] <- lapply(gp[["SetText"]],function(gps){gps$col<-"red";gps$cex<-2;gps})
plot(C2,gp=gp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs}% 