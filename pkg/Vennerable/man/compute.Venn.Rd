\name{compute.Venn}
\Rdversion{1.1}
\alias{compute.Venn}
\title{
Computes a geometrical description of a Venn diagram.
}
\description{
Takes an object of class \code{Venn} and returns one of class \code{VennDrawing}.
}
\usage{
compute.Venn(V, doWeights = TRUE, doEuler = FALSE, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
An object of class \code{Venn}. Must be present.
}
  \item{doWeights}{
If \code{TRUE}, attempt to produce a diagram in which the areas of faces are proportional to \code{Weights(Venn)}.

}
  \item{doEuler}{
If \code{TRUE}, attempt to produce a diagram in which faces with \code{Weights(Venn)==0} are suppressed..
}
  \item{type}{
	One of \code{squares,circles,ellipses,triangles,AWFE, AWFEscale, battle, cog}.
}
}
\details{
See the \code{Venn} vignette for more details.
}
\value{
An object of class \code{VennDrawing}.
}
\author{
Jonathan Swinton (jonathan@swintons.net)
}
\examples{

## The function is currently defined as
function (V, doWeights = TRUE, doEuler = FALSE, type) 
{
    nSets <- NumberOfSets(V)
    if (nSets < 2) {
        stop("Not enough sets")
    }
    if (missing(type)) {
        type <- if (nSets == 2) {
            "circles"
        }
        else if (nSets == 3) {
            "circles"
        }
        else if (nSets == 4) {
            if (doWeights) 
                "ChowRuskey"
            else "squares"
        }
        else {
            if (doWeights) 
                "ChowRuskey"
            else "AWFE"
        }
    }
    C3 <- switch(type, AWFE = , AWFEscale = , battle = , cog = , 
        compute.AWFE(V, type = type), ChowRuskey = , compute.CR(V, 
            doWeights), circles = , if (nSets == 2) {
            compute.C2(V, doWeights, doEuler)
        }
        else if (nSets == 3) {
            compute.C3(V, doWeights)
        }
        else {
            stop(sprintf("Type \%s not implemented for \%d sets", 
                type, nSets))
        }, squares = , if (nSets == 2) {
            compute.S2(V, doWeights, doEuler)
        }
        else if (nSets == 3) {
            compute.S3(V, doWeights)
        }
        else if (nSets == 4) {
            compute.S4(V, doWeights)
        }
        else {
            stop(sprintf("Type \%s not implemented for \%d sets", 
                type, nSets))
        }, triangles = , if (nSets == 3) {
            compute.T3(V, doWeights)
        }
        else {
            stop(sprintf("Type \%s not implemented for \%d sets", 
                type, nSets))
        }, ellipses = , if (nSets == 4) {
            compute.E4(V, doWeights)
        }
        else {
            stop(sprintf("Type \%s not implemented for \%d sets", 
                type, nSets))
        })
    C3
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
