\name{Venn}
\Rdversion{1.1}
\alias{Venn}
\alias{Weights}
\alias{Weights<-}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Construct intersections of sets}
\description{
Given a collection of Sets, \code{Venn} will compute all possible 
combinations of intersections and return an object of class \code{Venn}, storing the combinations as well
as the number of elements in each intersection (the 'weights'). 
It will also accept a specification of the weights directly,
or simply of the number of sets desired.
}
\usage{
Venn(Sets, Weight, SetNames, numberOfSets)
Weights(object)
Weights(object) <- value
}
\arguments{
  \item{Sets}{A list, each element of which can be a list defining the members of a Set}
  \item{Weight}{A named vector of weights to associate with each possible combination of Set intersections.
	Ignored if \code{Sets} is present.}
  \item{SetNames}{A character vector of names for each Set. Ignored if \code{Sets} present and has names}
  \item{numberOfSets}{An integer for the number of Sets. Ignored if  \code{Sets} or \code{SetNames} present.}
  \item{Venn}{An object of class \code{Venn}}
  \item{object}{An object of class \code{Venn}}
  \item{value}{An object like \code{Weight}}
}
\value{An object of class \code{Venn}}
\author{Jonathan Swinton jonathan@swintons.net}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Sets, Weight, SetNames, numberOfSets) 
{
    if (!missing(Sets)) {
        if (is.null(names(Sets)) & !missing(SetNames)) {
            names(Sets) <- SetNames
        }
        return(VennFromSets(Sets))
    }
    if (missing(numberOfSets)) {
        numberOfSets <- if (missing(SetNames)) 
            0
        else length(SetNames)
    }
    if (missing(SetNames)) {
        SetNames <- seq_len(numberOfSets)
    }
    if (numberOfSets == 0) {
        Indicator <- matrix(nrow = 0, ncol = 0)
    }
    else {
        Indicator <- (data.matrix(do.call(expand.grid, lapply(seq(1, 
            length = numberOfSets), function(x) {
            c(0, 1)
        })))) == 1
    }
    rownames(Indicator) <- apply((Indicator), 1, function(x) {
        paste(as.numeric(x), collapse = "")
    })
    colnames(Indicator) <- SetNames
    if (missing(Weight)) {
        Weight <- rep(1, nrow(Indicator))
    }
    else if (length(Weight) > 0 & length(Weight) < nrow(Indicator) & 
        is.null(names(Weight))) {
        stop("Weight length does not match number of intersections")
    }
    if (is.null(names(Weight))) {
        names(Weight) <- rownames(Indicator)
    }
    IndicatorWeight <- cbind(Indicator, .Weight = Weight[rownames(Indicator)])
    new("Venn", IndicatorWeight = IndicatorWeight)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs}
