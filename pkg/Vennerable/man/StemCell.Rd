\name{StemCell}
\Rdversion{1.1}
\alias{StemCell}
\docType{data}
\title{
Stem cell data
}
\description{
Data on the genes which bind to each of three transcription factors (and a fourth which may also be a transcription factor)
}
\usage{data(StemCell)}
\format{
  The format is:
List of 4
 $ OCT4 : chr [1:623] "AASDH" "ABTB2" "ACCN4" "ACD" ...
 $ SOX2 : chr [1:1279] "182-FIP" "AASDH" "ABCA5" "ABCB10" ...
 $ NANOG: chr [1:1687] "13CDNA73" "AASDH" "ABCA5" "ABCB10" ...
 $ E2F4 : chr [1:1273] "76P" "7h3" "AAMP" "AATF" ...
}
\details{
}
\source{
Taken from supplementary information in Boyer et al, "Core transcriptional regulatory circuitry in human embryonic stem cells",
\emph{Cell}, 2005, 122:947--956, \code{10.1016/j.cell.2005.08.020},\url{http://www.ncbi.nlm.nih.gov/pubmed/16153702}.
}

\examples{
data(StemCell)
plot(Venn(Sets=StemCell),type="squares",doWeights=FALSE)
}
\keyword{datasets}
