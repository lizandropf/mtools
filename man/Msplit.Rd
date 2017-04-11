\name{Msplit}
\alias{Msplit}
\title{
Tool for splitting migrate-n bayes-all files for seeing in Tracer.
}
\description{
This function splits the bayes-all output files from migrate-n analyses and
write them down to text files for analysis in Tracer or read them into R
environment for analysis in R.
}
\usage{
Msplit(x, loci, rep, writef=FALSE, f)
}
\arguments{
  \item{x}{file path to the migrate-n bayes-all file output.}
  \item{loci}{number of loci included in the migrate-n analysis.}
  \item{rep}{number of replicates in the migrate-n analysis.}
  \item{writef}{write tables to file if TRUE, otherwise they are readed
  into R for posterior analyses, the default}
  \item{f}{a stem file name for the new text files to be written down: only if
  writef=TRUE, otherwise no needed.}
}

\references{
Beerli, P. & Felsenstein, J. (2001) Maximum likelihood estimation of a
migration matrix and effective population sizes in n subpopulations by
using a coalescent approach. \emph{Proceedings of the National Academy
of Sciences}, 98, 4563--4568.\cr\cr
Beerli, P. & Palczewski, M. (2010) Unified framework to evaluate panmixia
and migration direction among multiple sampling locations.
\emph{Genetics}, \bold{185}, 313--326.\cr\cr
}
\author{Lizandro N. Peraza-Flores}

\note{The approximate sufficiency techniques described here are only suitable
for single parameters only.}

\seealso{
\code{\link{Lmmat}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
