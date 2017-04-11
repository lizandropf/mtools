\name{Lmmat}
\alias{Lmmat}
\title{
Matrix creation for Bayes factors model selection.
}
\description{
This function extracts all likelihood estimates from the output files
of migrate-n analyses and creates a matrix for model selection.
}
\usage{
Lmmat(x, loci, all=TRUE)
}
\arguments{
  \item{x}{(filepath) file name of one of the migrate-n output files.}
  \item{loci}{number of loci included in the migrate-n analyses.}
  \item{all}{logic value to indicate whether to include the results from
  concatenated loci.}
}
\details{All files should be in a single folder, the function extract all
values for subsequent model selection.}

\references{
Beerli, P. & Felsenstein, J. (2001) Maximum likelihood estimation of a
migration matrix and effective population sizes in n subpopulations by
using a coalescent approach. \emph{Proceedings of the National Academy
of Sciences}, 98, 4563--4568.\cr\cr
Beerli, P. & Palczewski, M. (2010) Unified framework to evaluate panmixia
and migration direction among multiple sampling locations.
\emph{Genetics}, \bold{185}, 313--326.\cr\cr
}
\author{L.N. Peraza-Flores}

\seealso{
\code{\link{Msplit}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
