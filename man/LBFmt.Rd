\name{LBFmt}
\alias{LBFmt}
\title{
log Bayes factors for migrate-n model selection.
}
\description{
This function calculates the log Bayes factor for each migrate-n analyses
and creates a matrix with the log BF values of each model.
}
\usage{
LBFmt(mat, lestimate="Bezier")
}
\arguments{
  \item{mat}{a matrix returned by function \code{\link{Lmmat}}.}
  \item{lestimate}{a value indicating the likelihood estimate. It can be:
  (1) "Thermo" for Raw Thermodynamic score; (2) "Bezier" (the default) for
  Bezier approximated score; or (3) "Hmean" for Harmonic mean.}
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

\note{function based on formula
LBF= 2(ln \emph{mL} (model 1) - ln \emph{mL} (model 2) ) at
"https://molevol.mbl.edu/index.php/Migrate_tutorial_2015"}

\seealso{
\code{\link{BFmt}, \link{Pmmt}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
