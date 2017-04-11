\name{BFmt}
\alias{BFmt}
\title{
Matrix creation for Bayes factor model selection.
}
\description{
This function calculates the Bayes factor for each migrate-n analyses and
creates a matrix with the BF values of each model.
}
\usage{
BFmt(mat, lestimate="Bezier")
}
\arguments{
  \item{mat}{a matrix returned by function \code{\link{Lmmat}}.}
  \item{lestimate}{a value indicating the likelihood estimate.
  It can be: (1) "Thermo" for Raw Thermodynamic score;
  (2) "Bezier" (the default) for Bezier approximated score;
  or (3) "Hmean" for Harmonic mean.}
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
\author{L.N. Peraza-Flores}

\note{function based on formula
BF= Exp(ln \emph{mL} (model 1) - ln \emph{mL} (model 2) ) from
migrate-n output suggestions}

\seealso{
\code{\link{LBFmt}, \link{Pmmt}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
