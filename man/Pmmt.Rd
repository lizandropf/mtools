\name{Pmmt}
\alias{Pmmt}
\title{
Probability values for migrate-n model selection.
}
\description{
This function calculates the probability values for each migrate-n analyses
and creates a matrix with the probabilities values of each model.
}
\usage{
Pmmt(mat, lestimate="Bezier")
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
}
\author{Lizandro N. Peraza-Flores}

\note{function based on recepe
(1) find the largest log likelihood,
(2) subtract that number form each log likelihood in the list
(3) exponentiate each element in the new list
(4) sum all elements in the list up, this is the denominator
(5) now divide each element in the list by that sum
at "https://molevol.mbl.edu/index.php/Migrate_tutorial_2015"}

\seealso{
\code{\link{BFmt}, \link{LBFmt}}
}
\examples{
# load example data:
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
