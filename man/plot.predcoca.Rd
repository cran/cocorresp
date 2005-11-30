\name{plot.predcoca}
\alias{plot.predcoca}
\title{Biplots for predictive co-correspondence analysis}
\description{
  Produces biplots of the response and predictor from the results of a
  predictive co-correspondence analysis.
}
\usage{
\method{plot}{predcoca}(x, pages = NULL, axes = c(1:2),
     cex = c(0.8, 0.8), pch = c(par("pch"), 3),
     ylab = NULL, xlab = NULL, ann = par("ann"),
     lab.plot = TRUE, \ldots)
}

\arguments{
  \item{x}{ an object of class \code{"predcoca"}, the result of a
    call to \code{\link{predcoca.eigen}} or \code{\link{predcoca.simpls}}.}
  \item{pages}{ the number of pages to print the biplots on. Currently ignored.}
  \item{axes}{ a vector of length 2 indicating which predictive CoCA axes to plot.}
  \item{cex}{ a vector of length 2, containing the character expansion
    factors to use for the samples (sites) and the species.}
  \item{pch}{ a vector of length 2, containing the plotting character to
  use for the samples (sites) and the species.}
  \item{ylab}{ the label for the y-axis of the biplots, currently ignored.}
  \item{xlab}{ the label for the x-axis of the biplots, currently ignored.}
  \item{ann}{ logical, if TRUE plots are annotated and not if FALSE,
    currently ignored.}
  \item{lab.plot}{ logical, if TRUE, the biplots are given a title,
    derived from the names of the original data matrices. See \code{\link{coca}}.}
  \item{\dots}{ graphical parameters can be given as arguments to
    \code{\link{plot.default}}, these are currently ignored.}
}
\references{

  Ter Braak, C.J.F and Schaffers, A.P. (2004) Co-Correspondence
  Analysis: a new ordination method to relate two community
  compositions. \emph{Ecology} \bold{85(3)}, 834--846
}

\author{Gavin L. Simpson.}

\note{This is a simple attempt at a plot method for predictive
  co-correspondence analysis biplots. A number of the arguments do
  nothing and some of the functionaility is hard-coded within the
  function definition. This will be updated in a future release of the
  package.}

\seealso{\code{\link{coca}}, \code{\link{plot.default}}}
\examples{
## predictive CoCA
data(beetles)
## log transform the bettle data
beetles <- log(beetles + 1)
data(plants)
## predictive CoCA using SIMPLS and formula interface
bp.pred <- coca(beetles ~ ., data = plants)
## draw the biplots
plot(bp.pred)
}
\keyword{hplot}
