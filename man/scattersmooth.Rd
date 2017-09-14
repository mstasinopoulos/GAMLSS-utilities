\name{scattersmooth}
\Rdversion{1.1}
\alias{scattersmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two dimensional Smooth scatter plots
}
\description{
The function produced two dimensional smooth scatter plots. The method used is described   in Eilers and Goeman (2004).
}
\usage{
scattersmooth(x, y, nbin = 100, lambda = 1, ndot = 500,
              csize = 0.3, ticks = TRUE, xlim = c(min(x), 
              max(x)), ylim = c(min(y), max(y)), show = TRUE, 
              save = FALSE, data = NULL, xlab = NULL, 
              ylab = NULL,  cols = heat.colors(10:200), 
              col.points = "blue", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 the x-variable
}
  \item{y}{
  the y-variable 
}
  \item{nbin}{
  the number of bins required for smoothing 
}
  \item{lambda}{
  the smoothing  parameter
}
  \item{ndot}{
how many data points to show in the plot
}
  \item{csize}{the size of the data points}
  \item{ticks}{whether ticks in the x and y axis appear in the plot}
  \item{xlim}{the x limit}
  \item{ylim}{the y limit}
  \item{show}{whether to show the graph or not}
  \item{save}{whether to save the output as a  list or not }
  \item{data}{the data file \code{data}} 
  \item{xlab}{the x label as character string}
  \item{ylab}{the y label as character string}
  \item{cols}{for changing the color scheme, the defaul is 
              \code{heat.colors(10:200)}. Other suggestions  are 
              \code{gray(0:100/100)},
              \code{heat.colors(101)},
              \code{rainbow(100:200)},
              \code{terrain.colors(101)},
              \code{topo.colors(101)},
              \code{cm.colors(101)}. Note that if you have the package 
              \pkg{colorspace} in R you can used
              \code{heat_hcl(100)} which was the default before.
              }
   \item{col.points}{the colours of the points}
  \item{\dots}{for extra arguments}
}
\details{
The function is similar to the function  \code{smoothScatter()} in \pkg{graphics} but it used penelized bin smoother as
described in Eilers and Goeman (2004) rather than kernel smoother.
}
\value{
the function produces a two dimensional smooth plot and saves if \code{save=TRUE} a list with the following components:

\item{Hraw}{A nbin by nbin matrix containing the bin row data}
\item{Hsmooth}{A nbib by nbib matrix containing the smooth two dimensional histogram}
\item{xgrid}{the x-grid}
\item{ygrid}{the y-grid}
\item{xbin}{the bin for x values}
\item{ybin}{the bin for y values}
\item{nmiss}{number of missing values}  
\item{seldots}{the values of the plotted dots}
}
\references{
Eilers, P. H. C. and Goeman, J. J. (2004). Enhancing scatterplots with smoothed density. \emph{Bioinformatics},
Vol \bold{20} no 5, pp 623-628.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Paul Eilers \email{p.eilers@erasmusmc.nl}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link[graphics]{smoothScatter}},\code{\link[gamlss]{gamlss}}
}
\examples{
m <- 1000
set.seed(pi)
phi <- 2 * pi * runif(m)
rho <- rchisq(m, df = 6)
x <- cos(phi) * rho
y <- sin(phi) * rho
H <- scattersmooth(x, y)
H1 <- scattersmooth(x, y, cols=rainbow(100:200)) 
#  If you have the package colorspace use instead 
# library(colorspace)
# H <- scattersmooth(x, y, cols=heat_hcl(100))
# H1 <- scattersmooth(x, y, cols=rainbow_hcl(100))
data(db)
scattersmooth(age, head,  data=db, cols=terrain.colors(101), ndot=2000, lambda=1)
# or if you have colorspace
#scattersmooth(age, head,  data=db, cols=terrain_hcl(100), ndot=2000, lambda=1)
}
\keyword{regression}
