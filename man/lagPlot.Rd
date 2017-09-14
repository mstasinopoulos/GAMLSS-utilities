\name{lagPlot}
\alias{lagPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lag plot for time series data}
\description{
The function \code{lagPlot()} plots a time series variable against its lagged values or
against the lagged values of an explanatory variable. 
}
\usage{
lagPlot(y, x = NULL, lags = 0, corr = TRUE, smooth = TRUE)
}
\arguments{
  \item{y}{time-series (univariate)}
  \item{x}{explanatory variable}
  \item{lags}{number of lag plots desired}
  \item{corr}{whether to include the coirrelation in the plot}
  \item{smooth}{whether to plot the smooting curve}
}
\details{
The function uses the functions \code{lag.plo1()} and \code{lag.plo2()} desribed in Shumway and Stoffer (2011) page 56.
}
\value{
A plot is produced.
}
\references{

Shumway R. H. and Stoffer D. S. (2011) \emph{Time Series Analysis and Its Applications, With R Examples}. (third edition),  Springer, New York,
.
}
\author{
Mikis Stasinopoulos 
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lag.plot}}
}
\examples{
dax<-EuStockMarkets[,"DAX"]
ftse<-EuStockMarkets[,"FTSE"]
lagPlot(dax, lags=9)
lagPlot(dax, ftse, lags=8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}

