\name{centiles.ts}
\alias{centiles.ts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots the centile curves for a time series GAMLSS object}
\description{
This function \code{centiles.ts()} plots centiles curves for time series response variables who's distributions belong to the GAMLSS family of distributions.  
The function also tabulates the sample percentages below each centile curve (for comparison with the model percentages given by the argument \code{cent}.)    
}
\usage{
centiles.ts(obj, xvar = NULL, cent = c(0.5, 2.5, 50, 95.5, 99.5), legend = TRUE, 
 ylab = "y", xlab = "x", main = NULL, main.gsub = "@", 
 xleg = min(xvar), yleg = max(obj$y), xlim = range(xvar), 
 ylim = range(obj$y), save = FALSE, plot = TRUE, type = "l", 
 points = TRUE, pch = "+", col = "blue", col.centiles = 1:length(cent) + 2, 
lty.centiles = 1, lwd.centiles = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{a fitted gamlss object which has a time series response variable}
  \item{xvar}{the time of the time series}
  \item{cent}{a vector with elements the \% centile values for which the centile curves have to be evaluated}
  \item{legend}{whether a legend is required in the plot or not, the default is \code{legent=TRUE}  }
  \item{ylab}{the y-variable label}
  \item{xlab}{the x-variable label}
  \item{main}{the main title here as character. 
              If NULL the default title "centile curves using  NO" (or the relevant distributions name) is shown}
  \item{main.gsub}{if the \code{main.gsub} (with default "@") appears in the \code{main} 
                 title then it is substituted with the default title.}
  \item{xleg}{position of the legend in the x-axis}
  \item{yleg}{position of the legend in the y-axis}
  \item{xlim}{the limits of the x-axis}
  \item{ylim}{the limits of the y-axis}
  \item{save}{whether to save the sample percentages or not with default equal to \code{FALSE}. 
            In this case the sample percentages are printed but are not saved }
  \item{plot}{whether to plot the centiles. This option is useful for \code{centile.split} } 
  \item{type}{type of line}
  \item{pch}{the character to be used as the default in plotting points see \code{par}}
  \item{col}{plotting colour see \code{par}}
  \item{col.centiles}{Plotting colours  for the centile curves}
  \item{lty.centiles}{line type for the centile curves}
  \item{lwd.centiles}{The line width for the centile curves}
  \item{points}{whether the data points should be plotted, default is \code{TRUE} for \code{centiles()} and
                \code{FALSE} for \code{centiles.fan()}}
  \item{\dots}{for extra arguments}          
}
\details{
 Centiles are calculated using the fitted values in \code{obj} and \code{xvar} must
correspond exactly to the time  of the response time series object}
\value{
A centile plot is produced and the sample centiles below each centile curve are printed (or saved)
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}). 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.}
\author{
Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} with contribution from Majid  
Djennad}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[gamlss]{centiles}} 
}
\examples{

\dontrun{
library(gamlss.add)
dax <- EuStockMarkets[,"DAX"]
# returs
rdax <- diff(dax,1)
  w1 <- wlag(rdax,30)
# garch type 
f1<- gamlss(rdax~ la(rdax, lags=30, from.lag=1), sigma.fo=~la(rdax^2, 
            lags=30, from.lag=1), weights=w1, bf.cyc=10, family=TF)
tiR <- as.numeric(time(rdax))
centiles.ts(f1, xvar=tiR, cent=c(2.5,50,97.5), col.cent="black")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}% 
