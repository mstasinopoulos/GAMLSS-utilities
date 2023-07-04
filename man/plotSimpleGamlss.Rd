\name{plotSimpleGamlss}
\alias{plotSimpleGamlss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting a simple GAMLSS model for demonstration purpose}
\description{
This is to plot a simple GAMLSS model where only one explanatory variable exist in order to demonstrated how the distribution of the response changes according to values of the explanatory variable.   
}
\usage{
plotSimpleGamlss(y, x, model = NULL, formula = NULL, data = NULL, 
         family = NULL, val = NULL, N = 1000, x.val = quantile(x), 
         ylim = c(min(y), max(y)), xlim = c(min(x), max(x)),  
         ylab = NULL, xlab = NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
The response variable 
}
  \item{x}{
The explanatory variable (only one is allowed here)
}
  \item{model}{
A fitted gamlss model
}
  \item{formula}{
A formula for the mean model if \code{model=NULL}
}
  \item{data}{
The data where the response and the one explanatory can be found
}
  \item{family}{
The gamlss family distribution
}
  \item{val}{
this parameter determines how the plotted distribution is shown, increase/decrease it if the distribution is not shown properly
}
  \item{N}{This parameters determine how many values of y are generated for each \code{x.var}   
}
  \item{x.val}{
the values of the explanatory variable where we want to see the distribution
}
  \item{ylim}{
the y limits in the plot
}
  \item{xlim}{
the x limits in the plot
}
  \item{ylab}{
the y labels in the plot
}
  \item{xlab}{
the x labels in the plot
}

  \item{\dots}{
extra argument to be passed to \code{gamlss()} function if \code{model=NULL}
}
}
\details{
This function is for pedagogical purpose rather than fitting models to demonstrate  that the distribution of the response variable can vary according to explanatory variables. In its current from it can be used with continuous and discrete responses only.
}
\value{
A plot is shown
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Mikis Stasinopoulos \email{mikis.stasinopoulos@gamlss.org}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{scattersmooth}}
}
\examples{
## the abdominal data
m1 <- gamlss(y~pb(x), sigma.fo=~pb(x), data=abdom, family=LO)
plotSimpleGamlss(y,x, model=m1,   data=abdom, x.val=seq(15, 40, 5), 
                    ylim=c(0, 450), xlim=c(5, 45))
data(species)
species$ll <- log(species$lake)
m2 <- gamlss(fish~ll, data=species, trace=FALSE, family=PO )
plotSimpleGamlss(fish,ll, model=m2,   data=species, x.val=c(3,5,7, 9),
                  val=20, N=100, ylim=c(0,80))
 m3 <- gamlss(fish~ll, data=species, trace=FALSE, family=NBI, sigma.fo=~ll )
plotSimpleGamlss(fish,ll, model=m3,   data=species, x.val=c(3,5,7, 9),
                    val=20, N=100, ylim=c(0,100))
\dontrun{
##------------------------------------------------------------------------------
## the rent data 
## fitting the model first 
r1 <- gamlss(R~pb(Fl), sigma.fo=~pb(Fl),data=rent, family=GA, ylim=c(0, 3000))
## plot 1
plotSimpleGamlss(R,Fl, model=r1,   data=rent, x.val=seq(40,120, 5))
## plot 2 finer grid 
plotSimpleGamlss(R,Fl, model=r1,   data=rent, x.val=seq(40,120, 1), 
                  xlim=c(10,120))
##  the same but fitting the model within the function
## note that sigma formula has to be specified 
plotSimpleGamlss(R,Fl, formula= R~pb(Fl),   family=GA, data=rent, 
                    x.val=seq(40,120, 5), sigma.fo=~pb(Fl))
#------------------------------------------------------------------------------
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
