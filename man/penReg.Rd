\name{penReg}
\alias{penReg}
\alias{penRegQ}
\alias{pbq}
\alias{gamlss.pbq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to fit penalised regression}
\description{
The function \code{penReg()} can be used to fit a P-spline. 
It can be used as demonstration of how the penalised B-splines can be fitted to one explanatory variable.
For more that one explanatory variables use the function \code{pb()} in \pkg{gamlss}. 
The function \code{penRegQ()} is similar to the function \code{penReg()} but it estimates the "random effect" sigmas using the Q-function (marginal likelihood). The Q-function estimation takes longer but it has the advantage  that standard errors are provided for \eqn{log(sigma_e)}{\log \sigma_e}  and \eqn{log(sigma_b)}{\log \sigma_b}, where the sigmas are the standard errors for the response  and the random effects respectively.
The function \code{pbq()} is a smoother within GAMLSS and should give identical results to the additive function \code{pb()}. The function \code{gamlss.pbq} is not for use. 
}
\usage{
penReg(y, x, weights = rep(1, length(y)), df = NULL, lambda = NULL, start = 10, 
      inter = 20, order = 2, degree = 3,  plot = FALSE,
      method = c("ML", "ML-1", "GAIC", "GCV", "EM"), k = 2, ...)
penRegQ(y, x, weights = rep(1, length(y)), order = 2, start = 10, 
       plot = FALSE, lambda = NULL, inter = 20, degree = 3, 
       optim.proc = c("nlminb", "optim"), 
       optim.control = NULL)        
pbq(x, control = pbq.control(...), ...)
gamlss.pbq(x, y, w, xeval = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the response variable}
  \item{x}{the unique explanatory variable}
  \item{weights}{prior weights}
   \item{w}{weights in the iretation withing GAMLSS}
  \item{df}{effective degrees of freedom}
  \item{lambda}{the smoothing parameter}
  \item{start}{the lambda starting value if the local methods are used}
  \item{inter}{the no of break points (knots) in the x-axis}
  \item{order}{the required difference in the vector of coefficients}
  \item{degree}{the degree of the piecewise polynomial}
  \item{plot}{whether to plot the data and  the fitted function}
  \item{method}{The method used in the (local) performance iterations. Available methods are "ML", "ML-1", "EM", "GAIC" and "GCV"}
  \item{k}{the penalty used in "GAIC" and "GCV"}
  \item{optim.proc}{which function to be use to optimise the Q-function, options are \code{c("nlminb", "optim")}}
 \item{optim.control}{options for the optimisation procedures}
  \item{control}{arguments for the fitting function. It takes one two: i) \code{order} the order of the B-spline and \code{plot} whether to plot the data and fit.}
  \item{xeval}{this is use for prediction}
  \item{\dots}{for extra arguments}
}

\value{Returns a fitted object of class \code{penReg}. The object contains 1) the fitted 
\code{coefficients} 2) the   \code{fitted.values} 3) the response variable \code{y}, 
4) the label of the response variable \code{ylabel}
5) the explanatory variable \code{x}, 6) the lebel of the explanatory variable
7) the smoothing parameter  \code{lambda}, 8) the effective degrees of freedom \code{df},
9) the estimete for sigma \code{sigma},  
10) the residual sum of squares   \code{rss}, 11) the Akaike information criterion \code{aic},
12) the Bayesian information criterion \code{sbc} and 13) the \code{deviance} 
}
\references{
Eilers, P. H. C. and Marx, B. D. (1996). Flexible smoothing with
B-splines and penalties (with comments and rejoinder). \emph{Statist. Sci},
\bold{11}, 89-121.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Mikis Stasinopoulos \email{mikis.stasinopoulos@gamlss.org}, Bob Rigby \email{r.rigby@londonmet.ac.uk } and Paul Eilers
}

\examples{
set.seed(1234)
x <- seq(0,10,length=200); y<-(yt<-1+2*x+.6*x^2-.1*x^3)+rnorm(200, 4)
library(gamlss)
#------------------ 
# df fixed
g1<-gamlss(y~pb(x, df=4))
m1<-penReg(y,x, df=4) 
cbind(g1$mu.coefSmo[[1]]$lambda, m1$lambda)
cbind(g1$mu.df, m1$edf)
cbind(g1$aic, m1$aic)
cbind(fitted(g1), fitted(m1))[1:10,]
# identical
#------------------
# estimate lambda using ML
g2<-gamlss(y~pb(x))
m2<-penReg(y,x) 
cbind(g2$mu.df, m2$edf)
cbind(g2$mu.lambda, m2$lambda) 
cbind(g2$aic, m2$aic) # different lambda
cbind(fitted(g2), fitted(m2))[1:10,]
# identical
#------------------
#  estimate lambda using GCV
g3 <- gamlss(y~pb(x, method="GCV"))
m3 <- penReg(y,x, method="GCV") 
cbind(g3$mu.df, m3$edf)
cbind(g3$mu.lambda, m3$lambda)
cbind(g3$aic, m3$aic)
cbind(fitted(g3), fitted(m3))[1:10,]
# almost identical
#------------------
#  estimate lambda using  GAIC(#=3)
g4<-gamlss(y~pb(x, method="GAIC", k=3))
m4<-penReg(y,x, method="GAIC", k=3) 
cbind(g4$mu.df, m4$edf )
cbind(g4$mu.lambda, m4$lambda)
cbind(g4$aic, m4$aic)
cbind(g4$mu.df, m4$df)
cbind(g4$mu.lambda, m4$lambda)
cbind(fitted(g4), fitted(m4))[1:10,]

#-------------------
plot(y~x)
lines(fitted(m1)~x, col="green")
lines(fitted(m2)~x, col="red")
lines(fitted(m3)~x, col="blue")
lines(fitted(m4)~x, col="yellow")
lines(fitted(m4)~x, col="grey")
# using the Q function

# the Q-function takes longer
system.time(g6<-gamlss(y~pbq(x)))
system.time(g61<-gamlss(y~pb(x)))
AIC(g6, g61)
#
system.time(m6<-penRegQ(y,x))
system.time(m61<-penReg(y,x)) 
AIC(m6, m61)

cbind(g6$mu.df, g61$mu.df,m6$edf, m61$edf)
cbind(g6$mu.lambda,g61$mu.lambda, m6$lambda, m61$lambda) 
cbind(g6$aic, AIC(g6), m6$aic, AIC(m6), m61$aic, AIC(m61)) 
cbind(fitted(g6), fitted(m6))[1:10,]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
