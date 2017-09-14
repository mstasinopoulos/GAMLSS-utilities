\name{scale0to1}
\alias{scale0to1}
\alias{unscale0to1}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scaling Matrices or Vectors to Values from Zero to One}
\description{
This function can take a matrix or a vector and scale it to values from zero to one
}
\usage{
scale0to1(x)

unscale0to1(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix or a vector}
}
\details{
This function is created to facilitate the construction of a design matrix where the explanatory variables are scaled to values from zero to one. This can be helpful for example if neural networks are fitted to a GAMLSS model like in p 306 of Stasinopoulos \emph{et al} (2017).
}
\value{=The rsult is a scaled  matrix}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017) \emph{Flexible Regression and Smoothing: Using GAMLSS in R}, Chapman and Hall/CRC. 

(see also \url{http://www.gamlss.org/}).
}

\author{
Mikis Stasinopoulos
}

\examples{
library(gamlss)
# variable unscaled
plot(y~x, data=abdom)
sabdom <- scale0to1(abdom)
plot(y~x, data=sabdom)
unabdom <- unscale0to1(sabdom)
plot(y~x, data= unabdom)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
