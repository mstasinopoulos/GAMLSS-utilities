#-----------------------------------------------
#-----------------------------------------------
# functions to scale variables from 0 to 1
scale0to1 <- function(x)
{
  x <- as.matrix(x)
  nc <- ncol(x)
  minX <- apply(x, 2L, min, na.rm=TRUE )
  maxX <- apply(x, 2L, max, na.rm=TRUE )
  Y <- sweep(x, 2L, minX, check.margin = FALSE)
  Y <- sweep(Y, 2L, maxX-minX, FUN="/",  check.margin = FALSE)
  # Y <- (x-min(x))/(max(x)-min(x))
  attr(Y, "min") <- minX
  attr(Y, "max") <- maxX
  Y
} 
#-----------------------------------------------
unscale0to1 <- function(x) 
{
  minX <- attributes(x)$min
  maxX <- attributes(x)$max
  Y <- sweep(x, 2L, (maxX-minX), FUN="*", check.margin = FALSE)
  Y <- sweep(Y, 2L, minX, FUN="+", check.margin = FALSE)
  attr(Y, "min") <- NULL
  attr(Y, "max") <- NULL
  Y
}
#----------------------------------------------- 
