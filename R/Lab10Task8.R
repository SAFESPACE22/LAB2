#' Maximum Likelihood Estimation for Normal Distribution Parameters
#'
#' This function performs maximum likelihood estimation for the parameters of a normal distribution, given a sample vector.
#' It plots the likelihood contour and identifies the maximum likelihood estimates.
#'
#' @param x Numeric vector. Sample data.
#' @param mu Numeric vector. Grid values for the mean parameter to evaluate likelihood.
#' @param sig Numeric vector. Grid values for the standard deviation parameter to evaluate likelihood.
#' @param ... Additional arguments to be passed to the contour plot function.
#'
#' @return A list containing:
#' \item{x}{The sample vector.}
#' \item{coord}{The coordinates of the maximum likelihood estimate in the grid.}
#' \item{maxl}{The maximum likelihood value.}
#' \item{muest}{The maximum likelihood estimate for the mean parameter.}
#' \item{sigest}{The maximum likelihood estimate for the standard deviation parameter.}
#'
#' @export
#'
#' @examples
#' mymlnorm(c(1, 2, 3, 4, 5), mu = seq(1, 10, 0.5), sig = seq(0.5, 3, 0.1))
mymlnorm=function(x, mu, sig, ...){  #x sample vector
  nmu=length(mu) # number of values in mu
  nsig=length(sig)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,m,p) log(dnorm(x, mean=m, sd=p))   # log lik for normal

  for(j in 1:nsig){
    z=outer(x, mu, lfun, p = sig[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of mu,
    # col2 each x with 2nd value of m
    # all with sig=sig[j]
    y=apply(z, 2, sum)
    # y is a vector filled with log lik values,
    # each with a difft mu and all with the same sig[j]
    zz=cbind(zz, y)
    ## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas
  }

  maxl=max(exp(zz))
  coord=which(exp(zz) == maxl, arr.ind = TRUE)
  maxlsig=apply(zz, 1, max)

  contour(mu, sig, exp(zz), las = 3, xlab = expression(mu), ylab = expression(sigma), axes = TRUE,
          main = expression(paste("L(", mu, ",", sigma, ")", sep = "")), ...)

  mlx=round(mean(x), 2)  # theoretical
  mly=round(sqrt((n-1) / n) * sd(x), 2)

  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)

  abline(v = mean(x), lwd = 2, col = "Green")
  abline(h = sqrt((n-1) / n) * sd(x), lwd = 2, col = "Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v = muest, h = sigest)

  # modified to output estimated mean and sigma to console
  return(list(x = x, coord = coord, maxl = maxl, muest = muest, sigest = sigest))
}
