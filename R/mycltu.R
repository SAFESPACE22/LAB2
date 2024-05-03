#' Title
#' Generate samples using Central Limit Theorem
#'
#' @param n Number of samples
#' @param iter Number of iterations
#'
#' @return Sum of samples
#' @export
#'
#' @examples
#' myclt(n=10, iter = 10000)
myclt = function(n, iter) {
  y = runif(n * iter, 0, 5) # A
  data = matrix(y, nr = n, nc = iter, byrow = TRUE); # B - Suppress output
  sm = apply(data, 2, sum) # C
  hist(sm)
  return(sm)
}
w = myclt(n = 10, iter = 10000) # D
