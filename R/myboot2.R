#' Bootstrap Function
#'
#' This function performs bootstrap resampling to estimate a statistic and visualize the results using a histogram.
#'
#' @param iter Number of iterations for bootstrap resampling.
#' @param x Vector of data values.
#' @param fun Function to apply on the bootstrap samples (default is "mean").
#' @param alpha Confidence level (default is 0.05).
#' @param cx Expansion factor for text size (default is 1.5).
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @return A list containing the confidence interval (ci), applied function (fun), and original data (x).
#'
#' @examples
#' myboot2(iter = 10000, x = my_data, fun = "median", alpha = 0.01, cx = 1.2)
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)   # Sample size

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun) # Vector of bootstrap sample statistics

  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))  # Confidence interval

  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap Sample Statistics", "\n", "alpha =", alpha, " iter =", iter, sep = ""),
               ...)  # Histogram plot

  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)

  pte <- apply(mat, 2, fun)  # Point estimate

  abline(v = pte, lwd = 3, col = "Black")  # Vertical line for point estimate
  segments(ci[1], 0, ci[2], 0, lwd = 4)  # Confidence interval segment
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)  # Text for lower bound of CI
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)  # Text for upper bound of CI

  text(pte, max(para$density)/2, round(pte, 2), cex = cx)  # Text for point estimate

  invisible(list(ci = ci, fun = fun, x = x))  # Output
}
