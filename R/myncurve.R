#' Task 7 Lab 6
#'
#' @param a
#'     The upper limit of integration (the point at which to stop shading the area under the curve).
#' @param mu
#'     The mean of the normal distribution.
#' @param sigma
#'     The standard deviation of the normal distribution.
#'
#' @return
#' A list containing the mean (`mu`), standard deviation (`sigma`), and the area under the curve up to `a` (`area`).
#'
#' @export
#'
#' @examples
#' myncurve(1, 0, 1)
myncurve = function(a, mu, sigma){
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma), ylab = "Normal Density")

  # x values corresponding to the x - cords of points on the curve
  xcurve = seq(mu - 3*sigma, a, length = 1000)
  # Y values corresponding t0 the x values
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)

  # Fill in the polygon with the given vertices
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col = "Red")

  # Area
  area = pnorm(a, mean = mu, sd = sigma)
  area = round(area, 4)
  list(mu = mu, sigma = sigma, area = area)
}
