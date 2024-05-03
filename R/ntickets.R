#' Project
#'
#' @param N
#'   The total number of tickets available.
#' @param gamma
#'   The risk tolerance level.
#' @param p
#'   The probability threshold for ticket sales.
#'
#' @return
#'   A list containing the optimal number of tickets to be sold under discrete and continuous distribution assumptions.
#'   The list includes:
#'   - 'nd': The optimal number of tickets sold under discrete distribution.
#'   - 'nc': The optimal number of tickets sold under continuous distribution.
#'   - 'N': The total number of tickets available.
#'   - 'p': The probability threshold for ticket sales.
#'   - 'gamma': The risk tolerance level.
#'
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  # Generate a sequence of values for n
  n <- seq(N, floor(N + 0.1 * N), by = 1)

  # Calculate the discrete distribution
  obj_discrete <- 1 - gamma - pbinom(q = N, size = n, prob = p)

  # Define the objective function for continuous distribution
  cont <- function(n) {
    1 - gamma - pnorm(N + 0.5, n * p, sqrt(n * p * (1 - p)))
  }

  # Calculate the continuous distribution
  obj_continuous <- cont(n)

  # Find the optimal number of tickets sold for discrete distribution
  nd <- n[which.min(abs(obj_discrete))]

  # Find the optimal number of tickets sold for continuous distribution
  nc <- optimize(f = function(x) abs(cont(x)), interval = c(N, floor(N + 0.1 * N)))$minimum

  # Plot discrete distribution
  plot(n, obj_discrete, type = 'b',
       main = paste("Objective vs. n to find optimal tickets sold\n(nd =", nd, ", gamma =", gamma, ", N =", N, "discrete)"),
       ylab = "Objective", pch = 21, bg = "blue")
  abline(v = nd, h = 0, col = "red")

  # Plot continuous distribution
  curve(cont(x), N, floor(N + 0.1 * N),
        main = paste("Objective vs. n to find optimal tickets sold\n(nc =", nc, ", gamma =", gamma, ", N =", N, "continuous)"),
        ylab = "Objective", xlab = "n", lwd = 2)
  abline(v = nc, h = 0, col = "blue")

  # Prepare results
  results <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Print results
  print(results)
}
