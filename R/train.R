#' Compute the Number of Tickets to Sell Based on Binomial and Normal Approximation
#'
#' @param N Integer. The number of available seats or tickets.
#' @param gamma Numeric. The risk threshold (e.g., probability of overselling).
#' @param p Numeric. The probability of a ticket holder actually attending.
#'
#' @returns A list containing:
#' \item{N}{The original number of tickets available.}
#' \item{p}{The probability of a ticket being used.}
#' \item{gamma}{The risk threshold.}
#' \item{nd}{The number of tickets calculated using the binomial distribution approach.}
#' \item{nc}{The number of tickets calculated using the normal approximation approach.}
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N, gamma, p) {
  # 1. Calculate number of tickets using Binomial Distribution
  nposs <- N
  while (pbinom(N, nposs, p) > (1- gamma)) {
    nposs <- nposs + 1
  }

  # 2. Calculate number of tickets using Normal Approximation
  norm <- qnorm(1 - gamma, mean = nposs * p, sd = sqrt(nposs * p * (1 - p))) + 0.5  # Approximate ticket count

  # 3. Define range of ticket sales for objective function plot
  n_values <- seq(N, N + 50, by = 1)  # Explore ticket sales above N

  # Compute objective functions
  obj_binom <- 1 - gamma - pbinom(N, n_values, p)
  obj_norm <- 1 - gamma - (pnorm((N - n_values * p) / sqrt(n_values * p * (1 - p))))

  # 4. Find where the function crosses zero for vertical lines
  n_binom_cross <- n_values[which.min(abs(obj_binom))]  # Closest n where obj_binom ~ 0
  n_norm_cross <- n_values[which.min(abs(obj_norm))]    # Closest n where obj_norm ~ 0

  # 5. Plot the Binomial Objective Function
  plot(n_values, obj_binom, type = "l", col = "blue", lwd = 2,
       xlab = "Number of Tickets Sold (n)", ylab = "Objective Function",
       main = "Objective Vs n - Discrete", ylim = c(min(obj_binom), max(obj_binom)))

  # Add a horizontal reference line at y = 0
  abline(h = 0, col = "black", lty = 3, lwd = 2)

  # Add vertical line where y crosses 0
  abline(v = n_binom_cross, col = "blue", lty = 2, lwd = 2)

  # 6. Plot the Normal Approximation Objective Function
  plot(n_values, obj_norm, type = "l", col = "red", lwd = 2,
       xlab = "Number of Tickets Sold (n)", ylab = "Objective Function",
       main = "Objective Vs n - Continuous", ylim = c(min(obj_norm), max(obj_norm)))

  # Add a horizontal reference line at y = 0
  abline(h = 0, col = "black", lty = 3, lwd = 2)

  # Add vertical line where y crosses 0
  abline(v = n_norm_cross, col = "red", lty = 2, lwd = 2)

  # 7. Return the computed values
  return(list(N = N, p = p, gamma = gamma, nd = n_binom_cross, nc = n_norm_cross))
}
