#' Plot Normal Distribution w/Shaded Area
#'
#' @param mu Numeric. Mean of the normal distribution.
#' @param sigma Numeric. Standard deviation of the normal distribution.
#' @param a Numeric. Value to calculate the probability P(X ≤ a).
#'
#' @returns List with the mean, standard deviation, value a, and probability P(X ≤ a).
#' @export
#'
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1.5)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  # Shade the area under the curve from -∞ to x=a
  xcurve = seq(mu-3*sigma, mu + 3*sigma, length.out = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)

  # Calculate the area under the curve from -∞ to x=a
  polygon(c(mu-3*sigma, xcurve, mu+3*sigma), c(0, ycurve, 0), col = "red")

  # Compute probability P(X ≤ a)
  prob = round(pnorm(a, mean=mu, sd=sigma), 4)

  # Print probabilty value
  text(a, dnorm(a, mean=mu, sd=sigma)/2, paste("P =", prob), pos=4)

  # Return list with results
  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
