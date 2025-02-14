#' Negative Binomial Mass Function
#'
#' @param y Integer. Total number of trials (must be >= `r`).
#' @param r Integer. Number of successes.
#' @param p Numeric. Probability of success.
#'
#' @returns The probability of observing `r` successes in `y` trials, given a probability of success `p`.
#' @export
#'
#' @examples
#' mynbin(10, 3, 0.4) # Compute P(Y = 10) for a negative binomial with r = 3, p = 0.4
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

