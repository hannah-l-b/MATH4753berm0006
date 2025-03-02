#' Fire Damage Dataset
#'
#' This dataset contains information about fire damage incidents, specifically examining the relationship
#' between the distance of the nearest fire station and the resulting property damage.
#'
#' @format A data frame with `15` rows and `2` variables:
#' \describe{
#'   \item{DISTANCE}{(numeric) Distance from the nearest fire station (in miles).}
#'   \item{DAMAGE}{(numeric) Property damage due to fire (in dollars).}
#' }
#'
#' @source Simulated data for educational purposes.
#' @usage data(fire)
#' @examples
#' data(fire)
#' head(fire)
"fire"
