#' Calculate the Mode of a Vector
#'
#' @param v A numeric or character vector.
#'
#' @returns The mode (most frequently occurring value) of the vector.
#' @export
#'
#' @examples
#' getmode(c(1, 2, 2, 3, 3, 3, 4)) # Returns 3
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
