#' Read a CSV file
#'
#' @param csv A string specifying the CSV filename.
#'
#' @returns A data frame containing the contents of the CSV file.
#' @export
#'
#' @examples
#' myread("data.csv")
myread=function(csv){
  # Define `dird` globally if not already defined
  if (!exists("dird", envir = .GlobalEnv)) {
    dird <<- getwd()  # Set default to current working directory
    message("Global variable 'dird' was not set. Using working directory: ", dird)
  }

  # Construct file path
  fl <- file.path(dird, csv)

  # Read the CSV file
  read.table(fl, header = TRUE, sep = ",")
}
