#' Read a CSV file
#'
#' @param csv A string specifying the CSV filename.
#' @param dir A string specifying the directory containing the CSV file.
#'
#' @returns A data frame containing the contents of the CSV file.
#' @export
#'
#' @examples
#' myread("data.csv", dir = "path/to/directory")
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
