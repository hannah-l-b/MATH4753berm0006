#' Read a CSV file
#'
#' @param csv A string specifying the CSV filename.
#'
#' @returns A data frame containing the contents of the CSV file.
#' @export
#'
#' @examples
#' dird = "data/"
#' myread("data.csv")
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
