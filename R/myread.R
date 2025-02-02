#' Read a CSV file
#'
#' @param csv A string specifying the CSV filename.
#' @param dird A string specifying the directory where the CSV file is located.
#'
#' @returns A data frame containing the contents of the CSV file.
#' @export
#'
#' @examples
#' myread("data.csv")
myread=function(csv){
  fl=paste(dird,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
