#' Binomial Simulation Function
#'
#' @param iter Integer. The number of iterations/simulations to run. Default is 100.
#' @param n Integer. The number of trials in each iteration. Default is 10.
#' @param p Numeric. The probability of success in each trial. Default is 0.5.
#' @param plot Logical. Whether to plot the results. Default is TRUE.
#'
#' @returns A table of proportions showing the frequency of each number of successes across all iterations as well as a bar plot.
#' @export
#'
#' @examples
#' mybin()
#' mybin(iter = 1000, n = 10, p = 0.7)
mybin=function(iter=100,n=10, p=0.5, plot = TRUE){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  if (plot == TRUE) {
    barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
    succ.tab/iter
  }

  return(succ.tab / iter)

}
