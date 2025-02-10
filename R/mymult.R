#' Multinomial Distribution Simulation
#'
#' @param iter Integer. Number of iterations to perform.
#' @param n Integer. Number of trials per iteration.
#' @param p Numeric vector. Probabilities of each category.
#'
#' @returns A matrix where each column represents the frequency distribution of categories for each iteration.
#' @export
#'
#' @examples
#' # Run the simulation with default parameters (4 categories with equal probabilities)
#' mymult()
#'
#' # Run the simulation with custom probabilities
#' mymult(iter = 200, n = 20, p = c(0.2, 0.3, 0.1, 0.4))
#'
#' # Run the simulation with 3 categories
#' mymult(iter = 150, n = 15, p = c(0.5, 0.3, 0.2))
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #The number of categories is k
  k=length(p)
  # Make a matrix that will hold the frequencies in each sample
  tab.mat=matrix(NA,nr=k,nc=iter, byrow=TRUE)


  for(i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    #Collect all the frequencies of each of the k values
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  # sum the frequecies
  freq=apply(tab.mat,1,sum)
  # put names to them
  names(freq)=1:k
  #create a barplot of refative freq
  barplot(freq/(n*iter),col=rainbow(k) )
  tab.mat
}
