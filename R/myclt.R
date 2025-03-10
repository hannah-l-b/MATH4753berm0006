#' Central Limit Theorem Simulation
#'
#' @param n Integer. The number of uniform random variables to sum in each iteration.
#' @param iter Integer. The number of iterations (samples) to generate.
#' @param a Numeric. The lower bound of the uniform distribution. Default is 0.
#' @param b Numeric. The upper bound of the uniform distribution. Default is 5.
#'
#' @returns A numeric vector containing the sums of the sampled uniform random variables. Additionally, a histogram is plotted showing the distribution of the sums over all iterations, with an overlaid normal density curve.
#' @export
#'
#' @examples
#' myclt(n=50, iter=10000, a=5, b=10)
#'
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
