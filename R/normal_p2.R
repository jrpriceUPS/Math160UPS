#' Find the Proportion of Observations Between Two Points in a Normal Distribution
#'
#' This function is one of those chosen through prompts by normal(). If one is looking for the proportion of observations expected to fall between two values for a distribution that is normal, one can use this function to calculate and visualize that.
#' @param x the left endpoint of the interval
#' @param y the right endpoint of the interval
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distributions
#' @export
#' @examples
#' 
#' > normal_p2(-1.2,-0.8,0,1)
#' The proportion of observations between -1.2 and -0.8 is 0.09678573
#' 
#' You can get this result by typing:
#'   pnorm(-0.8,0,1)-pnorm(-1.2,0,1)


normal_p2 <- function(x, y, mu, sigma){
  
  if(y<x){
    x0 = x
    y0 = y
    x = y0
    y = x0
  }
  
  lowz = (x-mu)/sigma
  highz = (y-mu)/sigma
  
  if(lowz>=-2.75 ){
    leftend = mu - 3*sigma
  }
  if(lowz < -2.75){
    leftend = x-sigma
  }
  
  if(highz<=2.75){
    rightend = mu + 3*sigma
  }
  if(highz>2.75){
    rightend = y+sigma
  }
  
  spacing = 0.1*sigma
  xgrid = seq(leftend,rightend,by=spacing)
  
  thing_to_type= paste("pnorm(",toString(y),",",toString(mu),",",toString(sigma),")-pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  result = pnorm(y,mu,sigma)-pnorm(x,mu,sigma)
  
  
  
  cord.x <- c(x,seq(from=x,to=y,by=spacing),y,y) 
  cord.y <- c(0,dnorm(seq(from=x,to=y,by=spacing),mu,sigma),dnorm(y,mu,sigma),0) 
  
  
  
  
  
  
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  
  polygon(cord.x,cord.y,col='skyblue')
  abline(v = c(x,y),col='skyblue',lwd = 2)
  
  
  
  
  cat(paste("The proportion of observations between",toString(x),"and",toString(y),"is",format(result,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
}