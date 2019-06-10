#' Find the Proportion of Observations Above or Below a Point for a Normal Distribution
#'
#' This function is one of those chosen through prompts by normal(). If one is looking for the proportion of observations expected to fall above or below a particular value for a distribution that is normal, one can use this function to calculate and visualize that.
#' @param x the point of interest
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distributions
#' @param type whether we are looking for the proportion above or below the chosen point. Options are 'less', 'greater', or 'both'. 'both' is used by the z-test function to find more extreme points.
#' @param print whether or not to print output to the screen.  Defaults to TRUE. Typically we want to print the results, but this is also called by the z-test function, in which case we wish to print something else.
#' @return the probability of finding a value in the desired interval and the pnorm() command to find that result.
#' @export
#' @examples
#' 
#' > output = normal_p(-1.5,0,1,'less',TRUE)
#' The proportion of observations with a value of -1.5 or more extreme is 0.0668072
#' 
#' You can get this result by typing:
#'  pnorm(-1.5,0,1)
#' 
#'
#' > output = normal_p(-1.5,0,1,'greater',TRUE)
#' The proportion of observations with a value of -1.5 or more extreme is 0.9331928
#' 
#' You can get this result by typing:
#'   1-pnorm(-1.5,0,1) 
normal_p <- function(x, mu, sigma, type, print = TRUE){
  
  z = (x-mu)/sigma
  
  if(abs(z)<2.75 ){
    leftend = mu - 3*sigma
    rightend = mu+3*sigma
    spacing = 0.1*sigma
    xgrid = seq(leftend,leftend,by=spacing)
  }
  
  if(abs(z)>2.75 ){
    
    if(z<=0){
      leftend = x-sigma
      if(type=="less" | type=="greater"){
        rightend = mu+3*sigma
      }
      if(type=="both"){
        rightend = mu+(abs(z)+1)*sigma
      }
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
    
    if(z>0){
      if(type=="both"){
        leftend = mu-sigma*(abs(z)+1)
      }
      if(type=="less" | type=="greater"){
        leftend = mu - 3*sigma
      }
      rightend = x+sigma
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  if(type == "both"){
    if(z<=0){
      thing_to_type= paste("2*pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
    }
    if(z>0){
      thing_to_type= paste("2*(1-pnorm(",toString(x),",",toString(mu),",",toString(sigma),"))",sep="")
    }
    z = abs(z)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu-abs(z)*sigma,mu-abs(z)*sigma) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu,sigma),dnorm(mu-abs(z)*sigma,mu,sigma),0) 
    
    cord.x2 <- c(mu+abs(z)*sigma,seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dnorm(seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),mu,sigma),0) 
    
    
    result = pnorm(-abs(z),0,1)*2
    
    
  }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=x,by=spacing),x,x) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=x,by=spacing),mu,sigma),dnorm(x,mu,sigma),0) 
    result = pnorm(x,mu,sigma)
    thing_to_type= paste("pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  }
  if(type == "greater"){
    cord.x <- c(x,seq(from=x,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dnorm(seq(from=x,to=rightend,by=spacing),mu,sigma),0) 
    result = 1 - pnorm(x,mu,sigma)
    thing_to_type= paste("1-pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  }
  
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(z*sigma+mu,-z*sigma+mu),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "greater"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = x,col='skyblue',lwd = 2)
  }
  
  
  if(print){
    cat(paste("The proportion of observations with a value of",toString(x),"or more extreme is",format(result,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(thing_to_type)
  }
  
  list(prob=result,p_value_type=thing_to_type)
}