#' Find the Value Such that Some Proportion of Observations Fall Above or Below That Point for a Normal Distribution
#'
#' This function is one of those chosen through prompts by normal(). If one is looking for the value such that a specificed proportion of observations are expected to fall above or below that value for a distribution that is normal, one can use this function to calculate and visualize that.
#' @param q the proportion of interest (a number between 0 and 1)
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distributions
#' @param type whether we are looking for the value with this proportion above it or below it. Options are 'less', 'more', or 'both'. 'both' could be used to find a point with that proportion more extreme than the output value.
#' @export
#' @examples
#' 
#' > normal_q(0.8,0,1,'more')
#' The proportion of observations with a value of -0.841621233572914 or more is 0.8
#' 
#' You can get this result by typing:
#'   qnorm(1-0.8,0,1)
#' 
#'
#' > normal_q(0.7,0,1,'less')
#' he proportion of observations with a value of 0.524400512708041 or less is 0.7
#' 
#' You can get this result by typing:
#'   qnorm(0.7,0,1)

normal_q <- function(q, mu, sigma, type){
  
  if(q < 0 | q > 1){
    stop('Please choose a proportion between 0 and 1')
  }
  
  
  if(type=="both"){x = qnorm(q/2,mu,sigma)
  thing_to_type = paste("qnorm(",toString(q),"/2,",toString(mu),",",toString(sigma),")",sep="")}
  
  if(type=="less"){x = qnorm(q,mu,sigma)
  thing_to_type = paste("qnorm(",toString(q),",",toString(mu),",",toString(sigma),")",sep="")}
  
  if(type=="more"){x = qnorm(1-q,mu,sigma)
  thing_to_type = paste("qnorm(1-",toString(q),",",toString(mu),",",toString(sigma),")",sep="")}
  
  
  
  
  
  
  
  
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
      if(type=="one"){
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
      if(type=="one"){
        leftend = mu - 3*sigma
      }
      rightend = x+sigma
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  # if(type == "both"){
  #   x = abs(x)
  #   
  #   
  #   cord.x <- c(leftend,seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu-abs(z)*sigma,mu-abs(z)*sigma) 
  #   cord.y <- c(0,dnorm(seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu,sigma),dnorm(mu-abs(z)*sigma,mu,sigma),0) 
  #   
  #   cord.x2 <- c(mu+abs(z)*sigma,seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),rightend) 
  #   cord.y2 <- c(0,dnorm(seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),mu,sigma),0) 
  #   
  #   result = pnorm(-abs(x),mu,sigma)*2
  #   
  #   
  # }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=x,by=spacing),x,x) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=x,by=spacing),mu,sigma),dnorm(x,mu,sigma),0)
  }
  
  if(type == "more"){
    cord.x <- c(x,seq(from=x,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dnorm(seq(from=x,to=rightend,by=spacing),mu,sigma),0)
  }
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  # if(type == "both"){
  #   polygon(cord.x,cord.y,col='skyblue')
  #   polygon(cord.x2,cord.y2,col='skyblue')
  #   abline(v = c(z*sigma+mu,-z*sigma+mu),col=c('skyblue','skyblue'),lwd = c(2,2))
  # }
  if(type == "less" | type == "more"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = x,col='skyblue',lwd = 2)
  }
  
  
  
  if(type == "less"){
  cat(paste("The proportion of observations with a value of",toString(x),"or less is",format(q,scientific=FALSE)))
  }
  if(type == "more"){
    cat(paste("The proportion of observations with a value of",toString(x),"or more is",format(q,scientific=FALSE)))
  }
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
  
}