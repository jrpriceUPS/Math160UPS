# If these functions aren't working, highlight this whole block and press Run


normal <- function(){
  
  prob = readline("Are you calculating a proportion or a value? Possible answers are 'proportion' or 'value'. ")
  
  if(prob=="proportion"){
    
    version = readline("Do you have one value or two values? Possible answers are 'one' and 'two'. ")
  
  
    if(version == "one"){
  x = as.numeric(readline("What is the value you are concerned with? "))
  mu = as.numeric(readline("What is the mean of your distribution? "))
  sigma = as.numeric(readline("What is the standard deviation of your distribution? "))
  type = readline("Do you want the proportion greater or less? Possible answers are 'greater' and 'less' ")
  
  normal_p(x,mu,sigma,type)
    }
  
  if(version == "two"){
    x = as.numeric(readline("What is the left value? "))
    y = as.numeric(readline("What is the right value? "))
    mu = as.numeric(readline("What is the mean of your distribution? "))
    sigma = as.numeric(readline("What is the standard deviation of your distribution? "))
    
    normal_p2(x,y,mu,sigma)
  }
    
  }
  
  if(prob=="value"){
    q = as.numeric(readline("What is the proportion you are interested in? "))
    mu = as.numeric(readline("What is the mean of your distribution? "))
    sigma = as.numeric(readline("What is the standard deviation of your distribution? "))
    type = readline("Do you want the point that has that proportion less than it, that proportion more than it, or that proportion more extreme than it? Possible answers are 'less', 'more', or 'extreme'. ")
    if(type=="extreme"){type="both"}
    normal_q(q,mu,sigma,type)
  }
  
  
}
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
normal_q <- function(q, mu, sigma, type){
  
  
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
  
  
  if(type == "both"){
    x = abs(x)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu-abs(z)*sigma,mu-abs(z)*sigma) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu,sigma),dnorm(mu-abs(z)*sigma,mu,sigma),0) 
    
    cord.x2 <- c(mu+abs(z)*sigma,seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dnorm(seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),mu,sigma),0) 
    
    result = pnorm(-abs(x),mu,sigma)*2
    
    
  }
  
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
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(z*sigma+mu,-z*sigma+mu),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "more"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = x,col='skyblue',lwd = 2)
  }
  
  
  
  
  cat(paste("The proportion of observations with a value of",toString(x),"or more extreme is",format(q,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
  
}
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