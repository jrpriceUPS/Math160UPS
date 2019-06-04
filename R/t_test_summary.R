t.test.summary <- function(xbar,s,n,conf.level=0.95,mu=0,alternative="not equal") {
  error = s/sqrt(n)
  df = n-1
  tstar = -qt((1-conf.level)/2,df)
  lower = xbar - tstar*error
  upper = xbar + tstar*error
  t = -abs((xbar - mu)/error)
  print(t)
  if (alternative=="not equal"){p = pt(t,df)}
  
  x = seq(3*t,-3*t,by=0.1)
  
  cord.x <- c(3*t,seq(from=3*t,to=t,by=0.01),t) 
  cord.y <- c(0,dt(seq(from=3*t,to=t,by=0.01),df),0) 
  
  cord.x2 <- c(-t,seq(from=-t,to=-3*t,by=0.01),-3*t) 
  cord.y2 <- c(0,dt(seq(from=-t,to=-3*t,by=0.01),df),0) 
  
  # Make a curve
  curve(dt(x,df), xlim=c(3*t,-3*t),xlab="",ylab="",xaxt="n",yaxt="n")
  
  # Add the shaded area.
  polygon(cord.x,cord.y,col='skyblue')
  polygon(cord.x2,cord.y2,col='skyblue')
  
  list(lower.limit=lower,upper.limit=upper,p_value=p)
}
