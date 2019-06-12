#' Conduct a t-test using provided test statistic data.
#'
#' This function conducts a t-test, exports the results, exports what a user would need to type into R to get the same result, and produces a visualization.
#' @param t the t-statistic you are testing
#' @param df the degrees of freedom in your sample
#' @param type the type of test. Options are 'less', 'greater', and 'both'
#' @export
#' @examples
#' > conduct_t_test(3,10,"greater") 
#' $prob
#' [1] 0.006671828
#' 
#' $p_value_type
#' [1] "1-pt(3,10)"

conduct_t_test <- function(t, df, type){
  if(abs(t)<2.75){
    leftend = -3
    rightend = 3
    spacing = 0.1
    xgrid = seq(leftend,rightend,by=spacing)
  }
  if(abs(t)>2.75){
    
    if(t<=0){
      leftend = t-1
      if(type=="less" | type=="greater"){
        rightend = 3
      }
      if(type=="both"){
        rightend = 1-t
      }
      spacing = 0.01*(rightend-leftend)
      xgrid = seq(leftend,rightend,by=spacing)
    }
    
    if(t>0){
      if(type=="both"){
        leftend = -1-t
      }
      if(type=="less" | type=="greater"){
        leftend = -3
      }
      rightend = t+1
      spacing = 0.1*(rightend-leftend)
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  
  if(type == "both"){
    if(t<=0){
      thing_to_type= paste("2*pt(",toString(t),",",toString(df),")",sep="")
    }
    if(t>0){
      thing_to_type= paste("2*(1-pt(",toString(t),",",toString(df),"))",sep="")
    }
    t = abs(t)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=-t,by=spacing),-t,-t) 
    cord.y <- c(0,dt(seq(from=leftend,to=-t,by=spacing),df),dnorm(-t),0) 
    
    cord.x2 <- c(t,seq(from=t,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dt(seq(from=t,to=rightend,by=spacing),df),0) 
    
    result = pt(-abs(t),df)*2
    
    
  }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=t,by=spacing),t,t) 
    cord.y <- c(0,dt(seq(from=leftend,to=t,by=spacing),df),dt(t,df),0) 
    result = pt(t,df)
    thing_to_type= paste("pt(",toString(t),",",toString(df),")",sep="")
  }
  if(type == "greater"){
    cord.x <- c(t,seq(from=t,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dt(seq(from=t,to=rightend,by=spacing),df),0) 
    result = 1 - pt(t,df)
    thing_to_type= paste("1-pt(",toString(t),",",toString(df),")",sep="")
  }
  
  
  
  
  
  
  
  # Make a curve
  
  curve(dt(x,df), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(t,-t),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "greater"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = t,col='skyblue',lwd = 2)
  }
  
  list(prob=result,p_value_type=thing_to_type)
  
}
