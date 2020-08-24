#' Demo of Student's t-distribution
#'
#' This function allows you to produce samples for every student in a class as a way of seeing
#' the shape of Student's t-distribution.
#' @export
#' @examples

t_dist_demo = function(n = 4,numstudents = "N",section_name = "section"){
  students = get("students")
  sleep = students$Sleep[!is.na(students$Sleep)]
  
  trueMean = mean(sleep)
  trueSD = sd(sleep)/sqrt(n)
  
  
  if(numstudents == "N"){
    section = get(section_name)
    
    ts = rep(0,length(section))
    for (i  in 1:length(section)){
      
      current = sample(sleep,n)
      cat(paste(section[i],": ",toString(current),"\n",sep=""))
      ts[i] = (mean(current) - trueMean)/(sd(current)/sqrt(n))
      
    }
    cat("\nThe true mean is: ",sprintf("%1.3f", trueMean)," hours.\n\n",sep = "")
    cat("Calculate your t-statistic t = (xbar - ",sprintf("%1.3f", trueMean),") / (s / sqrt(",toString(n),"))\n\n",sep = "")
    
    viz = readline("Do you want to see a visualization?\n")
    
    if(viz == "Yes" | viz == "y" | viz == "yes" | viz == "Y"){
    hist(ts,freq = FALSE, breaks = 5, main = "Histogram of t-statistics", xlim = c(-7,7))
    
    xgrid = seq(from = -7,to = 7,length.out = 1000)
    
    
    lines(xgrid,dnorm(xgrid,0,1))
    
    lines(xgrid,dt(xgrid, df = n - 1), col = "red")
    }
    
  }
  
  
  
  
  if(numstudents != "N"){
  
  ts = rep(0,numstudents)
  for(i in 1:numstudents){
    mySample = sample(sleep,n, replace = TRUE)
    ts[i] = (mean(mySample) - trueMean)/(sd(mySample)/sqrt(n))
  }
  
  hist(ts,freq = FALSE, breaks = 20, main = "Histogram of t-statistics",xlim = c(-7,7))
  
  xgrid = seq(from = -7,to = 7,length.out = 1000)
  
  
  lines(xgrid,dnorm(xgrid,0,1))

  lines(xgrid,dt(xgrid, df = n - 1), col = "red")
  
  }
}