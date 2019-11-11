#' Guide the class in constructing confidence intervals
#'
#' Allows students to calculate their own sample mean and use it to make a confidence interval.
#' @export
confint_activity <- function(n = 9,numstudents = "N",conf = .95){
  
  if (numstudents == "N"){
    data(section)
    data(students)
    heights = students$Height[!is.na(students$Height)]
    
    cat(paste("The actual standard deviation is ",toString(round(sd(heights),2))," inches.\n\n",sep=""))
    

    
    for (i  in 1:length(section)){
      
      current = sample(heights,n)
      cat(paste(section[i],": ",toString(current),"\n",sep=""))

      
    }
    
    cat(paste("\nThe actual mean is ",toString(round(mean(heights),2))," inches.\n",sep=""))

    
  }
  
  if(numstudents != "N"){
    
    data(students)
    heights = students$Height[!is.na(students$Height)]
    sigma = sd(heights)
    mu = mean(heights)
    list = rep(0,numstudents)
    for (i  in 1:numstudents){
      
      current = sample(heights,n)
      list[i] = mean(current)
      
    }
    
    zstar = -qnorm((1-conf)/2)
    
    upper_bound = list + zstar*sigma/sqrt(n)
    lower_bound = list - zstar*sigma/sqrt(n)
    
    percent = round(sum(mu>lower_bound & mu<upper_bound)/numstudents*100,3)
    
    cat(paste(toString(percent),"% of students created a confidence interval that included the actual mean of ",toString(round(mean(heights),2))," inches.\n",sep=""))
    
  }
  
}