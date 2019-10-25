#' Give everyone a sample of n heights
#'
#' Allows students to calculate their own sample mean and see the sampling distribution in action.
#' @export
sampling_dist_activity <- function(n = 3,numstudents = "N"){
  
  if (numstudents == "N"){
  data(section)
  data(students)
  heights = students$Height[!is.na(students$Height)]
  
  list = rep(0,length(section))
  
  for (i  in 1:length(section)){
    
    current = sample(heights,n)
    cat(paste(section[i],": ",toString(current),"\n",sep=""))
    list[i] = mean(current)
    
  }
  
  
  
  x <- readline("Do you want me to make a histogram? ")
  
  if (x == "Y"){
    hist(list,xlab = "Height (in)",ylab = "Number of students", main = "An approximation of the sampling distribution")
  }
  
  }
  
  if(numstudents != "N"){
    
    data(students)
    heights = students$Height[!is.na(students$Height)]
    list = rep(0,numstudents)
    for (i  in 1:numstudents){
      
      current = sample(heights,n)
      list[i] = mean(current)
      
    }
    hist(list,xlab = "Height (in)",ylab = "Number of students", main = "An approximation of the sampling distribution")
  }
  
  cat(paste("The actual mean is ",toString(mean(heights))," inches.\n",sep=""))

}