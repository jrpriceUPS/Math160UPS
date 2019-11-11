#' Guide the class in conducting hypothesis tests
#'
#' Allows students to calculate their own sample mean and use it to do a significance test.
#' @export
hyp_test_activity <- function(n = 9,numstudents = "N"){
  sigma = 2.94
  mu0 = 69.17
  
  if (numstudents == "N"){
    data(section)
    data(students)
    heights = students$Height[!is.na(students$Height)]
    
    
    
    for (i  in 1:length(section)){
      
      current = sample(heights,n)
      cat(paste(section[i],": ",toString(current),"\n",sep=""))
      
      
    }
    
    cat(paste("\nThe national standard deviation is ",toString(sigma)," inches.\n",sep=""))
    cat(paste("The national average height is ",toString(mu0)," inches.\n",sep=""))
    cat(paste("Each sample has ",toString(n)," observations.\n\n",sep=""))
    
    
  }
  
  if(numstudents != "N"){
    
    data(students)
    heights = students$Height[!is.na(students$Height)]
    sigma = sd(heights)
    mu = mean(heights)
    list = rep(0,numstudents)
    prob = rep(0,numstudents)
    for (i  in 1:numstudents){
      
      current = sample(heights,n)
      list[i] = mean(current)
      prob[i] = 2*pnorm(list[i],mu0,sigma/sqrt(n))
      sum(prob<0.05)/2800
      
    }
    
    percent = sum(prob<0.05)/numstudents
    
    cat(paste(toString(round(percent*100,2)),"% of students found their result had a probability of below 0.05 under the null hypothesis.\n",sep=""))
    
  }
  
}