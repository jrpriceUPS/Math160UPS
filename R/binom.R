binom <- function(){
  n = as.numeric(readline("How many trials are there in your binomial experiment? "))
  p = as.numeric(readline("What is the chance of success on one trial of your binomial experiment? "))
  x = as.numeric(readline("How many successes are you interested in? "))
  type = readline("Are you looking for the probability of getting more than this value, less than this value, or exactly this value? Possible answers are 'less', 'more', 'exactly', and 'between'. ")
  
  nums = 0:n
  cols = rep("gray",n+1)
  
  if(type == "less"){
    ans = pbinom(x-1,n,p)
    result = paste("The probability of getting less than ",toString(x)," successes in ",toString(n)," trials with probability of success ",toString(p)," each time is ",toString(ans),".",sep="")
    thing_to_type= paste("pbinom(",toString(x),"-1,",toString(n),",",toString(p),")",sep="")
    
    cols[1:x] = rep("skyblue",x)
  }
  
  if(type == "more"){
    ans = 1-pbinom(x,n,p)
    result = paste("The probability of getting more than ",toString(x)," successes in ",toString(n)," trials with probability of success ",toString(p)," each time is ",toString(ans),".",sep="")
    thing_to_type= paste("1-pbinom(",toString(x),",",toString(n),",",toString(p),")",sep="")
    
    cols[(x+2):n] = rep("skyblue",n-x-1)
  }
  
  if(type == "exactly"){
    ans = dbinom(x,n,p)
    result = paste("The probability of getting exactly ",toString(x)," successes in ",toString(n)," trials with probability of success ",toString(p)," each time is ",toString(ans),".",sep="")
    thing_to_type= paste("dbinom(",toString(x),",",toString(n),",",toString(p),")",sep="")
    cols[x+1 ] = "skyblue"
  }
  
  if(type == "between"){
    y = as.numeric(readline("What is the other number of successes you are interested in? "))
    if(y<x){
      ynew = x
      xnew = y
      y = ynew
      x = xnew
    }
    print(x)
    print(y)
    ans = pbinom(y,n,p)-pbinom(x-1,n,p)
    result = paste("The probability of getting between ",toString(x)," and ",toString(y)," successes in ",toString(n)," trials with probability of success ",toString(p)," each time is ",toString(ans),".",sep="")
    thing_to_type= paste("pbinom(",toString(y),",",toString(n),",",toString(p),")-","pbinom(",toString(x),"-1,",toString(n),",",toString(p),")",sep="")
    cols = rep("gray",n+1)
    cols[(x+1):(y+1)] = rep("skyblue",y-x+1)
  }
  
  barplot(dbinom(0:n,n,p),names = c(0:n),col=cols)
  
  cat(result)
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
}