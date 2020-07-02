#' Binomial Distribution Calculations
#'
#' This function allows to compute quantities relating to binomial distributions and visualize the results clearly.
#' @export
#' @examples
#' 
#' > binom()
#' How many trials are there in your binomial experiment? 13
#' What is the chance of success on one trial of your binomial experiment? 0.3
#' How many successes are you interested in? 4
#' Are you looking for the probability of getting more than this value, less than this value, exactly this value, or between this value and another value? Possible answers are 'less', 'more', 'exactly', and 'between'. less
#' The probability of getting less than 4 successes in 13 trials with probability of success 0.3 each time is 0.420605645761.
#' 
#' You can get this result by typing:
#'   pbinom(4-1,13,0.3)
#'   
#'   
#' > binom()
#' How many trials are there in your binomial experiment? 15
#' What is the chance of success on one trial of your binomial experiment? .4
#' How many successes are you interested in? 8
#' Are you looking for the probability of getting more than this value, less than this value, exactly this value, or between this value and another value? Possible answers are 'less', 'more', 'exactly', and 'between'. more
#' The probability of getting more than 8 successes in 15 trials with probability of success 0.4 each time is 0.0950474081566721.
#' 
#' You can get this result by typing:
#'   1-pbinom(8,15,0.4)
#'   
#'   
#' > binom()
#' How many trials are there in your binomial experiment? 12
#' What is the chance of success on one trial of your binomial experiment? 0.8
#' How many successes are you interested in? 9
#' Are you looking for the probability of getting more than this value, less than this value, exactly this value, or between this value and another value? Possible answers are 'less', 'more', 'exactly', and 'between'. exactly
#' The probability of getting exactly 9 successes in 12 trials with probability of success 0.8 each time is 0.23622320128.
#' 
#' You can get this result by typing:
#'   dbinom(9,12,0.8)
#'   
#'   
#' > binom()
#' How many trials are there in your binomial experiment? 16
#' What is the chance of success on one trial of your binomial experiment? 0.6
#' How many successes are you interested in? 10
#' Are you looking for the probability of getting more than this value, less than this value, exactly this value, or between this value and another value? Possible answers are 'less', 'more', 'exactly', and 'between'. between
#' What is the other number of successes you are interested in? 14
#' The probability of getting between 10 and 14 successes in 16 trials with probability of success 0.6 each time is 0.523882818699263.
#' 
#' You can get this result by typing:
#'   pbinom(14,16,0.6)-pbinom(10-1,16,0.6)



binom <- function(){
  n = as.numeric(readline("How many trials are there in your binomial experiment? "))
  while(n!=round(n) | n<1){cat('Please choose a whole number greater than zero')
    n = as.numeric(readline("How many trials are there in your binomial experiment? "))
  }
  
  p = as.numeric(readline("What is the chance of success on one trial of your binomial experiment? "))
  while(p<0 | p>1){cat('Please choose a probability between 0 and 1')
    p = as.numeric(readline("What is the chance of success on one trial of your binomial experiment? "))
  }
  
  x = as.numeric(readline("How many successes are you interested in? "))
  while(x!=round(x) | x<0 | x>n){cat(paste('Please choose a whole number greater than zero but less than ',toString(n),".",sep=""))
    x = as.numeric(readline("How many successes are you interested in? "))
  }
  
  type = readline("Are you looking for the probability of getting more than this value, less than this value, exactly this value, or between this value and another value? Possible answers are 'less', 'more', 'exactly', and 'between'. ")
  
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
    
    cols[(x+2):n+1] = rep("skyblue",n-x-1)
  }
  
  if(type == "exactly"){
    ans = dbinom(x,n,p)
    result = paste("The probability of getting exactly ",toString(x)," successes in ",toString(n)," trials with probability of success ",toString(p)," each time is ",toString(ans),".",sep="")
    thing_to_type= paste("dbinom(",toString(x),",",toString(n),",",toString(p),")",sep="")
    cols[x+1 ] = "skyblue"
  }
  
  if(type == "between"){
    y = as.numeric(readline("What is the other number of successes you are interested in? "))
    while(y!=round(y) | y<0 | y>n){cat(paste('Please choose a whole number greater than zero but less than ',toString(n),".",sep=""))
      y = as.numeric(readline("What is the other number of successes you are interested in? "))
    }
    if(y<x){
      ynew = x
      xnew = y
      y = ynew
      x = xnew
    }
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
