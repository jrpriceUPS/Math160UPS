#' Proportion Test
#'
#' This function asks you a sequence of questions in order to execute a proportion test. It finds a confidence interval and a p-value, produces a plot, and indicates how this could be queried directly from R.
#' @export
#' @examples
#' > prop_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. single
#' How many trials were there in your experiment? 10
#' How many successes were there? 5
#' The statistics for your dataset are: 
#'   phat = 0.5
#' s = sqrt(0.5*(1-0.5)/10) = 0.1581139
#' What is the theoretical proportion you are testing against (called p_0)? .2
#' What is your desired confidence level? .9
#' Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. two-sided
#' The probability of getting this result or more extreme for phat if the proportion really is 0.2 is 
#' p =  0.0327935
#' 
#' The 90% confidence interval for the population proportion is
#' 0.187086  < p <  0.812914
#' 
#' 
#' You can get this result by typing:
#'   binom.test(x = 0, n = 10, p = 0.2, alternative = 'two.sided', conf.level = 0.9)
#'   
#'   
#'   
#'   
#'   
#'   
#' > prop_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. comparing
#' How many trials were there in your first sample? 15
#' How many successes were there in your first sample? 10
#' How many trials were there in your second sample? 20
#' How many successes were there in your second sample? 10
#' The statistics for your dataset are: 
#'   phat1 = 0.6666667
#' phat2 = 0.5
#' s = sqrt(0.6666667*(1-0.6666667)/15+0.5*(1-0.5)/20) = 0.1652719
#' What is your desired confidence level? .95
#' Are you checking whether the proportion of the second population is less, greater, or different than the proportion of the first population? Possible answers are 'less', 'greater', and 'different'. greater
#' The probability of getting this result or more extreme for phat2 - phat1 if phat1 really is bigger than phat2 is
#' p =  0.739209
#' 
#' The 95% confidence interval for the difference in proportions is
#' -0.5489271  < p2 - p1 <  0.2155937
#' 
#' 
#' You can get this result by typing:
#'   prop.test(c(10,10), c(20,15), alternative = 'greater', conf.level = 0.95)
#' 
#' For the confidence interval, you would type:
#'   prop.test(c(10,10), c(20,15), alternative = 'two.sided',conf.level = 0.95)

prop_test <- function(){
  compare = readline("Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. ")
  
  if(compare=="comparing"){
    n1 = as.numeric(readline("How many trials were there in your first sample? "))
    while(n1!=round(n1) | n1<1){cat('Please choose a whole number greater than zero')
      n1 = as.numeric(readline("How many trials were there in your first sample? "))
    }
    
    X1 = as.numeric(readline("How many successes were there in your first sample? "))
    while(X1!=round(X1) | X1<0 | X1>n1){cat(paste('Please choose a whole number greater than zero but less than ',toString(n1),".",sep=""))
      X1 = as.numeric(readline("How many successes were there in your first sample? "))
    }
    
    phat1 = X1/n1
    
    n2 = as.numeric(readline("How many trials were there in your second sample? "))
    while(n2!=round(n2) | n2<1){cat('Please choose a whole number greater than zero')
      n2 = as.numeric(readline("How many trials were there in your second sample? "))
    }
    X2 = as.numeric(readline("How many successes were there in your second sample? "))
    while(X2!=round(X2) | X2<0 | X2>n2){cat(paste('Please choose a whole number greater than zero but less than ',toString(n2),".",sep=""))
      X2 = as.numeric(readline("How many successes were there in your second sample? "))
    }
    
    phat2 = X2/n2
    
    p = (X1 + X2)/(n1 + n2)
    s = sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
    cat("The statistics for your dataset are: ")
    cat("\n")
    cat(paste("phat1 =",format(phat1,scientific=FALSE)))
    cat("\n")
    cat(paste("phat2 =",format(phat2,scientific=FALSE)))
    cat("\n")
    cat(paste("s = sqrt(",format(phat1,scientific=FALSE),"*(1-",format(phat1,scientific=FALSE),")/",format(n1,scientific=FALSE),"+",format(phat2,scientific=FALSE),"*(1-",format(phat2,scientific=FALSE),")/",format(n2,scientific=FALSE),") = ",format(s,scientific=FALSE),sep=""))
    cat("\n")
    
    conf_level = as.numeric(readline("What is your desired confidence level? "))
    while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1')
      conf_level = as.numeric(readline("What is your desired confidence level? "))
    }
    sidedness = readline("Are you checking whether the proportion of the second population is less, greater, or different than the proportion of the first population? Possible answers are 'less', 'greater', and 'different'. ")
    
    sidedness2=sidedness
    if(sidedness=="different"){
      sidedness = "two.sided"
      sidedness2 = "both"
    }
    
    out = prop.test(c(X2,X1),c(n2,n1),alternative=sidedness,conf.level=conf_level)
    out_conf = prop.test(c(X2,X1),c(n2,n1),alternative="two.sided",conf.level=conf_level)
    normal_p(X2/n2-X1/n1, 0, sqrt(p*(1-p)*(1/n1+1/n2)), sidedness2, print = FALSE)
    
    if(sidedness=="two.sided"){
      first_statement = paste("The probability of getting this result or more extreme for phat2 - phat1 if there really is no difference is")
    }
    if(sidedness=="less"){
      first_statement = paste("The probability of getting this result or more extreme for phat2 - phat1 if phat1 really is bigger than phat2 is")
    }
    if(sidedness=="greater"){
      first_statement = paste("The probability of getting this result or more extreme for phat2 - phat1 if phat1 really is bigger than phat2 is")
    }
    
    
    cat(first_statement)
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat(paste("The ",toString(conf_level*100),"% confidence interval for the difference in proportions is",sep=""))
    cat("\n")
    cat(paste(format(out_conf$conf.int[1],scientific=FALSE)," < p2 - p1 < ",format(out_conf$conf.int[2],scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(paste("prop.test(c(",format(X2,scientific=FALSE),",",format(X1,scientific=FALSE),"), c(",format(n2,scientific=FALSE),",",format(n1,scientific=FALSE),"), alternative = '",sidedness,"', conf.level = ",format(conf_level,scientific=FALSE), ")",sep=""))
    
    if(sidedness!="two.sided"){
      cat("\n")
      cat("\n")
      cat("For the confidence interval, you would type:")
      cat("\n")
      cat(paste("prop.test(c(",format(X2,scientific=FALSE),",",format(X1,scientific=FALSE),"), c(",format(n2,scientific=FALSE),",",format(n1,scientific=FALSE),"), alternative = 'two.sided',", "conf.level = ",format(conf_level,scientific=FALSE), ")",sep=""))
    }
  }
  
  if(compare=="single"){
    
    n = as.numeric(readline("How many trials were there in your experiment? "))
    while(n!=round(n) | n<1){cat('Please choose a whole number greater than zero')
      n = as.numeric(readline("How many trials were there in your experiment? "))
    }
    
    X = as.numeric(readline("How many successes were there? "))
    while(X!=round(X) | X<0 | X>n){cat(paste('Please choose a whole number greater than zero but less than ',toString(n),".",sep=""))
      X = as.numeric(readline("How many successes were there? "))
      
    phat = X/n
    s = sqrt(phat*(1-phat)/n)
    cat("The statistics for your dataset are: ")
    cat("\n")
    cat(paste("phat =",format(phat,scientific=FALSE)))
    cat("\n")
    cat(paste("s = sqrt(",format(phat,scientific=FALSE),"*(1-",format(phat,scientific=FALSE),")/",format(n,scientific=FALSE),") = ",format(s,scientific=FALSE),sep=""))
    
    
    p_0 = as.numeric(readline("What is the theoretical proportion you are testing against (called p_0)? "))
    conf_level = as.numeric(readline("What is your desired confidence level? "))
    sidedness = readline("Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. ")
    sidedness2=sidedness
    if(sidedness=="two-sided"){
      sidedness = "two.sided"
      sidedness2 = "both"
    }
    
    out = binom.test(X,n,p=p_0,alternative=sidedness)
    out_conf = binom.test(X,n,p=p_0,alternative="two.sided")
    
    if(n>=100){
      normal_p(X/n, p_0, s, sidedness2, print = FALSE)
    }
    
    
    if(n<100){
      cols = rep("gray",n+1)
      
      if(sidedness == "less"){
        cols[1:(X+1)] = rep("skyblue",X+1)
      }
      
      if(sidedness == "greater"){
        cols[(X+1):(n+1)] = rep("skyblue",n-X+1)
      }
      
      if(sidedness == "two.sided"){
        if(X<=p_0*n){
          Y = min(c(p_0*n+(p_0*n-X),n))
        }
        if(X>=p_0*n){
          Y = X
          X = max(c(p_0*n-(Y - p_0*n),0))
        }
        cols = rep("gray",n+1)
        cols[1:(X+1)] = rep("skyblue",X+1)
        cols[(Y+1):(n+1)] = rep("skyblue",n-Y+1)
      }
      
      barplot(dbinom(0:n,n,p_0),names = c(0:n),col=cols)
    }
    
    
    
    cat(paste("The probability of getting this result or more extreme for phat if the proportion really is ",format(p_0,scientific=FALSE)," is ",sep=""))
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat(paste("The ",toString(conf_level*100),"% confidence interval for the population proportion is",sep=""))
    cat("\n")
    cat(paste(format(out_conf$conf.int[1],scientific=FALSE)," < p < ",format(out_conf$conf.int[2],scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(paste("binom.test(x = ",format(X,scientific=FALSE),", n = ",format(n,scientific=FALSE),", p = ",format(p_0,scientific=FALSE),", alternative = '",sidedness,"', conf.level = ", format(conf_level,scientific=FALSE),")",sep=""))
    
    if(sidedness!="two.sided"){
      cat("\n")
      cat("\n")
      cat("For the confidence interval, you would type:")
      cat("\n")
      cat(paste("binom.test(x = ",format(X,scientific=FALSE),", n = ",format(n,scientific=FALSE),", p = ",format(p_0,scientific=FALSE),", alternative = 'two.sided', conf.level = ", format(conf_level,scientific=FALSE),")",sep=""))
    }
  }
  
  
  
  
}