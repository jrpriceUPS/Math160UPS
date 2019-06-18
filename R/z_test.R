#' Z-test
#'
#' This function asks you a sequence of questions in order to execute a z-test. It finds a confidence interval and a p-value, produces a plot, and indicates how this could be queried directly from R.
#' @export
#' @examples
#' 
#' *******
#' Z-TESTS
#' *******
#' 
#' > z_test()
#' What is the *population* standard deviation? 4
#' What is your sample mean? 8
#' What is the theoretical mean you are testing against (called mu_0)? 7.5
#' What is your sample size? 36
#' What is your desired confidence level? .90
#' Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. two-sided
#' The probability of getting this result or more extreme for xbar if mu really is 7.5 is
#' p =  0.4532547
#' 
#' You can get this result by typing:
#'   2*(1-pnorm(8,7.5,4/sqrt(36))
#'      
#'      
#'      The 90% confidence interval for the population mean is
#'      6.903431  < mu <  9.096569
#'      
#'      You can get this result by finding:
#'        zstar = 1-qnorm((1-0.9)/2,0,1) = 1.644854
#'      
#'      and then calculating:
#'        8 - 1.644854 x 4/sqrt(36)  and  8 + 1.644854 x 4/sqrt(36)
#'        
#'        
#'        
#'        
#'        
#'        
#' > z_test()
#' What is the *population* standard deviation? 4
#' What is your sample mean? 8
#' What is the theoretical mean you are testing against (called mu_0)? 7.5
#' What is your sample size? 16
#' What is your desired confidence level? .99
#' Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. less
#' The probability of getting this result or more extreme for xbar if mu really is 7.5 is
#' p =  0.6914625
#' 
#' You can get this result by typing:
#'   pnorm(8,7.5,4/sqrt(16))
#' 
#' 
#' The 99% confidence interval for the population mean is
#' 5.424171  < mu <  10.57583
#' 
#' You can get this result by finding:
#'   zstar = 1-qnorm((1-0.99)/2,0,1) = 2.575829
#' 
#' and then calculating:
#'   8 - 2.575829 x 4/sqrt(16)  and  8 + 2.575829 x 4/sqrt(16)

z_test <- function(){
  sigma = as.numeric(readline("What is the *population* standard deviation? "))
  xbar = as.numeric(readline("What is your sample mean? "))
  mu_0 = readline("What is the theoretical mean you are testing against (called mu_0)? ")
  if(mu_0!="NA"){mu_0 = as.numeric(mu_0)}
  n = as.numeric(readline("What is your sample size? "))
  conf_level = as.numeric(readline("What is your desired confidence level? "))
  while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1')
    conf_level = as.numeric(readline("What is your desired confidence level? "))
  }
  if(mu_0!="NA"){
  sidedness = readline("Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. ")
  }
  
  s = sigma/sqrt(n)
  
  # new always the same confidence interval
  zstar = -qnorm((1-conf_level)/2,0,1)
  
  thing_to_type2 = paste("zstar = 1-qnorm((1-",format(conf_level,scientific=FALSE),")/2,0,1) = ",format(zstar,scientific=FALSE),sep="")
  thing_to_type3 = paste(toString(xbar)," - ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
  thing_to_type4 = paste(toString(xbar)," + ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
  
  lower = xbar - zstar*s
  upper = xbar + zstar*s
  
  if(mu_0!="NA"){
  z = (xbar-mu_0)/s
  if(z<=0){
    thing_to_type1 = paste("2*pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
  }
  if(z>0){
    thing_to_type1 = paste("2*(1-pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
  }
  
  if(sidedness == "two-sided"){
    out = normal_p(xbar, mu_0, s, "both", FALSE)
    # zstar = -qnorm((1-conf_level)/2,0,1)
    # 
    # thing_to_type2 = paste("zstar = 1-qnorm((1-",format(conf_level,scientific=FALSE),")/2,0,1) = ",format(zstar,scientific=FALSE),sep="")
    # thing_to_type3 = paste(toString(xbar)," - ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    # thing_to_type4 = paste(toString(xbar)," + ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    # 
    # lower = xbar - zstar*s
    # upper = xbar + zstar*s
    
    
    z = (xbar-mu_0)/s
    if(z<=0){
      thing_to_type1 = paste("2*pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
    }
    if(z>0){
      thing_to_type1 = paste("2*(1-pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
    }
    
    
  }
  
  if(sidedness == "less"){
    out = normal_p(xbar, mu_0, s, "less", FALSE)
    
    # zstar = qnorm(conf_level,0,1)
    # 
    # thing_to_type2 = paste("zstar = qnorm(",format(conf_level,scientific=FALSE),") = ",format(zstar,scientific=FALSE),sep="")
    # thing_to_type3 = "-Infinity"
    # thing_to_type4 = paste(toString(xbar)," + ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    # 
    # lower = -Inf
    # upper = xbar + zstar*s
    
    thing_to_type1 = paste("pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
    
    
    
    
  }
  
  if(sidedness == "greater"){
    out = normal_p(xbar, mu_0, s, "greater", FALSE)
    
    # zstar = qnorm(conf_level,0,1)
    # 
    # thing_to_type2 = paste("zstar = qnorm(1-",format(conf_level,scientific=FALSE),") = ",format(zstar,scientific=FALSE),sep="")
    # thing_to_type3 = paste(toString(xbar)," - ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    # thing_to_type4 = "Infinity"
    # 
    # lower = xbar - zstar*s
    # upper = Inf
    
    thing_to_type1 = paste("1-pnorm(",toString(xbar),",",toString(mu_0),",",toString(sigma),"/sqrt(",toString(n),"))",sep="")
    
  }

  cat(paste("The probability of getting this result or more extreme for xbar if mu really is ",toString(mu_0)," is",sep=""))
  cat("\n")
  cat(paste("p = ",format(out$prob,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type1)
  cat("\n")
  cat("\n")
  cat("\n")}
  cat(paste("The ",toString(conf_level*100),"% confidence interval for the population mean is",sep=""))
  cat("\n")
  cat(paste(format(lower,scientific=FALSE)," < mu < ",format(upper,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by finding:")
  cat("\n")
  cat(thing_to_type2)
  cat("\n")
  cat("\n")
  cat("and then calculating:")
  cat("\n")
  cat(paste(thing_to_type3," and ",thing_to_type4))
}