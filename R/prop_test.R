#' Proportion Test
#'
#' This function asks you a sequence of questions in order to execute a proportion test. It finds a confidence interval and a p-value, produces a plot, and indicates how this could be queried directly from R.
#' @export
#' @examples
#' > prop_test()
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' single
#' How many trials were there in your experiment?
#'   10
#' How many successes were there?
#'   5
#' The statistics for your dataset are:
#'   phat = 0.5
#' s = sqrt(0.5*(1-0.5)/10) = 0.1581139
#'
#' What is the theoretical proportion you are testing against (called p_0)?
#'   (If you only want a confidence interval, type 'NA')
#' .2
#' What is your desired confidence level?
#'   .9
#' The probability of getting this result or more extreme for phat
#' if the proportion really is 0.2 is
#' p =  0.0327935
#'
#' The 90% confidence interval for the population proportion is
#' 0.187086  < p <  0.812914
#'
#'
#' You can get this result by typing:
#'   binom.test(x = 5, n = 10, p = 0.2, alternative = 'two.sided', conf.level = 0.9)
#'
#'
#'
#'
#'
#'
#' > prop_test()
#' Do you have a single population or are you comparing populations?
#'   Possible answers are 'single' and 'comparing'.
#' comparing
#' How many trials were there in your first sample?
#'   15
#' How many successes were there in your first sample?
#'   10
#' How many trials were there in your second sample?
#'   20
#' How many successes were there in your second sample?
#'   10
#' The statistics for your dataset are:
#'   phat1 = 0.6666667
#' phat2 = 0.5
#' s = sqrt(0.6666667*(1-0.6666667)/15+0.5*(1-0.5)/20) = 0.1652719
#' What is your desired confidence level?
#'   .95
#' The probability of getting this result or more extreme for phat2 - phat1
#' if there really is no difference is
#' p =  0.521582
#'
#' The 95% confidence interval for the difference in proportions is
#' -0.5489271  < p2 - p1 <  0.2155937
#'
#'
#' You can get this result by typing:
#'   prop.test(c(10,10), c(20,15), alternative = 'two.sided', conf.level = 0.95)

prop_test <- function(){
  cat("Do you have a single population or are you comparing populations?\nPossible answers are 'single' and 'comparing'. \n")
  compare = readline()

  if(compare=="comparing"){
    cat("How many trials were there in your first sample? \n")
    n1 = as.numeric(readline())
    while(n1!=round(n1) | n1<1){cat('Please choose a whole number greater than zero\n')
      cat("How many trials were there in your first sample? \n")
      n1 = as.numeric(readline())
    }

    cat("How many successes were there in your first sample? \n")
    X1 = as.numeric(readline())
    while(X1!=round(X1) | X1<0 | X1>n1){cat(paste('Please choose a whole number greater than zero but less than ',toString(n1),".",sep=""))
      cat("How many successes were there in your first sample? \n")
      X1 = as.numeric(readline())
    }

    phat1 = X1/n1

    cat("How many trials were there in your second sample? \n")
    n2 = as.numeric(readline())
    while(n2!=round(n2) | n2<1){cat('Please choose a whole number greater than zero\n')
      cat("How many trials were there in your second sample? \n")
      n2 = as.numeric(readline())
    }

    cat("How many successes were there in your second sample? \n")
    X2 = as.numeric(readline())
    while(X2!=round(X2) | X2<0 | X2>n2){cat(paste('Please choose a whole number greater than zero but less than ',toString(n2),".\n",sep=""))
      cat("How many successes were there in your second sample? \n")
      X2 = as.numeric(readline())
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

    cat("What is your desired confidence level? \n")
    conf_level = as.numeric(readline())
    while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1\n')
      cat("What is your desired confidence level? \n")
      conf_level = as.numeric(readline())
    }

    out = prop.test(c(X2,X1),c(n2,n1),alternative="two.sided",conf.level=conf_level)
    normal_p(X2/n2-X1/n1, 0, sqrt(p*(1-p)*(1/n1+1/n2)), "both", print = FALSE)



    cat("The probability of getting this result or more extreme for phat2 - phat1\nif there really is no difference is")
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat(paste("The ",toString(conf_level*100),"% confidence interval for the difference in proportions is",sep=""))
    cat("\n")
    cat(paste(format(out$conf.int[1],scientific=FALSE)," < p2 - p1 < ",format(out$conf.int[2],scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(paste("prop.test(c(",format(X2,scientific=FALSE),",",format(X1,scientific=FALSE),"), c(",format(n2,scientific=FALSE),",",format(n1,scientific=FALSE),"), alternative = 'two.sided', conf.level = ",format(conf_level,scientific=FALSE), ")",sep=""))

}


  if(compare=="single"){
    cat("How many trials were there in your experiment? \n")
    n = as.numeric(readline())
    while(n!=round(n) | n<1){cat('Please choose a whole number greater than zero')
      cat("How many trials were there in your experiment? \n")
      n = as.numeric(readline())
    }

    cat("How many successes were there? \n")
    X = as.numeric(readline())
    while(X!=round(X) | X<0 | X>n){cat(paste('Please choose a whole number greater than zero but less than ',toString(n),".\n",sep=""))
      cat("How many successes were there? \n")
      X = as.numeric(readline())}

    phat = X/n
    s = sqrt(phat*(1-phat)/n)
    cat("The statistics for your dataset are: ")
    cat("\n")
    cat(paste("phat =",format(phat,scientific=FALSE)))
    cat("\n")
    cat(paste("s = sqrt(",format(phat,scientific=FALSE),"*(1-",format(phat,scientific=FALSE),")/",format(n,scientific=FALSE),") = ",format(s,scientific=FALSE),sep=""))

    cat("\n\nWhat is the theoretical proportion you are testing against (called p_0)? \n")
    cat("(If you only want a confidence interval, type 'NA')\n")
    p_0 = readline()
    if(p_0!="NA"){p_0=as.numeric(p_0)}
    cat("What is your desired confidence level? \n")
    conf_level = as.numeric(readline())
    while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1\n')
      cat("What is your desired confidence level? \n")
      conf_level = as.numeric(readline())
    }


    if(p_0!="NA"){
      sidedness = "two.sided"
      sidedness2 = "both"
      out = binom.test(X,n,p=p_0,alternative="two.sided")}

    if(p_0=="NA"){out = binom.test(X,n,p=0.5,alternative="two.sided",conf.level = conf_level)}



    if(p_0!="NA"){
    if(n>=100){
      normal_p(X/n, p_0, s, sidedness2, print = FALSE)
    }


    if(n<100){
      cols = rep("gray",n+1)

      if(X<=p_0*n){
        Y = min(c(p_0*n+(p_0*n-X),n))
        newX = X}
      if(X>=p_0*n){
        Y = X
        newX = max(c(p_0*n-(Y - p_0*n),0))
      }
        cols = rep("gray",n+1)
        if(newX>0){
          cols[1:(newX+1)] = rep("skyblue",newX+1)
        }
        cols[(Y+1):(n+1)] = rep("skyblue",n-Y+1)


      barplot(dbinom(0:n,n,p_0),names = c(0:n),col=cols)
    }
    }

    if(p_0!="NA"){
    cat(paste("The probability of getting this result or more extreme for phat\nif the proportion really is ",format(p_0,scientific=FALSE)," is ",sep=""))
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")}
    cat(paste("The ",toString(conf_level*100),"% confidence interval for the population proportion is",sep=""))
    cat("\n")
    cat(paste(format(out$conf.int[1],scientific=FALSE)," < p < ",format(out$conf.int[2],scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    if(p_0!="NA"){
      cat(paste("binom.test(x = ",format(X,scientific=FALSE),", n = ",format(n,scientific=FALSE),", p = ",format(p_0,scientific=FALSE),", alternative = '",sidedness,"', conf.level = ", format(conf_level,scientific=FALSE),")",sep=""))
    }
    if(p_0 == "NA"){
      cat(paste("binom.test(x = ",format(X,scientific=FALSE),", n = ",format(n,scientific=FALSE),", conf.level = ", format(conf_level,scientific=FALSE),")",sep=""))
    }




  }
}
