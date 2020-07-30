#' Chi-Squared Test
#'
#' This function asks you a sequence of questions in order to execute a proportion test. It finds a p-value, produces a plot, and indicates how this could be queried directly from R.
#' @export
#' @examples
#' #' > chi_test()
#' Are you comparing two distributions or checking goodness of fit? Possible answers are 'comparing' and 'goodness'. comparing
#' How many rows are there in your table? 3
#' How many columns are there in your table? 2
#' What is the entry in row 1 and column 1? 10
#' What is the entry in row 2 and column 1? 9
#' What is the entry in row 3 and column 1? 8
#' What is the entry in row 1 and column 2? 5
#' What is the entry in row 2 and column 2? 6
#' What is the entry in row 3 and column 2? 19
#' The expected data were:
#'   [,1]      [,2]
#' [1,]  7.105263  7.894737
#' [2,]  7.105263  7.894737
#' [3,] 12.789474 14.210526
#' 
#' but you observed:
#'   [,1] [,2]
#' [1,]   10    5
#' [2,]    9    6
#' [3,]    8   19
#' 
#' Your chi-squared-statistic is:
#'   X^2  = 6.60856
#' 
#' The degrees of freedom are:
#'   df  = 2
#' 
#' The probability of getting this result or more extreme if there really is no relationship is
#' p =  0.03672565
#' 
#' You can get this result by:
#' 
#'   Inputting the data in the table in a list that goes column-by-column:
#'   data  = c(10,9,8,5,6,19)
#' 
#' Then converting that into a matrix:
#'   A  = matrix(data,nrow = 3)
#' 
#' Then using that to run the test:
#'   chisq.test(A)
#'   
#'   
#'   
#'   
#'   
#'   
#' > chi_test()
#' Are you comparing two distributions or checking goodness of fit? Possible answers are 'comparing' and 'goodness'. goodness
#' How many categories are there in your distribution? 6
#' What is entry number 1 in your sample? 10
#' What is entry number 2 in your sample? 8
#' What is entry number 3 in your sample? 14
#' What is entry number 4 in your sample? 9
#' What is entry number 5 in your sample? 5
#' What is entry number 6 in your sample? 16
#' Is your hypothesis that all categories are equally likely? yes
#' The expected data were:
#'   [1] 10.33333 10.33333 10.33333 10.33333 10.33333 10.33333
#' 
#' but you observed:
#'   [1] 10  8 14  9  5 16
#' 
#' Your chi-squared-statistic is:
#'   X^2  = 7.870968
#' 
#' The degrees of freedom are:
#'   df  = 5
#' 
#' The probability of getting this result or more extreme if the distribution is really the theoretical one
#' p =  0.1634917
#' 
#' You can get this result by:
#' 
#'   Inputting the sample data in a list:
#'   data  = c(10,8,14,9,5,16)
#' 
#' and also recording the theoretical data:
#'   prob  = c(0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667,0.166666666666667)
#' 
#' Then using that to run the test:
#'   chisq.test(data,p = prob)

chi_test <- function(){
  cat("Are you comparing two distributions or checking goodness of fit?\nPossible answers are 'comparing' and 'goodness'.\n")
  compare = readline()
  
  if(compare=="comparing"){
    cat("How many rows are there in your table?\n")
    m = as.numeric(readline())
    cat("How many columns are there in your table?\n")
    n = as.numeric(readline())
    
    nrow = 1:m
    ncol = 1:n
    x = rep(0,m*n)
    count = 0
    for(j in ncol){
      for(i in nrow){
        count = count + 1
        cat(paste("What is the entry in row ",format(i,scientific=FALSE)," and column ",format(j,scientific=FALSE),"?\n",sep=""))
        x[count] = as.numeric(readline())
      }
    }
    A = matrix(x,nrow=m)
    out = chisq.test(A)
    
    cat("The expected data were:")
    cat("\n")
    print(out$expected)
    cat("\n")
    
    cat("but you observed:")
    cat("\n")
    print(out$observed)
    cat("\n")
    
    cat("Your chi-squared-statistic is:")
    cat("\n")
    cat(paste("X^2  = ",format(out$statistic,scientific=FALSE),sep=""))
    cat("\n")
    cat("\n")
    cat("The degrees of freedom are:")
    cat("\n")
    cat(paste("df  = ",format(out$parameter,scientific=FALSE),sep=""))
    cat("\n")
    cat("\n")
    cat(paste("The probability of getting this result or more extreme\nif there really is no relationship is",sep=""))
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("You can get this result by:")
    cat("\n")
    cat("\n")
    
    collapsed = paste(x,collapse=",")
    
    cat("Inputting the data in the table in a list that goes column-by-column:")
    cat("\n")
    cat(paste("data  = c(",collapsed,")",sep=""))
    cat("\n")
    cat("\n")
    cat(paste("Then converting that into a matrix:",sep=""))
    cat("\n")
    cat(paste("A  = matrix(data,nrow = ",format(m,scientific=FALSE),")",sep=""))
    cat("\n")
    cat("\n")
    cat("Then using that to run the test:")
    cat("\n")
    cat("chisq.test(A)")
    
    
    
    
    
    
    
    
    
  }
  if(compare=="goodness"){
    cat("How many categories are there in your distribution?\n")
    m = as.numeric(readline())
    
    nrow = 1:m
    x = rep(0,m)
    count = 0
    for(i in nrow){
      cat(paste("What is entry number ",format(i,scientific=FALSE)," in your sample?\n",sep=""))
      x[i] = as.numeric(readline())
    }
    
    cat("Is your hypothesis that all categories are equally likely?\n")
    same = readline()
    if(same=="yes" | same == "Yes" | same == "y" | same == "Y"){p = rep(1/m,m)}
    if(same=="no" | same == "No" | same == "N" | same == "n"){
      p = rep(0,m)
      count = 0
      for(i in nrow){
        cat(paste("What is the theoretical probability of entry ",format(i,scientific=FALSE),"?\n",sep=""))
        p[i] = as.numeric(readline())
      }
    }
    out = suppressWarnings(chisq.test(x,p=p))
    
    cat("The expected data were:")
    cat("\n")
    print(out$expected)
    cat("\n")
    
    cat("but you observed:")
    cat("\n")
    print(out$observed)
    cat("\n")
    
    cat("Your chi-squared-statistic is:")
    cat("\n")
    cat(paste("X^2  = ",format(out$statistic,scientific=FALSE),sep=""))
    cat("\n")
    cat("\n")
    cat("The degrees of freedom are:")
    cat("\n")
    cat(paste("df  = ",format(out$parameter,scientific=FALSE),sep=""))
    cat("\n")
    cat("\n")
    cat(paste("The probability of getting this result or more extreme if the distribution\nis really the theoretical one",sep=""))
    cat("\n")
    cat(paste("p = ",format(out$p.value,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("You can get this result by:")
    cat("\n")
    cat("\n")
    
    collapsed = paste(x,collapse=",")
    
    cat("Inputting the sample data in a list:")
    cat("\n")
    cat(paste("data  = c(",collapsed,")",sep=""))
    cat("\n")
    cat("\n")
    collapsedp = paste(p,collapse=",")
    
    cat("and also recording the theoretical data:")
    cat("\n")
    cat(paste("prob  = c(",collapsedp,")",sep=""))
    cat("\n")
    cat("\n")
    cat("Then using that to run the test:")
    cat("\n")
    cat("chisq.test(data,p = prob)")
  }
  
  
  # make a plot
  
  rightend = qchisq(.99,out$parameter)
  
  if(out$statistic>rightend){
    rightend = 1.1*out$statistic
  }
  df = out$parameter
  spacing = rightend*0.001
  x = seq(from=0,to=rightend,by=spacing)
  curve(dchisq(x,df), xlim=c(0,rightend),xlab="",ylab="",yaxt="n")
  
  cord.x <- c(out$statistic,seq(from=out$statistic,to=rightend,by=spacing),rightend) 
  cord.y <- c(0,dchisq(seq(from=out$statistic,to=rightend,by=spacing),df),0) 
  
  polygon(cord.x,cord.y,col='skyblue')
  abline(v = c(out$statistic,-out$statistic),col=c('skyblue','skyblue'),lwd = c(2,2))
  
}