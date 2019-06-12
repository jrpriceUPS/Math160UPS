#' t-test
#'
#' This function asks you a sequence of questions in order to execute a t-test. It finds a confidence interval and a p-value, produces a plot, and indicates how this could be queried directly from R.
#' @export
#' @examples
#' > x = c(1, 2, 3, 4, 5, 6, 7)
#' > t_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. single
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. whole
#' What is the name of your variable? x
#' The statistics for your dataset are: 
#'   xbar =  4
#' s =  2.160247
#' n =  7
#' df =  7 - 1 =  6
#' 
#' What is the theoretical mean you are testing against (called mu_0)? 3
#' What is your desired confidence level? .95
#' Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. two-sided
#' Your t-statistic is:
#'   t  = (4-3)/(2.160247/sqrt(7)) = 1.224745
#' 
#' The probability of getting this result or more extreme for xbar if mu really is 3 is
#' p =  0.2665697
#' 
#' You can get this result by typing:
#'   2*(1-pt(1.22474487139159,6))
#' 
#' 
#' The 95% confidence interval for the population mean is
#' 2.002105  < mu <  5.997895
#' 
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#' 
#' and then calculating:
#'   4 - 2.446912 x 2.160247/sqrt(7)  and  4 + 2.446912 x 2.160247/sqrt(7)
#' 
#' 
#' Or, since you have the whole dataset, you could just type:
#'   t.test(x,mu = 3)
#'   
#'   
#'   
#'   
#'   
#'   
#' > t_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. single
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. stats
#' What is your sample mean? 4
#' What is your sample standard deviation? 2.16
#' What is your sample size? 7
#' What is the theoretical mean you are testing against (called mu_0)? 3
#' What is your desired confidence level? .95
#' Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. greater
#' Your t-statistic is:
#'   t  = (4-3)/(2.16/sqrt(7)) = 1.224885
#' 
#' The probability of getting this result or more extreme for xbar if mu really is 3 is
#' p =  0.1332603
#' 
#' You can get this result by typing:
#'   1-pt(1.22488486623361,6)
#' 
#' 
#' The 95% confidence interval for the population mean is
#' 2.002333  < mu <  5.997667
#' 
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#' 
#' and then calculating:
#'   4 - 2.446912 x 2.16/sqrt(7)  and  4 + 2.446912 x 2.16/sqrt(7)
#'
#'
#'
#'
#'
#'
#' > x = c(1, 2, 3, 4, 5, 6, 7)
#' > y = c(2.5,  5.1,  6.4,  8.4, 10.8, 13.4, 15.3)
#' > t_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. comparing
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. whole
#' Is this a matched-pairs comparison in which the same subjects are measured twice? yes
#' What is the name of the variable for the first set of measurements? x
#' What is the name of the variable for the second set of measurements? y
#' The statistics for your datasets are: 
#'   n =  7
#' xbar1 =  4
#' s1 =  2.160247
#' 
#' xbar2 =  8.842857
#' s2 =  4.595236
#' 
#' The statistics for the difference are: 
#'   xbar =  4.842857
#' s =  2.445988
#' n =  7
#' df =  7 - 1 =  6
#' 
#' What is your desired confidence level? .95
#' Are you checking whether the mean of the second population is less, greater, or different than the mean of the first population? Possible answers are 'less', 'greater', and 'different'. greater
#' Your t-statistic is:
#'   t  = 4.842857/(2.445988/sqrt(7)) = 5.238372
#' 
#' The probability of getting this result or more extreme for xbar2 - xbar1 if mu1 really is bigger than mu2 is
#' p =  0.0009707175
#' 
#' You can get this result by typing:
#'   1-pt(5.23837230565063,6)
#' 
#' 
#' The 95% confidence interval for the difference in population means is
#' 2.580696  < mu2 - mu1 <  7.105019
#' 
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#' 
#' and then calculating:
#'   (8.84285714285714-4) - 2.446912 x 2.445988/sqrt(7)  and  (8.84285714285714-4) + 2.446912 x 2.445988/sqrt(7)
#' 
#' 
#' Or, since you have the whole dataset, you could just type:
#'   t.test(y,x,paired=TRUE,alternative='greater')
#'   
#'   
#'   
#'   
#'   
#'   
#' > t_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. comparing
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. stats
#' Is this a matched-pairs comparison in which the same subjects are measured twice? yes
#' What is the sample mean of the first set of measurements? 4
#' What is the sample mean of the second set of measurements? 8.85
#' What is the sample standard deviation of the first set of measurements? 4
#' What is the sample standard deviation of the second set of measurements? 2.446
#' What is the sample size? 7
#' The statistics for the difference are: 
#'   xbar =  4.85
#' s =  4.688594
#' n =  7
#' df =  7 - 1 =  6
#' 
#' What is your desired confidence level? .95
#' Are you checking whether the mean of the second population is less, greater, or different than the mean of the first population? Possible answers are 'less', 'greater', and 'different'. different
#' Your t-statistic is:
#'   t  = 4.85/(4.688594/sqrt(7)) = 2.736832
#' 
#' The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is
#' p =  0.03387768
#' 
#' You can get this result by typing:
#'   2*(1-pt(2.73683180639237,6))
#' 
#' 
#' The 95% confidence interval for the difference in population means is
#' 0.5137735  < mu2 - mu1 <  9.186226
#' 
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,6) = 2.446912
#' 
#' and then calculating:
#'   (8.85-4) - 2.446912 x 4.688594/sqrt(7)  and  (8.85-4) + 2.446912 x 4.688594/sqrt(7)
#'   
#'   
#'   
#'   
#'   
#'   
#' > t_test()
#' Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. comparing
#' Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. whole
#' Is this a matched-pairs comparison in which the same subjects are measured twice? no
#' What is the name of the variable for the first set of measurements? x
#' What is the name of the variable for the second set of measurements? y
#' The statistics for your datasets are: 
#'   n1 =  7
#' xbar1 =  4
#' s1 =  2.160247
#' 
#' n2 =  7
#' xbar2 =  8.842857
#' s2 =  4.595236
#' 
#' The statistics for the difference are: 
#'   xbar =  4.842857
#' s = sqrt(2.160247^2/7 + 4.595236^2/7) = 1.919183
#' df = 8.5285
#' 
#' What is your desired confidence level? .95
#' Are you checking whether the mean of the second population is less, greater, or different than the mean of the first population? Possible answers are 'less', 'greater', and 'different'. different
#' Your t-statistic is:
#'   t  = (4.84285714285714)/(1.919183) = 2.523395
#' 
#' The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is
#' p =  0.03391985
#' 
#' You can get this result by typing:
#'   2*(1-pt(2.52339452856832,8.52849965837585))
#' 
#' 
#' The 95% confidence interval for the difference in population means is
#' 0.4644978  < mu2 - mu1 <  9.221216
#' 
#' You can get this result by finding:
#'   tstar = 1-qt((1-0.95)/2,8.5285) = 2.281366
#' 
#' and then calculating:
#'   (8.84285714285714-4) - 2.281366 x 1.919183  and  (8.84285714285714-4) + 2.281366 x 1.919183
#' 
#' 
#' Or, since you have the whole dataset, you could just type:
#'   t.test(y,x)

t_test <- function(){
  compare = readline("Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. ")
  vec = readline("Do you have the whole dataset or do you just have the statistics (mean, standard deviation)? Possible answers are 'whole' or 'stats'. ")
  
  if(compare=="comparing"){
    matched = readline("Is this a matched-pairs comparison in which the same subjects are measured twice? ")
    if(matched=="yes"){
      if(vec=="stats"){
        xbar1 = as.numeric(readline("What is the sample mean of the first set of measurements? "))
        xbar2 = as.numeric(readline("What is the sample mean of the second set of measurements? "))
        s1 = as.numeric(readline("What is the sample standard deviation of the first set of measurements? "))
        s2 = as.numeric(readline("What is the sample standard deviation of the second set of measurements? "))
        n = as.numeric(readline("What is the sample size? "))
        xbar = xbar2-xbar1
        s = sqrt(s1^2+s2^2)
        cat("The statistics for the difference are: ")
        cat("\n")
        cat(paste("xbar = ",format(xbar,scientific=FALSE)))
        cat("\n")
        cat(paste("s = ",format(s,scientific=FALSE)))
        cat("\n")
        cat(paste("n = ",format(n,scientific=FALSE)))
        cat("\n")
        cat(paste("df = ",format(n,scientific=FALSE),"- 1 = ",format(n-1,scientific=FALSE)))
        cat("\n")
        cat("\n")
      }
      
      if(vec=="whole"){
        varname1 = readline("What is the name of the variable for the first set of measurements? ")
        varname2 = readline("What is the name of the variable for the second set of measurements? ")
        if(grepl("$", varname1, fixed=TRUE)){
          names = strsplit(varname1,"\\$")
          frame = get(names[[1]])
          data1 = frame[[names[[1]][2]]]
        } else{
          data1 = get(varname1)}
        data1 = data1[!is.na(data1)]
        xbar1 = mean(data1)
        s1 = sd(data1)
        n = length(data1) 
       
        if(grepl("$", varname2, fixed=TRUE)){
          names = strsplit(varname2,"\\$")
          frame = get(names[[1]])
          data2 = frame[[names[[1]][2]]]
        } else{
          data2 = get(varname2)}
        data2 = data2[!is.na(data2)]
        xbar2 = mean(data2)
        s2 = sd(data2)
        
        data = data2-data1
        xbar = mean(data)
        s = sd(data)
        
        cat("The statistics for your datasets are: ")
        cat("\n")
        cat(paste("n = ",format(n,scientific=FALSE)))
        cat("\n")
        cat(paste("xbar1 = ",format(xbar1,scientific=FALSE)))
        cat("\n")
        cat(paste("s1 = ",format(s1,scientific=FALSE)))
        cat("\n")
        cat("\n")
        cat(paste("xbar2 = ",format(xbar2,scientific=FALSE)))
        cat("\n")
        cat(paste("s2 = ",format(s2,scientific=FALSE)))
        cat("\n")
        cat("\n")
        cat("The statistics for the difference are: ")
        cat("\n")
        cat(paste("xbar = ",format(xbar,scientific=FALSE)))
        cat("\n")
        cat(paste("s = ",format(s,scientific=FALSE)))
        cat("\n")
        cat(paste("n = ",format(n,scientific=FALSE)))
        cat("\n")
        cat(paste("df = ",format(n,scientific=FALSE),"- 1 = ",format(n-1,scientific=FALSE)))
        cat("\n")
        cat("\n")
        
      }
      
      conf_level = as.numeric(readline("What is your desired confidence level? "))
      while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1')
        conf_level = as.numeric(readline("What is your desired confidence level? "))
      }
      sidedness = readline("Are you checking whether the mean of the second population is less, greater, or different than the mean of the first population? Possible answers are 'less', 'greater', and 'different'. ")
      if(sidedness=="different"){
        sidedness = "both"
      }
      
      
      t = xbar/(s/sqrt(n))
      df = n-1
      out = conduct_t_test(t,df,sidedness)
      
      # new version with only one-sided confidence intervals:
      tstar = -qt((1-conf_level)/2,df)
      
      thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
      thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
      thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
      
      lower = xbar - tstar*s/sqrt(n)
      upper = xbar + tstar*s/sqrt(n)
      
      
      
      
      if(sidedness == "both"){
        
        # tstar = -qt((1-conf_level)/2,df)
        # 
        # thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
        # thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
        # 
        # lower = xbar - tstar*s/sqrt(n)
        # upper = xbar + tstar*s/sqrt(n)
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is",sep="")
      }
      
      if(sidedness == "less"){
        
        # tstar = qt(conf_level,df)
        # 
        # thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = "-Infinity"
        # thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
        # 
        # lower = -Inf
        # upper = xbar + tstar*s/sqrt(n)
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if mu2 really is bigger than mu1 is",sep="")
      }
      
      if(sidedness == "greater"){
        # tstar = qt(conf_level,df)
        # 
        # thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
        # thing_to_type4 = "Infinity"
        # 
        # lower = xbar - tstar*s/sqrt(n)
        # upper = Inf
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if mu1 really is bigger than mu2 is",sep="")
        
      }
      
      
      
      cat("Your t-statistic is:")
      cat("\n")
      cat(paste("t  = ",format(xbar,scientific=FALSE),"/(",format(s,scientific=FALSE),"/sqrt(",format(n,scientific=FALSE),")) = ",format(t,scientific=FALSE),sep=""))
      cat("\n")
      cat("\n")
      cat(sidedness_type)
      cat("\n")
      cat(paste("p = ",format(out$prob,scientific=FALSE)))
      cat("\n")
      cat("\n")
      cat("You can get this result by typing:")
      cat("\n")
      cat(out$p_value_type)
      cat("\n")
      cat("\n")
      cat("\n")
      cat(paste("The ",toString(conf_level*100),"% confidence interval for the difference in population means is",sep=""))
      cat("\n")
      cat(paste(format(lower,scientific=FALSE)," < mu2 - mu1 < ",format(upper,scientific=FALSE)))
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
      
      if(vec=="whole"){
        cat("\n")
        cat("\n")
        cat("\n")
        cat("Or, since you have the whole dataset, you could just type:")
        cat("\n")
        if(sidedness=="both"){
          cat(paste("t.test(",varname2,",",varname1,",paired=TRUE)",sep=""))
        }
        if(sidedness=="less"){
          cat(paste("t.test(",varname2,",",varname1,",paired=TRUE,alternative='less')",sep=""))
        }
        if(sidedness=="greater"){
          cat(paste("t.test(",varname2,",",varname1,",paired=TRUE,alternative='greater')",sep=""))
        }
        
      }
    }
    
    if(matched=="no"){
      if(vec=="stats"){
        xbar1 = as.numeric(readline("What is the sample mean of the first set of measurements? "))
        xbar2 = as.numeric(readline("What is the sample mean of the second set of measurements? "))
        s1 = as.numeric(readline("What is the sample standard deviation of the first set of measurements? "))
        s2 = as.numeric(readline("What is the sample standard deviation of the second set of measurements? "))
        n1 = as.numeric(readline("What is the sample size of the first set of measurements? "))
        n2 = as.numeric(readline("What is the sample size of the second set of measurements? "))
        xbar = xbar2-xbar1
        df = n1+n2-2
        s = sqrt(s1^2/n1+s2^2/n2)
        cat("The statistics for the difference are: ")
        cat("\n")
        cat(paste("xbar = ",format(xbar,scientific=FALSE)))
        cat("\n")
        cat(paste("s = sqrt(",format(s1,scientific=FALSE),"^2/",format(n1,scientific=FALSE)," + ",format(s2,scientific=FALSE),"^2/",format(n2,scientific=FALSE),") = ",format(s,scientific=FALSE),sep=""))
        cat("\n")
        cat(paste("df = ",format(n1,scientific=FALSE),"+",format(n2,scientific=FALSE),"- 2 = ",format(n1+n2-2,scientific=FALSE)))
        cat("\n")
        cat("\n")
      }
      
      if(vec=="whole"){
        varname1 = readline("What is the name of the variable for the first set of measurements? ")
        varname2 = readline("What is the name of the variable for the second set of measurements? ")
        
        if(grepl("$", varname1, fixed=TRUE)){
          names = strsplit(varname1,"\\$")
          frame = get(names[[1]])
          data1 = frame[[names[[1]][2]]]
        } else{
          data1 = get(varname1)}
        data1 = data1[!is.na(data1)]
        xbar1 = mean(data1)
        s1 = sd(data1)
        n1 = length(data1) 
        
        if(grepl("$", varname2, fixed=TRUE)){
          names = strsplit(varname2,"\\$")
          frame = get(names[[1]])
          data2 = frame[[names[[1]][2]]]
        } else{
          data2 = get(varname2)}
        data2 = data2[!is.na(data2)]
        xbar2 = mean(data2)
        s2 = sd(data2)
        n2 = length(data2)
        
        
        xbar = mean(data2)-mean(data1)
        s = sqrt(sd(data1)^2/n1 + sd(data2)^2/n2)
        test_result = t.test(data1,data2)
        df = as.numeric(test_result$parameter)
        
        cat("The statistics for your datasets are: ")
        cat("\n")
        cat(paste("n1 = ",format(n1,scientific=FALSE)))
        cat("\n")
        cat(paste("xbar1 = ",format(xbar1,scientific=FALSE)))
        cat("\n")
        cat(paste("s1 = ",format(s1,scientific=FALSE)))
        cat("\n")
        cat("\n")
        cat(paste("n2 = ",format(n2,scientific=FALSE)))
        cat("\n")
        cat(paste("xbar2 = ",format(xbar2,scientific=FALSE)))
        cat("\n")
        cat(paste("s2 = ",format(s2,scientific=FALSE)))
        cat("\n")
        cat("\n")
        cat("The statistics for the difference are: ")
        cat("\n")
        cat(paste("xbar = ",format(xbar,scientific=FALSE)))
        cat("\n")
        cat(paste("s = sqrt(",format(s1,scientific=FALSE),"^2/",format(n1,scientific=FALSE)," + ",format(s2,scientific=FALSE),"^2/",format(n2,scientific=FALSE),") = ",format(s,scientific=FALSE),sep=""))
        cat("\n")
        cat(paste("df = ",format(df,scientific=FALSE),sep=""))
        cat("\n")
        cat("\n")
        
      }
      
      conf_level = as.numeric(readline("What is your desired confidence level? "))
      while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1')
        conf_level = as.numeric(readline("What is your desired confidence level? "))
      }
      
      sidedness = readline("Are you checking whether the mean of the second population is less, greater, or different than the mean of the first population? Possible answers are 'less', 'greater', and 'different'. ")
      if(sidedness=="different"){
        sidedness = "both"
      }
      
      
      t = xbar/s
      out = conduct_t_test(t,df,sidedness)
      
      # new version with no one-sided confidence intervals:
      tstar = -qt((1-conf_level)/2,df)
      
      thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
      thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
      thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
      
      lower = xbar - tstar*s
      upper = xbar + tstar*s
      
      
      if(sidedness == "both"){
        
        # tstar = -qt((1-conf_level)/2,df)
        # 
        # thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
        # thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
        # 
        # lower = xbar - tstar*s
        # upper = xbar + tstar*s
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if there really is no difference is",sep="")
      }
      
      if(sidedness == "less"){
        
        # tstar = qt(conf_level,df)
        # 
        # thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = "-Infinity"
        # thing_to_type4 = paste("(",toString(xbar2),"-",toString(xbar1),") + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
        # 
        # lower = -Inf
        # upper = xbar + tstar*s
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if mu2 really is bigger than mu1",sep="")
      }
      
      if(sidedness == "greater"){
        # tstar = qt(conf_level,df)
        # 
        # thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
        # thing_to_type3 = paste("(",toString(xbar2),"-",toString(xbar1),") - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),sep="")
        # thing_to_type4 = "Infinity"
        # 
        # lower = xbar - tstar*s
        # upper = Inf
        
        sidedness_type = paste("The probability of getting this result or more extreme for xbar2 - xbar1 if mu1 really is bigger than mu2",sep="")
        
      }
      
      
      
      cat("Your t-statistic is:")
      cat("\n")
      cat(paste("t  = (",toString(xbar),")/(",format(s,scientific=FALSE),") = ",format(t,scientific=FALSE),sep=""))
      cat("\n")
      cat("\n")
      cat(sidedness_type)
      cat("\n")
      cat(paste("p = ",format(out$prob,scientific=FALSE)))
      cat("\n")
      cat("\n")
      cat("You can get this result by typing:")
      cat("\n")
      cat(out$p_value_type)
      cat("\n")
      cat("\n")
      cat("\n")
      cat(paste("The ",toString(conf_level*100),"% confidence interval for the difference in population means is",sep=""))
      cat("\n")
      cat(paste(format(lower,scientific=FALSE)," < mu2 - mu1 < ",format(upper,scientific=FALSE)))
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
      
      if(vec=="whole"){
        cat("\n")
        cat("\n")
        cat("\n")
        cat("Or, since you have the whole dataset, you could just type:")
        cat("\n")
        if(sidedness=="both"){
          cat(paste("t.test(",varname2,",",varname1,")",sep=""))
        }
        if(sidedness=="less"){
          cat(paste("t.test(",varname2,",",varname1,",alternative='less')",sep=""))
        }
        if(sidedness=="greater"){
          cat(paste("t.test(",varname2,",",varname1,",alternative='greater')",sep=""))
        }
        
      }
    }
  }
  
  if(compare=="single"){
    if(vec=="stats"){
      xbar = as.numeric(readline("What is your sample mean? "))
      s = as.numeric(readline("What is your sample standard deviation? "))
      n = as.numeric(readline("What is your sample size? "))
    }
    
    if(vec=="whole"){
      varname = readline("What is the name of your variable? ")
      if(grepl("$", varname, fixed=TRUE)){
        names = strsplit(varname,"\\$")
        frame = get(names[[1]])
        data = frame[[names[[1]][2]]]
      } else{
        data = get(varname)}
      data = data[!is.na(data)]
      xbar = mean(data)
      s = sd(data)
      n = length(data)
      
      
      cat("The statistics for your dataset are: ")
      cat("\n")
      cat(paste("xbar = ",format(xbar,scientific=FALSE)))
      cat("\n")
      cat(paste("s = ",format(s,scientific=FALSE)))
      cat("\n")
      cat(paste("n = ",format(n,scientific=FALSE)))
      cat("\n")
      cat(paste("df = ",format(n,scientific=FALSE),"- 1 = ",format(n-1,scientific=FALSE)))
      cat("\n")
      cat("\n")
    }
    
    mu_0 = as.numeric(readline("What is the theoretical mean you are testing against (called mu_0)? "))
    conf_level = as.numeric(readline("What is your desired confidence level? "))
    while(conf_level<0 | conf_level>1){cat('Please choose a confidence level between 0 and 1')
      conf_level = as.numeric(readline("What is your desired confidence level? "))
    }
    sidedness = readline("Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. ")
    if(sidedness=="two-sided"){
      sidedness = "both"
    }
    
    
    t = (xbar - mu_0)/(s/sqrt(n))
    df = n-1
    out = conduct_t_test(t,df,sidedness)
    
    # new version with no one-sided confidence intervals
    tstar = -qt((1-conf_level)/2,df)
    
    thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
    thing_to_type3 = paste(toString(xbar)," - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    thing_to_type4 = paste(toString(xbar)," + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    
    lower = xbar - tstar*s/sqrt(n)
    upper = xbar + tstar*s/sqrt(n)
    
    
    # if(sidedness == "both"){
    #   
    #   tstar = -qt((1-conf_level)/2,df)
    #   
    #   thing_to_type2 = paste("tstar = 1-qt((1-",format(conf_level,scientific=FALSE),")/2,",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
    #   thing_to_type3 = paste(toString(xbar)," - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    #   thing_to_type4 = paste(toString(xbar)," + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    #   
    #   lower = xbar - tstar*s/sqrt(n)
    #   upper = xbar + tstar*s/sqrt(n)
    # }
    # 
    # if(sidedness == "less"){
    #   
    #   tstar = qt(conf_level,df)
    #   
    #   thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
    #   thing_to_type3 = "-Infinity"
    #   thing_to_type4 = paste(toString(xbar)," + ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    #   
    #   lower = -Inf
    #   upper = xbar + tstar*s/sqrt(n)
    # }
    # 
    # if(sidedness == "greater"){
    #   tstar = qt(conf_level,df)
    #   
    #   thing_to_type2 = paste("tstar = qt(",format(conf_level,scientific=FALSE),",",format(df,scientific=FALSE),") = ",format(tstar,scientific=FALSE),sep="")
    #   thing_to_type3 = paste(toString(xbar)," - ",format(tstar,scientific=FALSE)," x ",format(s,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
    #   thing_to_type4 = "Infinity"
    #   
    #   lower = xbar - tstar*s/sqrt(n)
    #   upper = Inf
    #   
    # }
    
    
    
    cat("Your t-statistic is:")
    cat("\n")
    cat(paste("t  = (",format(xbar,scientific=FALSE),"-",format(mu_0,scientific=FALSE),")/(",format(s,scientific=FALSE),"/sqrt(",format(n,scientific=FALSE),")) = ",format(t,scientific=FALSE),sep=""))
    cat("\n")
    cat("\n")
    cat(paste("The probability of getting this result or more extreme for xbar if mu really is ",toString(mu_0)," is",sep=""))
    cat("\n")
    cat(paste("p = ",format(out$prob,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(out$p_value_type)
    cat("\n")
    cat("\n")
    cat("\n")
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
    
    if(vec=="whole"){
      cat("\n")
      cat("\n")
      cat("\n")
      cat("Or, since you have the whole dataset, you could just type:")
      cat("\n")
      if(sidedness=="both"){
        cat(paste("t.test(",varname,",mu = ",format(mu_0,scientific=FALSE),")",sep=""))
      }
      if(sidedness=="less"){
        cat(paste("t.test(",varname,",mu=",format(mu_0,scientific=FALSE),",alternative='less'",")",sep=""))
      }
      if(sidedness=="greater"){
        cat(paste("t.test(",varname,",mu=",format(mu_0,scientific=FALSE),",alternative='greater'",")",sep=""))
      }
      
    }
  }
  
  
  
  
}