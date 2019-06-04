stat.test <- function(){
  
  x_or_p = readline("Are you considering a population mean (or means), a population proportion (or proportions), or a table? Possible answers are 'xbar', 'phat', or 'table'. ")
  
  if(x_or_p=="xbar"){
    
    z = readline("Do you know the *population* standard deviation? (this is rare) ")
    
    
    if(z == "yes"){
      z_test()
    }
    
    if(z == "no"){
      t_test()
    }
  }
  
  if(x_or_p=="phat"){
    
    prop_test()
  }
  
  if(x_or_p=="table"){
    chi_test()
  }
}

chi_test <- function(){
  compare = readline("Are you comparing two distributions or checking goodness of fit? Possible answers are 'comparing' and 'goodness'. ")
  
  if(compare=="comparing"){
    m = as.numeric(readline("How many rows are there in your table? "))
    n = as.numeric(readline("How many columns are there in your table? "))
    
    nrow = 1:m
    ncol = 1:n
    x = rep(0,m*n)
    count = 0
    for(j in ncol){
      for(i in nrow){
        count = count + 1
        x[count] = as.numeric(readline(paste("What is the entry in row ",format(i,scientific=FALSE)," and column ",format(j,scientific=FALSE),"? ",sep="")))
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
    cat(paste("The probability of getting this result or more extreme if there really is no relationship is",sep=""))
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
    m = as.numeric(readline("How many categories are there in your distribution? "))
    
    nrow = 1:m
    x = rep(0,m)
    count = 0
    for(i in nrow){
      x[i] = as.numeric(readline(paste("What is entry number ",format(i,scientific=FALSE)," in your sample? ",sep="")))
    }
    
    same = readline("Is your hypothesis that all categories are equally likely? ")
    if(same=="yes"){p = rep(1/m,m)}
    if(same=="no"){
      p = rep(0,m)
      count = 0
      for(i in nrow){
        p[i] = as.numeric(readline(paste("What is the theoretical probability of entry ",format(i,scientific=FALSE),"? ",sep="")))
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
    cat(paste("The probability of getting this result or more extreme if the distribution is really the theoretical one",sep=""))
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
  
  if(out$statistic <= 20){
  rightend = 1.5*out$statistic
  }
  if(out$statistic>20){
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




z_test <- function(){
  sigma = as.numeric(readline("What is the *population* standard deviation? "))
  xbar = as.numeric(readline("What is your sample mean? "))
  mu_0 = as.numeric(readline("What is the theoretical mean you are testing against (called mu_0)? "))
  n = as.numeric(readline("What is your sample size? "))
  conf_level = as.numeric(readline("What is your desired confidence level? "))
  sidedness = readline("Are you doing a one-sided or two-sided test? Possible answers are 'less', 'greater', and 'two-sided'. ")
  
  s = sigma/sqrt(n)
  
  # new always the same confidence interval
  zstar = -qnorm((1-conf_level)/2,0,1)
  
  thing_to_type2 = paste("zstar = 1-qnorm((1-",format(conf_level,scientific=FALSE),")/2,0,1) = ",format(zstar,scientific=FALSE),sep="")
  thing_to_type3 = paste(toString(xbar)," - ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
  thing_to_type4 = paste(toString(xbar)," + ",format(zstar,scientific=FALSE)," x ",format(sigma,scientific=FALSE),"/sqrt(",toString(n),")",sep="")
  
  lower = xbar - zstar*s
  upper = xbar + zstar*s
  
  
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
}

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
        data1 = get(varname1)
        xbar1 = mean(data1)
        s1 = sd(data1)
        n = length(data1) 
        data2 = get(varname2)
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
        data1 = get(varname1)
        xbar1 = mean(data1)
        s1 = sd(data1)
        n1 = length(data1) 
        data2 = get(varname2)
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
      data = get(varname)
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

prop_test <- function(){
  compare = readline("Do you have a single population or are you comparing populations? Possible answers are 'single' and 'comparing'. ")
  
  if(compare=="comparing"){
    n1 = as.numeric(readline("How many trials were there in your first sample? "))
    X1 = as.numeric(readline("How many successes were there in your first sample? "))
    phat1 = X1/n1
    n2 = as.numeric(readline("How many trials were there in your second sample? "))
    X2 = as.numeric(readline("How many successes were there in your second sample? "))
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

conduct_t_test <- function(t, df, type){
  if(abs(t)<2.75){
    leftend = -3
    rightend = 3
    spacing = 0.1
    xgrid = seq(leftend,rightend,by=spacing)
  }
  if(abs(t)>2.75){
    
    if(t<=0){
      leftend = t-1
      if(type=="less" | type=="greater"){
        rightend = 3
      }
      if(type=="both"){
        rightend = 1-t
      }
      spacing = 0.01*(rightend-leftend)
      xgrid = seq(leftend,rightend,by=spacing)
    }
    
    if(t>0){
      if(type=="both"){
        leftend = -1-t
      }
      if(type=="less" | type=="greater"){
        leftend = -3
      }
      rightend = t+1
      spacing = 0.01*(rightend-leftend)
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  
  if(type == "both"){
    if(t<=0){
      thing_to_type= paste("2*pt(",toString(t),",",toString(df),")",sep="")
    }
    if(t>0){
      thing_to_type= paste("2*(1-pt(",toString(t),",",toString(df),"))",sep="")
    }
    t = abs(t)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=-t,by=spacing),-t,-t) 
    cord.y <- c(0,dt(seq(from=leftend,to=-t,by=spacing),df),dt(-t,df),0) 
    
    cord.x2 <- c(t,seq(from=t,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dt(seq(from=t,to=rightend,by=spacing),df),0) 
    
    result = pt(-abs(t),df)*2
    
    
  }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=t,by=spacing),t,t) 
    cord.y <- c(0,dt(seq(from=leftend,to=t,by=spacing),df),dt(t,df),0) 
    result = pt(t,df)
    thing_to_type= paste("pt(",toString(t),",",toString(df),")",sep="")
  }
  if(type == "greater"){
    cord.x <- c(t,seq(from=t,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dt(seq(from=t,to=rightend,by=spacing),df),0) 
    result = 1 - pt(t,df)
    thing_to_type= paste("1-pt(",toString(t),",",toString(df),")",sep="")
  }
  
  
  
  
  
  
  
  # Make a curve
  
  curve(dt(x,df), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(t,-t),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "greater"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = t,col='skyblue',lwd = 2)
  }
  
  list(prob=result,p_value_type=thing_to_type)
  
}

normal_p <- function(x, mu, sigma, type, print = TRUE){
  
  z = (x-mu)/sigma
  
  if(abs(z)<2.75 ){
    leftend = mu - 3*sigma
    rightend = mu+3*sigma
    spacing = 0.1*sigma
    xgrid = seq(leftend,leftend,by=spacing)
  }
  
  if(abs(z)>2.75 ){
    
    if(z<=0){
      leftend = x-sigma
      if(type=="less" | type=="greater"){
        rightend = mu+3*sigma
      }
      if(type=="both"){
        rightend = mu+(abs(z)+1)*sigma
      }
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
    
    if(z>0){
      if(type=="both"){
        leftend = mu-sigma*(abs(z)+1)
      }
      if(type=="less" | type=="greater"){
        leftend = mu - 3*sigma
      }
      rightend = x+sigma
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  if(type == "both"){
    if(z<=0){
      thing_to_type= paste("2*pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
    }
    if(z>0){
      thing_to_type= paste("2*(1-pnorm(",toString(x),",",toString(mu),",",toString(sigma),"))",sep="")
    }
    z = abs(z)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu-abs(z)*sigma,mu-abs(z)*sigma) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu,sigma),dnorm(mu-abs(z)*sigma,mu,sigma),0) 
    
    cord.x2 <- c(mu+abs(z)*sigma,seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dnorm(seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),mu,sigma),0) 
    
    result = pnorm(-abs(z),0,1)*2
    
    
  }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=x,by=spacing),x,x) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=x,by=spacing),mu,sigma),dnorm(x,mu,sigma),0) 
    result = pnorm(x,mu,sigma)
    thing_to_type= paste("pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  }
  if(type == "greater"){
    cord.x <- c(x,seq(from=x,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dnorm(seq(from=x,to=rightend,by=spacing),mu,sigma),0) 
    result = 1 - pnorm(x,mu,sigma)
    thing_to_type= paste("1-pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  }
  
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(z*sigma+mu,-z*sigma+mu),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "greater"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = x,col='skyblue',lwd = 2)
  }
  
  
  if(print){
    cat(paste("The proportion of observations with a value of",toString(x),"or more extreme is",format(result,scientific=FALSE)))
    cat("\n")
    cat("\n")
    cat("You can get this result by typing:")
    cat("\n")
    cat(thing_to_type)
  }
  
  list(prob=result,p_value_type=thing_to_type)
}
normal_q <- function(q, mu, sigma, type){
  
  
  if(type=="both"){x = qnorm(q/2,mu,sigma)
  thing_to_type = paste("qnorm(",toString(q),"/2,",toString(mu),",",toString(sigma),")",sep="")}
  
  if(type=="less"){x = qnorm(q,mu,sigma)
  thing_to_type = paste("qnorm(",toString(q),",",toString(mu),",",toString(sigma),")",sep="")}
  
  if(type=="more"){x = qnorm(1-q,mu,sigma)
  thing_to_type = paste("qnorm(1-",toString(q),",",toString(mu),",",toString(sigma),")",sep="")}
  
  
  
  
  
  
  
  
  z = (x-mu)/sigma
  
  if(abs(z)<2.75 ){
    leftend = mu - 3*sigma
    rightend = mu+3*sigma
    spacing = 0.1*sigma
    xgrid = seq(leftend,leftend,by=spacing)
  }
  
  if(abs(z)>2.75 ){
    
    if(z<=0){
      leftend = x-sigma
      if(type=="one"){
        rightend = mu+3*sigma
      }
      if(type=="both"){
        rightend = mu+(abs(z)+1)*sigma
      }
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
    
    if(z>0){
      if(type=="both"){
        leftend = mu-sigma*(abs(z)+1)
      }
      if(type=="one"){
        leftend = mu - 3*sigma
      }
      rightend = x+sigma
      spacing = 0.1*sigma
      xgrid = seq(leftend,rightend,by=spacing)
    }
  }
  
  
  if(type == "both"){
    x = abs(x)
    
    
    cord.x <- c(leftend,seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu-abs(z)*sigma,mu-abs(z)*sigma) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=mu-abs(z)*sigma,by=spacing),mu,sigma),dnorm(mu-abs(z)*sigma,mu,sigma),0) 
    
    cord.x2 <- c(mu+abs(z)*sigma,seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),rightend) 
    cord.y2 <- c(0,dnorm(seq(from=mu+abs(z)*sigma,to=rightend,by=spacing),mu,sigma),0) 
    
    result = pnorm(-abs(x),mu,sigma)*2
    
    
  }
  
  if(type == "less"){
    cord.x <- c(leftend,seq(from=leftend,to=x,by=spacing),x,x) 
    cord.y <- c(0,dnorm(seq(from=leftend,to=x,by=spacing),mu,sigma),dnorm(x,mu,sigma),0)
  }
  
  if(type == "more"){
    cord.x <- c(x,seq(from=x,to=rightend,by=spacing),rightend) 
    cord.y <- c(0,dnorm(seq(from=x,to=rightend,by=spacing),mu,sigma),0)
  }
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  if(type == "both"){
    polygon(cord.x,cord.y,col='skyblue')
    polygon(cord.x2,cord.y2,col='skyblue')
    abline(v = c(z*sigma+mu,-z*sigma+mu),col=c('skyblue','skyblue'),lwd = c(2,2))
  }
  if(type == "less" | type == "more"){
    polygon(cord.x,cord.y,col='skyblue')
    abline(v = x,col='skyblue',lwd = 2)
  }
  
  
  
  
  cat(paste("The proportion of observations with a value of",toString(x),"or more extreme is",format(q,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
  
}
normal_p2 <- function(x, y, mu, sigma){
  
  if(y<x){
    x0 = x
    y0 = y
    x = y0
    y = x0
  }
  
  lowz = (x-mu)/sigma
  highz = (y-mu)/sigma
  
  if(lowz>=-2.75 ){
    leftend = mu - 3*sigma
  }
  if(lowz < -2.75){
    leftend = x-sigma
  }
  
  if(highz<=2.75){
    rightend = mu + 3*sigma
  }
  if(highz>2.75){
    rightend = y+sigma
  }
  
  spacing = 0.1*sigma
  xgrid = seq(leftend,rightend,by=spacing)
  
  thing_to_type= paste("pnorm(",toString(y),",",toString(mu),",",toString(sigma),")-pnorm(",toString(x),",",toString(mu),",",toString(sigma),")",sep="")
  result = pnorm(y,mu,sigma)-pnorm(x,mu,sigma)
  
  
  
  cord.x <- c(x,seq(from=x,to=y,by=spacing),y,y) 
  cord.y <- c(0,dnorm(seq(from=x,to=y,by=spacing),mu,sigma),dnorm(y,mu,sigma),0) 
  
  
  
  
  
  
  
  
  # Make a curve
  
  curve(dnorm(x,mu,sigma), xlim=c(leftend,rightend),xlab="",ylab="",yaxt="n")
  
  # Add the shaded area.
  
  polygon(cord.x,cord.y,col='skyblue')
  abline(v = c(x,y),col='skyblue',lwd = 2)
  
  
  
  
  cat(paste("The proportion of observations between",toString(x),"and",toString(y),"is",format(result,scientific=FALSE)))
  cat("\n")
  cat("\n")
  cat("You can get this result by typing:")
  cat("\n")
  cat(thing_to_type)
}
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