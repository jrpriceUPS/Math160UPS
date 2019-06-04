linreg <- function(){
  
  varname1 = readline("What is the name of the list with your x variable? ")
  varname2 = readline("What is the name of the list with your y variable? ")
  
  x = get(varname1)
  y = get(varname2)
  
  
  plot(x,y,xlab=varname1,ylab=varname2)
  regress = lm(y ~ x)
  
  intercept = as.numeric(coefficients(regress)[1])
  slope = as.numeric(coefficients(regress)[2])
  
  cat(paste("The best fit line for these data is:"))
  cat("\n")
  cat(paste(varname2," = ",toString(slope)," x (",varname1,") + ",toString(intercept),sep=""))
  
  # we can add this line to our plot with the function abline:
  abline(regress)
  
  cat("\n")
  cat("\n")
  cat("\n")
  cat(paste("You can plot these data by typing:"))
  cat("\n")
  cat(paste("plot(",varname1,",",varname2,")",sep=""))
  cat("\n")
  cat("\n")
  cat(paste("You can find the best fit line by typing:"))
  cat("\n")
  cat(paste("lm(",varname2,"~",varname1,")",sep=""))
  cat("\n")
  cat("\n")
  cat(paste("You can add the best fit line to the plot by saving the model:"))
  cat("\n")
  cat(paste("model = lm(",varname2,"~",varname1,")",sep=""))
  cat("\n")
  cat("\n")
  cat(paste("and then adding it to the plot:"))
  cat("\n")
  cat("abline(model)")
  
  
}