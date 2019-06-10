#' Find Residuals
#'
#' This function allows you to find and plot the residuals from a linear fit of two datasets.
#' @export
#' @examples
#' > x = c(1,2,3,4,5,6,7)
#' > y = c(2.5,5.1,6.4,8.4,10.8,13.4,15.3)
#' > find_residuals()
#' What is the name of the list with your x variable? x
#' What is the name of the list with your y variable? y
#' The best fit line for these data is:
#'   y = 2.12142857142857 x (x) + 0.357142857142856
#' 
#' 
#' You can find the residuals by saving the model:
#'   model = lm(y~x)
#' 
#' and then typing:
#'   resid(model))
#' 
#' To plot them, type:
#'   plot(x,resid(model))

find_residuals <- function(){
  
  varname1 = readline("What is the name of the list with your x variable? ")
  varname2 = readline("What is the name of the list with your y variable? ")
  
  x = get(varname1)
  y = get(varname2)
  regress = lm(y ~ x)
  
  intercept = as.numeric(coefficients(regress)[1])
  slope = as.numeric(coefficients(regress)[2])
  
  cat(paste("The best fit line for these data is:"))
  cat("\n")
  cat(paste(varname2," = ",toString(slope)," x (",varname1,") + ",toString(intercept),sep=""))
  
  cat("\n")
  cat("\n")
  cat("\n")
  cat(paste("You can find the residuals by saving the model:"))
  cat("\n")
  cat(paste("model = lm(",varname2,"~",varname1,")",sep=""))
  cat("\n")
  cat("\n")
  
  resid(regress)
  plot(x,resid(regress),xlab=varname1,ylab="Residual")
    
  cat(paste("and then typing:"))
  cat("\n")
  cat(paste("resid(model))"))
  cat("\n")
  cat("\n")
  cat(paste("To plot them, type:\n"))
  cat(paste("plot(",varname1,",resid(model))",sep=""))
  
  
}