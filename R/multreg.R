#' Multiple Linear Regression
#'
#' This function allows you to find a multiple linear regression model with several variables, or a categorical predictor with multiple levels.
#' @export
#' @examples
#' > x = c(1,2,3,4,5,6,7)
#' > y = c(2.5,5.1,6.4,8.4,10.8,13.4,15.3)
#' > linreg()
#' What is the name of the list with your x variable?
#'   x
#' What is the name of the list with your y variable?
#'   y
#' The best fit line for these data is:
#'   y = 2.12142857142857 x (x) + 0.357142857142856
#'
#'
#' You can plot these data by typing:
#'   plot(x,y)
#'
#' You can find the best fit line by typing:
#'   lm(y~x)
#'
#' You can add the best fit line to the plot by saving the model:
#'   model = lm(y~x)
#'
#' and then adding it to the plot:
#'   abline(model)

multreg <- function(){
  cat("What is the name of your dataframe? \n")
  frameName = readline()
  myFrame = get(frameName)
  cat("What is the name of the response variable? \n")
  response = readline()
  cat("How many predictor variables will you use? \n")
  numVars = readline()
  
  predVars = rep(0,numVars)
  for(i in 1:numVars){
    cat(paste("Predictor variable number",i,": \n"))
    predVars[i] = readline()
  }
  
  myRedFrame = subset(myFrame, select = c(response,predVars))
  myRedFrame[myRedFrame==""] = NA
  
  
  model = lm(reformulate(predVars,response), data = na.omit(myRedFrame))
  
  print(summary(model))
  
  cat(paste("You can see the regression summary by typing:"))
  cat("\n")
  cat(paste(paste("summary(lm(",response,"~ "),sep=""),paste(predVars,collapse = " + "),", data = ", frameName, "))",sep = "")
}
