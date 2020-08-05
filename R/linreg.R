#' Simple Linear Regression
#'
#' This function allows you to plot two variables against one another, execute simple linear regression between two saved datasets, and view the least-squares fit model with the line added to the plot.
#' @export
#' @examples
#' > x = c(1,2,3,4,5,6,7)
#' > y = c(2.5,5.1,6.4,8.4,10.8,13.4,15.3)
#' > linreg()
#' What is the name of the list with your x variable? x
#' What is the name of the list with your y variable? y
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

linreg <- function(){
  cat("What is the name of the list with your x variable? \n")
  varname1 = readline()
  cat("What is the name of the list with your y variable? \n")
  varname2 = readline()
  
  if(grepl("$", varname1, fixed=TRUE)){
    names = strsplit(varname1,"\\$")
    frame = get(names[[1]])
    x = frame[[names[[1]][2]]]
  } else{
    x = get(varname1)}
  
  if(grepl("$", varname2, fixed=TRUE)){
    names = strsplit(varname2,"\\$")
    frame = get(names[[1]])
    y = frame[[names[[1]][2]]]
  } else{
    y = get(varname2)}
  
  
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