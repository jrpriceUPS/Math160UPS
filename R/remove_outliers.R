#' Remove Outliers
#'
#' This function allows you to remove the outliers found in a list you are working with and save it to a new list.
#' @export
#' @examples
#' > x = c(-5,1,3,4,5,5,5,7,8,8,9,10,10,15,24,27)
#' > new_x = remove_outliers()
#' Remember! remove_outliers() needs to know what you want the new list name to be!
#' Make sure you typed *your new list name here* = remove_outliers(). If not, press Esc to cancel.
#'   
#' What is the name of the list with your data? x
#' 
#' Look at the boxplot that was just created.
#' What is the lower cutoff for small outliers? You may answer 'none'. 0
#' What is the upper cutoff for large outliers? You may answer 'none'. 20
#' 
#' 
#' You can do this yourself by typing:
#' *new list name* = x[x > 0 & x < 20]
#' > new_x
#' [1]  1  3  4  5  5  5  7  8  8  9 10 10 15


remove_outliers <- function(){
  
  cat("Remember! remove_outliers() needs to know what you want the new list name to be!")
  cat("\n")
  cat("Make sure you typed *your new list name here* = remove_outliers(). If not, press Esc to cancel.")
  cat("\n")
  cat("\n")
  
  varname = readline("What is the name of the list with your data? ")
  original_data = get(varname)
  boxplot(original_data,horizontal = TRUE,names = "Original Dataset")
  
  cat("\n")
  cat("Look at the boxplot that was just created.")
  cat("\n")
  
  lowerend = readline("What is the lower cutoff for small outliers? You may answer 'none'. ")
  if(lowerend=="none"){lowerend=-Inf}
  
  upperend = readline("What is the upper cutoff for large outliers? You may answer 'none'. ")
  if(upperend=="none"){upperend=Inf}
  
  upperend = as.numeric(upperend)
  lowerend = as.numeric(lowerend)
  
  cat("\n")
  cat("\n")
  cat(paste("You can do this yourself by typing:\n *new list name* = ",varname,"[",varname," > ",format(lowerend,scientific=FALSE)," & ",varname," < ",format(upperend,scientific=FALSE),"]",sep=""))
  
  
  
  new_data = original_data[original_data > lowerend & original_data < upperend]
  boxplot(original_data,new_data,horizontal = TRUE,names = c("Original Dataset","New Dataset"))
  
  new_data
  
}