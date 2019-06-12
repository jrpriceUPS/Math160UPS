#' Remove NAs from Two Data Sets
#'
#' This function allows you to remove the NAs found in two lists you are working with so you can calculate the correlation, for example.
#' @keywords processing
#' @export
#' @examples
#' > x = c(1, 2, 3, NA, 4, 5, NA)
#' > y = c(NA, 1, 1, NA, NA, 3, 5)
#' > newvars = remove_NA2()
#' Remember! remove_NA2() needs to know what you want the new lists to be stored in!
#' Make sure you typed *your new double list name here* = remove_NA2(). If not, press Esc to cancel.
#' 
#' What is the name of the first list with your data? x
#' What is the name of the second list with your data? y
#' Removed 4 NAs!
#' 
#' Both lists are stored in the variable you indicated at the beginning. You can save the two new lists by typing:
#' *new list 1 name* = *name of output*[[1]]
#' *new list 2 name* = *name of output*[[2]]
#'   
#'   
#' You can also do this whole process yourself by typing:
#' *new list 1 name* = x[!is.na(x) & !is.na(y)]
#' and
#' *new list 2 name* = y[!is.na(y) & !is.na(y)]
#' > new_x = newvars[[1]]
#' > new_y = newvars[[2]]
#' > new_x
#' [1] 2 3 5
#' > new_y
#' [1] 1 1 3

remove_NA2 <- function(){
  
  cat("Remember! remove_NA2() needs to know what you want the new lists to be stored in!")
  cat("\n")
  cat("Make sure you typed *your new double list name here* = remove_NA2(). If not, press Esc to cancel.")
  cat("\n")
  cat("\n")
  
  varname1 = readline("What is the name of the first list with your data? ")
  
  varname2 = readline("What is the name of the second list with your data? ")
  
  if(grepl("$", varname1, fixed=TRUE)){
    names = strsplit(varname1,"\\$")
    frame = get(names[[1]])
    original_data1 = frame[[names[[1]][2]]]
  } else{
    original_data1 = get(varname1)}
  
  if(grepl("$", varname2, fixed=TRUE)){
    names = strsplit(varname2,"\\$")
    frame = get(names[[1]])
    original_data2 = frame[[names[[1]][2]]]
  } else{
    original_data2 = get(varname2)}
  
  num_removed = sum(is.na(original_data1+original_data2))
  cat(paste("Removed ",format(num_removed,scientific=FALSE)," NAs!\n\n",sep=""))
  
  cat("Both lists are stored in the variable you indicated at the beginning. You can save the two new lists by typing:\n")
  cat(paste("*new list 1 name* = *name of output*[[1]]\n"))
  cat(paste("*new list 2 name* = *name of output*[[2]]\n\n"))
  
  cat("\n")
  cat(paste("You can also do this whole process yourself by typing:\n")) 
  cat(paste("*new list 1 name* = ",varname1,"[!is.na(",varname1,") & !is.na(",varname2,")]\n",sep=""))
  cat("and\n")
  cat(paste("*new list 2 name* = ",varname2,"[!is.na(",varname2,") & !is.na(",varname2,")]",sep=""))
  
  new_data1 = original_data1[!is.na(original_data1) & !is.na(original_data2)]
  new_data2 = original_data2[!is.na(original_data1) & !is.na(original_data2)]
  
  output = list(new_data1,new_data2)
}