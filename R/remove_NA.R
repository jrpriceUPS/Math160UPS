#' Remove NAs
#'
#' This function allows you to remove the NAs found in a list you are working with and save it to a new list.
#' @keywords processing
#' @export
#' @examples
#' > x = c(1,2,3,NA,4,5,NA)
#' > new_x = remove_NA()
#' Remember! remove_NA() needs to know what you want the new list name to be!
#' Make sure you typed *your new list name here* = remove_NA(). If not, press Esc to cancel.
#' 
#' What is the name of the list with your data? x
#' Removed 2 NAs!xx
#'  
#' You can do this yourself by typing:
#' *new list name* = x[!is.na(x)]
#' > new_x
#' [1] 1 2 3 4 5

remove_NA <- function(){
  
  cat("Remember! remove_NA() needs to know what you want the new list name to be!")
  cat("\n")
  cat("Make sure you typed *your new list name here* = remove_NA(). If not, press Esc to cancel.")
  cat("\n")
  cat("\n")
  
  varname = readline("What is the name of the list with your data? ")
  if(grepl("$", varname, fixed=TRUE)){
    names = strsplit(varname,"\\$")
    frame = get(names[[1]])
    original_data = frame[[names[[1]][2]]]
  } else{
  original_data = get(varname)}
  
  num_removed = sum(is.na(original_data))
  cat(paste("Removed ",format(num_removed,scientific=FALSE)," NAs!",sep=""))
  
  cat("\n")
  cat("\n")
  cat(paste("You can do this yourself by typing:\n *new list name* = ",varname,"[!is.na(",varname,")]",sep=""))
  
  new_data = original_data[!is.na(original_data)]
}