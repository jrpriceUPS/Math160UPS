#' Update Code to Latest Version
#'
#' This function updates your library to the newest version and sets those to become available.
#' @export
#' @examples
#' > update_code()

update_code <- function(){
  remove.packages("Math160UPS")
  library(devtools)
  install_github("kalamadude/Math160UPS")
  library("Math160UPS")
}