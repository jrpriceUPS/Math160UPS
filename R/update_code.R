#' Update Code to Latest Version
#'
#' This function updates your library to the newest version and sets those to become available.
#' @export
#' @examples
#' > update_code()

update_code <- function(){
  library(devtools)
  detach("package:Math160UPS", unload = TRUE)
  install_github("jrpriceUPS/Math160UPS")
  .rs.restartR()
  library("Math160UPS")
  cat("\n\n\n\n\n")
  cat("*******************************\n")
  cat("* Your update was successful! *\n")
  cat("*******************************")
}