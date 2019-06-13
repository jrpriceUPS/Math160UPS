#' Update Code to Latest Version
#'
#' This function updates your library to the newest version and sets those to become available.
#' @export
#' @examples
#' > update_code()

update_code <- function(){
  library(devtools)
  install_github("kalamadude/Math160UPS")
  .rs.restartR()
  library("Math160UPS")
}