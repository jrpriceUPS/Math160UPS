#' @export
try_read_url <- function(website, reader, backup) {
  out <- tryCatch(
    {
      # Try part
      out <- reader(website)
    },
    error=function(cond) {
      #message(paste("URL does not seem to exist,", website))
      #message("Here's the original error message:")
      #message(cond); message()
      message("File from URL ", website, " not found. Using backup dataframe from library.")
      return(backup)
    },
    warning=function(cond) {
      #message(paste("URL caused a warning:", website))
      #message("Here's the original warning message:")
      #message(cond); message()
      message("File from URL ", website, " not found. Using backup dataframe from library.")
      return(backup)
    },
    finally={
      # Execute at the end, regardless of success or error.
      #message("success")
    }
  )
  return(out)
}
