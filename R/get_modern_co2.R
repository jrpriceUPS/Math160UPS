#' @export
get_modern_co2 <- function() {
  # .. Get the modern CO2 data from the NOAA website
  website_co2 <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"
  modern_co2 <- try_read_url(website_co2, read_co2_url, modern_co2_backup)
}
