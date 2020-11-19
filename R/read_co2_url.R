#' Read CO2
#'
#' This function loads CO2 data from a website (Penny Rowe - polar activity).
#' @export
read_co2_url <- function(co2_website) {
  modern_co2 <- readr::read_delim(co2_website,
                                  delim = " ",
                                  locale = readr::locale(decimal_mark = "."),
                                  na = c("-99.99", "-1"),
                                  col_types = "iidddidd",
                                  col_names = c("year", "month", "decimal_date",
                                                "co2", "deseasonalized",
                                                "no_days", "std_dev_of_days",
                                                "unc_of_monthly_mean"),
                                  comment = "#", trim_ws = TRUE)
}
