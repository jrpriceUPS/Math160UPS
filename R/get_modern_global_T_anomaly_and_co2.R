#' @export
get_modern_global_T_anomaly_and_co2 <- function(modern_co2m, law_dome_co2) {
  website_modern_T <- "https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt"
  modern_T <- try_read_url(website_modern_T, read_modern_T, modern_T_backup)

  year = modern_T$year
  T_anomaly <- modern_T$T_anomaly

  year1 = year[1]
  year2 = modern_co2$year[1]
  year3 = year[length(year)]

  co2_pre <- law_dome_co2$co2[law_dome_co2$year>=year1 & law_dome_co2$year<year2]


  co2_post <- approx(modern_co2$decimal_date, modern_co2$co2,
                     year[year>=year2 & year<=year],
                     method = "linear", na.rm = TRUE,
                     rule = 1)$y

  co2 <- c(co2_pre, co2_post)

  # Create a data frame
  modern_global <- data.frame(year, co2, T_anomaly)
}
