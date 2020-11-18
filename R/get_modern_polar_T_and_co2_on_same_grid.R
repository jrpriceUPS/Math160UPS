#' @export
get_modern_polar_T_and_co2_on_same_grid <- function(modern_polar_T, modern_co2) {
  # # #    TEMPERATURE VS CO2 in modern times  # # #
  # .. It looks like a reasonable year range is 1958 to 2019
  #    create a year vector and then pick out the CO2 concentration
  #    and average Utqiagvik temperature for that year


  # .. To get temperature and CO2 on the same year grid,
  #    they need to  share an x-value. Let's use year
  year1 <- modern_polar_T$DATE
  year2 <- modern_co2$year[modern_co2$month == 6]

  y1 <- max(min(year1), min(year2))
  y2 <- min(max(year1), max(year2))
  year <- c(y1:y2)
  i1 <- which(modern_polar_T$DATE == y1)
  i2 <- which(modern_polar_T$DATE == y2)
  T <- modern_polar_T$TAVG[i1:i2]

  modern_co2$year[modern_co2$month == 6]
  i1 <- which(modern_co2$year[modern_co2$month == 6] == y1)
  i2 <- which(modern_co2$year[modern_co2$month == 6] == y2)
  co2 <- modern_co2$co2[modern_co2$month == 6][i1:i2]

  # .. Also get anomaly, relative to 1981-2010
  T_anomaly <- T - mean(T[year >= 1981 & year <= 2010])

  # Create a data frame
  modern_polar <- data.frame(year, co2, T, T_anomaly)
}
