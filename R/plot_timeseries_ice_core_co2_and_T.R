#' @export
plot_timeseries_ice_core_co2_and_T = function(ice_core_co2, ice_core_T) {

  year_bp <- ice_core_co2$'Age(yrBP)'
  temperature <- approx(ice_core_T$Age, ice_core_T$Temperature, year_bp,
                        method = "linear", na.rm = TRUE)
  temperature = temperature$y

  par(mar = c(5, 4, 4, 4) + 0.3)                           # Space for second y-axis

  plot(year_bp, ice_core_co2$'CO2(ppmv)', pch = 16, col = "blue",
       xlab = "year", ylab = "CO2 (ppm)",
       main = "Ice Core CO2 & Temperature", type = "l")     # Create first plot
  par(new = TRUE)                                           # Add new plot
  plot(year_bp, temperature, pch = 17, col = "orange",      # Create second plot without axes
       axes = FALSE, xlab = "", ylab = "", type = "l")
  axis(4, at = pretty(range(ice_core_co2$'CO2(ppmv)')))     # Add right axis
  axis(4, ylim=c(-10,4), col="orange",col.axis="orange",las=1)
  mtext("Temperature Anomaly (C)", side = 4, line = 3, col = "orange")  # Add second axis label
}
