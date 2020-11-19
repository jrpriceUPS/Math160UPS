#' Plot Modern Timeseries
#'
#' Plots simultaneous timeseries of CO2 and temperature (Penny Rowe - Polar project).
#' @export
plot_timeseries_modern_co2_and_T = function(df) {
  # Plot co2 and T as timeseries on the same graph
  par(mar = c(5, 4, 4, 4) + 0.3)                                 # Extra space for right y-axis
  plot(df$year, df$co2, pch = 16, col = "blue",
       xlab = "year", ylab="", type = "l")                       # CO2 will use left axis
  mtext("CO2 (ppm)", side = 2, line = 3, col = "blue")           # Add left axis label
  par(new = TRUE)                                                # Add new plot
  plot(df$year, df$T, pch = 17, col = "orange",                  # T will use right axis
       axes = FALSE, xlab = "", ylab = "",
       type = "l")
  axis(4, at = pretty(range(df$T)))                              # Add right axis
  axis(4, ylim=c(0,25), col="orange",col.axis="orange",las=1)
  mtext("Temperature (C)", side = 4, line = 3, col = "orange")   # Add right axis label
}
