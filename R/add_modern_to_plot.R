#' Adds modern data to ice core plot
#'
#' Adds modern temp and co2 data to ice core plot (Penny Rowe - Polar project)
#' @export
add_modern_to_plot = function(ice_core, modern_polar, modern_global) {

  modern_global <- get_modern_global_T_anomaly_and_co2(modern_co2, law_dome_co2)

  plot(ice_core$co2, ice_core$temperature,
       ylim=c(-12, 20),
       xlim=c(170, 500), col = 'blue', main = 'Temperature Anomaly',
       xlab = 'CO2 (ppm)', ylab = 'Temperature Anomaly (C)')
  relation <- lm(ice_core$temperature~ice_core$co2)
  abline(relation, cex = 1.3, pch = 16)

  points(ice_core$co2[1:21], ice_core$temperature[1:21], col = 'cyan', pch = 4)
  points(modern_polar$co2, modern_polar$T_anomaly, col = 'green', pch=2)
  points(modern_global$co2, modern_global$T_anomaly, col = 'orange', pch=4)
  points(414, 1.0, col = 'red', pch = 18)

  leg1 = 'Global: Ice core (740,000 - 137 BP)'
  leg2 = 'Global: Ice core (0 - 1883)'
  leg3 = 'Polar: Modern (1958-2020)'
  leg4 = 'Global: Modern (1881-2019)'
  leg5 = 'Global: 2020'
  legend(200, 15, c(leg1, leg2, leg3,leg4,leg5),
         cex=.8,col=c("blue", "cyan", "green","orange","red"),
         pch=c(1,4, 2,4,18))
}
