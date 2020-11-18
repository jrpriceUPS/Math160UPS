#' @export
get_ice_core_T_and_co2_on_same_grid = function(ice_core_co2, ice_core_T) {
  # .. Next we will look at temperature and CO2 in ice core records
  #    You already loaded in the ice core data. Plot it
  # .. Use CO2 dates for scatter plot of CO2 vs T
  #    we will linearly interpolate the temperatures onto these dates
  year_bp <- ice_core_co2$'Age(yrBP)'
  temperature <- approx(ice_core_T$Age, ice_core_T$Temperature, year_bp,
                        method = "linear", na.rm = TRUE)
  temperature = temperature$y

  co2 = ice_core_co2$'CO2(ppmv)'

  # Create a data frame
  ice_core <- data.frame(year_bp, co2, temperature)
}
