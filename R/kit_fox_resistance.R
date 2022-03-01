#' Kit Fox Resistance Across a Landscape
#' 
#' This function determines kit fox resistance across a landscape given habitat suitability and sensitivity
#' @param r resistance coefficient (default = 100)
#' @param m resistance coefficient 2 (default = 99)
#' @param c kit fox sensitivity to habitat suitability score. Must be greater than or equal to 0.
#' @param h habitat suitability score. Must be greater than or equal to 0 and less than or equal to 1.
#' @return resistance (ohm)
#' 
#function definition
kit_fox_resistance = function(r=100, m=99, c, h) {
  # make sure that habitat suitability inputs are between 0 and 1. If not, return NA.
  h = ifelse(h >= 0, ifelse(h <=1, h, NA), NA)
  
  # make sure that sensitivity inputs are greater than 0. If not, return NA.
  c = ifelse(c > 0, c, NA)
  
  result = r-m*((1-exp(-c*h))/(1-exp(-c)))
  return(result)
}
