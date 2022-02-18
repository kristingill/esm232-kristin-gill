#' Power required to keep a car moving at a given speed
#'
#' This function determines the power required to keep a car moving at a given speed
#' @param v vehicle speed (m/s)
#' @param m vehicle mass (kg)
#' @param g acceleration due to gravity (m/s) default=9.8
#' @param p_air density of air (kg/m^3) default=1.2
#' @param a surface area of car (m^2)
#' @param crolling rolling coefficient default=0.015
#' @param cdrag aerodynamic resistive coefficient default=0.3
#' @return power (W)
#'
# function definition
power_car = function(velocity, mass, gravity=9.8, p_air=1.2, area, crolling=0.015, cdrag=0.3) {
  result = (crolling * mass * gravity * velocity) + (0.5 * area * p_air * cdrag * (velocity)^3)
  return(result)
}
