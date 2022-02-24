#' Exponential Population Growth
#'
#' This function determines the population at time t for species whose populations grow exponentially at a constant growth rate.
#' 
#' @param init_pop initial population size
#' @param e Euler's number, default = 2.71
#' @param t time (days)
#' @param r growth rate constant (days^-1)
#' @return final_pop population size at time t

exponential_growth = function(init_pop, e = 2.71, t, r) {
  # error-checking
  if (t < 0) return("Time cannot be negative.")
  if (init_pop < 0) return("Population size cannot be negative")
  # exp growth equation
  final_pop = init_pop*(e^(r*t))
  return(final_pop)
}
