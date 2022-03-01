#' Fish Function
#'
#' Write a function that takes a vector of fish names and always returns:
#' the most common fish, 
#' the rarest fish,
#' and the total number of fish.
#' 
#' @param fish, a vector of fish
#' @return total, the total number of fish
#' @return most_common, the most common fish
#' @return rarest, the rarest fish

fish_function = function(fish) {
  total = count(fish)
  fish <- as.factor(fish$fish)
  most_common = names(which.max(summary(fish)))
  rarest = names(which.min(summary(fish)))
  return(c(total, most_common, rarest))
}