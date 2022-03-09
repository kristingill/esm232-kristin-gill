#' temperature risk
#'
#' compute temperature risk for urban dwellers
#' @param temp (C) (at least 5 days)
#' @param temp_threshold (C) (default 30)
#' @param trees number of trees in the area
#' @param buildings number of buildings in the area
#' @param consecutive_days length of the heat event
#' @param income yearly income of the individual
#' @return risk (high, med, low)

temp_risk = function(temp, consecutive_days, temp_threshold = 30, trees, income, buildings) {

  usethis::use_package('glue')

  use_tidy_dependencies()

  # create empty lists
  temp_over <- list()

  temp_final <- list()

  risk_equation <- 0

  mean_temp <- 0

  mean_temp_all <- list()

  cons_days_all <- list()

  risk <- list()

  for (i in 1:length(temp)) {

    # if temperature is over the temperature threshold, add it to the list
    if (temp[i] > temp_threshold) {
      temp_over <- append(temp_over, temp[i])
    }

    else {
      # how many days the temperature is over the threshold, can count this now that the condition is no longer met
      cons_days <- length(temp_over)

      # if the length of time that temperature is over the threshold meets our conditions for an extreme event,
      # add this list of temperatures to the final list
      if (cons_days >= consecutive_days){
        temp_final <- append(temp_final, temp_over)
        mean_temp <- mean(as.numeric(temp_over))
        mean_temp_all <- append(mean_temp_all, mean_temp)
        risk_equation <- (cons_days*mean_temp*buildings)/(trees*income)
        risk <- append(risk, risk_equation)
        cons_days_all <- append(cons_days_all, cons_days)
      }

      # restart the empty list to find the next event
      temp_over = list()
    }
  }

  numb_events <- as.character(length(risk))

  print(glue::glue("The number of heat events is {numb_events}."))

  risk <- as.data.frame(unlist(risk)) %>%
    rename("risk_value" = "unlist(risk)") %>%
    mutate(risk_level =case_when(
      risk >= 0.1 ~ "high",
      risk < 0.1 ~ "low"
    )) %>%
    mutate(mean_temp = unlist(mean_temp_all)) %>%
    mutate(cons_days = unlist(cons_days_all))

  return(risk = risk)
}
