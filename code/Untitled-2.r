dengue_base <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    # Calculate the total population sizes
    Total_human_population <-  Susceptible_human + Infectious_human + Recovered_human
    Total_mosquito_population <- Susceptible_mosquito+Infectious_mosquito
    
    # Calculate the average force of infection imposed on each susceptible human
    force_of_infection_on_human <- bites *T_HM*Infectious_mosquito/ Total_human_population
    
    # Calculate the average force of infection imposed on each susceptible mosquito
    force_of_infection_on_mosquito <- bites *T_MH*Infectious_human/ Total_human_population
    
    # Calculate the (net) instantaneous change in each compartment
    Susceptible_human_change <- -force_of_infection_on_human*Susceptible_human
    Infectious_human_change <- force_of_infection_on_human*Susceptible_human - Infectious_human / infectious_period 
    Recovered_human_change <- Infectious_human / infectious_period
    Susceptible_mosquito_change <- (mu_M*Total_mosquito_population) - (force_of_infection_on_mosquito+mu_M)*Susceptible_mosquito
    Infectious_mosquito_change <- force_of_infection_on_mosquito * Susceptible_mosquito - mu_M * Infectious_mosquito
    
    # Return net changes as list
    return(list(
      c(
        Susceptible_human_change,
        Infectious_human_change,
        Recovered_human_change,
        Susceptible_mosquito_change,
        Infectious_mosquito_change
      )
    ))
  })
}