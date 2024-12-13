required_packages <- (c("dplyr",
                        "deSolve",
                        "ggplot2"))
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
# Model parameters
parameters <- c(
  bites = 0.63, # number of bites per mosquito per day
  T_HM = 0.26, # probability of transmission - mosquito to human
  T_MH = 0.26, # DESCRIPTION
  infectious_period = 5, # days
  mu_M = 1/14 # DESCRIPTION
)

start_date = as.Date("2008-11-02")
end_date = as.Date("2009-05-31")
times = seq(start_date, end_date, 1)

# Initial conditions
human_population <- 150000 # population of Cairns in 2008
initial_human_susceptible <- 150000-2
initial_human_infectious <- 2
initial_human_recovered <- 0
mosquito_population <- 225000 
initial_mosquito_susceptible <- 224990
initial_mosquito_infectious <- 10

state <- c(Susceptible_human = initial_human_susceptible,
           Infectious_human = initial_human_infectious,
           Recovered_human = initial_human_recovered,
           Susceptible_mosquito = initial_mosquito_susceptible,
           Infectious_mosquito = initial_mosquito_infectious)

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

# Solve model
out_dengue_base <- ode(y = state, times = as.numeric(times - times[1]), func = dengue_base, parms = parameters)

# Plot solution
plot(out_dengue_base, 
     main = c("Susceptible humans", "Infectious humans", "Recovered humans",
              "Susceptible mosquitoes", "Infectious mosquitoes"), 
     xlab = "Time", ylab = c("population size"))

# Model function
dengue_base2 <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    # Calculate the total population sizes
    Total_human_population <- Susceptible_human + Infectious_human + Recovered_human
    Total_mosquito_population <- Susceptible_mosquito + Infectious_mosquito
    
    # Calculate the average force of infection imposed on each susceptible human
    force_of_infection_on_human <- bites * T_HM * Infectious_mosquito / Total_human_population
    
    # Calculate the average force of infection imposed on each susceptible mosquito
    force_of_infection_on_mosquito <- bites * T_MH * Infectious_human / Total_human_population
    
    # Calculate the (net) instantaneous change in each compartment
    Susceptible_human_change <- -force_of_infection_on_human * Susceptible_human
    Infectious_human_change <- force_of_infection_on_human * Susceptible_human - Infectious_human / infectious_period
    Recovered_human_change <- Infectious_human / infectious_period
    Susceptible_mosquito_change <- mu_M * Total_mosquito_population - (force_of_infection_on_mosquito + mu_M) * Susceptible_mosquito 
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

# Solve model
out_dengue_base2 <- ode(y = state, times = as.numeric(times - times[1]), func = dengue_base2, parms = parameters)

# Plot solution
plot(out_dengue_base2, 
     main = c("Susceptible humans", "Infectious humans", "Recovered humans",
              "Susceptible mosquitoes", "Infectious mosquitoes"), 
     xlab = "Time", ylab = c("population size"))
#############################################################
## MODELLING VECTOR-BORNE DISEASES PRACTICAL SESSION PART 2##
#############################################################

# A model for dengue transmission - Part 2
# Some R code to numerically solve a set of ordinary differential equations
# for a modified model of dengue. Results are then plotted.

# Model parameters
parameters <- c(
  bites = 0.63, # number of bites per mosquito per day
  T_HM = 0.26, # probability of transmission - mosquito to human
  T_MH = 0.26, # probability of transmission - human to mosquito
  infectious_period = 5, # days. Duration in infectious compartment
  mu_M = 1/14, # per day. Mosquito birth and death rate.
  exposed_period_H = 10 , # days. Duration in exposed compartment - human
  exposed_period_M = 5.5  # days. Duration in exposed compartment - mosquito
)

# Initial conditions
human_population <- 150000 # population of Cairns in 2008
initial_human_susceptible <- 149998
initial_human_exposed <- 0
initial_human_infectious <- 2
initial_human_recovered <-0
mosquito_population <- 225000 # 3 times human population, divided by 2
initial_mosquito_susceptible <- 224990
initial_mosquito_exposed <- 0
initial_mosquito_infectious <- 0

# Define model compartments, gather into a single variable to capture the state of the system and set to initial values
state <- c(Susceptible_human = initial_human_susceptible,
           Exposed_human = initial_human_exposed,
           Infectious_human = initial_human_infectious,
           Recovered_human = initial_human_recovered,
           Susceptible_mosquito = initial_mosquito_susceptible,
           Exposed_mosquito = initial_mosquito_exposed,
           Infectious_mosquito = initial_mosquito_infectious)

# Time window
start_date = as.Date("2008-11-02")
end_date = as.Date("2011-05-31")
times = seq(start_date, end_date, by = 1)

# Model function
dengue_with_E <- function(t, state, parameters){
  with(as.list(c(state, parameters)), {
    # Calculate the total population sizes
    Total_human_population <- Susceptible_human + Infectious_human + Recovered_human + Exposed_human
    Total_mosquito_population <- Susceptible_mosquito + Infectious_mosquito + Exposed_mosquito
    
    # Calculate the average force of infection imposed on each susceptible human
    force_of_infection_on_human <- bites * T_HM * Infectious_mosquito / Total_human_population
    
    # Calculate the average force of infection imposed on each susceptible mosquito
    force_of_infection_on_mosquito <- bites * T_MH * Infectious_human / Total_human_population
    
    # Calculate the (net) instantaneous change in each compartment
    Susceptible_human_change <- -force_of_infection_on_human * Susceptible_human
    Exposed_human_change <- force_of_infection_on_human * Susceptible_human - Exposed_human / exposed_period_H
    Infectious_human_change <- Exposed_human / exposed_period_H - Infectious_human / infectious_period
    Recovered_human_change <- Infectious_human / infectious_period
    Susceptible_mosquito_change <- mu_M * Total_mosquito_population - (force_of_infection_on_mosquito + mu_M) * Susceptible_mosquito
    Exposed_mosquito_change <- force_of_infection_on_mosquito * Susceptible_mosquito - Exposed_mosquito * (1 / exposed_period_M + mu_M) 
    Infectious_mosquito_change <- Exposed_mosquito / exposed_period_M - mu_M * Infectious_mosquito
    
    # Return net changes as list
    return(list(
      c(
        Susceptible_human_change,
        Exposed_human_change,
        Infectious_human_change,
        Recovered_human_change,
        Susceptible_mosquito_change,
        Exposed_mosquito_change,
        Infectious_mosquito_change
      )
    ))
  })
}

# Solve model
out_dengue_with_E <- ode(y = state, times = as.numeric(times - times[1]), func = dengue_with_E, parms = parameters)

# Plot solution
plot(out_dengue_with_E, 
     main = c("Susceptible humans", "Exposed humans", "Infectious humans", "Recovered humans",
              "Susceptible mosquitoes", "Exposed mosquitoes", "Infectious mosquitoes"), 
     xlab = "Time", ylab = c("population size"))

# Convert ode output to data frame
dengue_with_E_infections <- as.data.frame(out_dengue_with_E[, c("time", "Infectious_human", "Recovered_human")])

# Create columns for new infections and cumulative new infections, 
# Select every 7th row and calculate the difference
dengue_with_E_infections <- dengue_with_E_infections  %>%
  mutate(Infectious_human_change = c(0, diff(Infectious_human)),
         Recovered_human_change = c(0, diff(Recovered_human)),
         New_human_infections = Infectious_human_change + Recovered_human_change,
         Cumulative_human_infections = cumsum(New_human_infections)) %>%
  filter(time %% 7 == 0) %>%
  select(c("time", "Cumulative_human_infections")) %>%
  mutate(Weekly_human_infections = c(0, diff(Cumulative_human_infections)))

# Plot solution
plot(dengue_with_E_infections$time, dengue_with_E_infections$Weekly_human_infections, 
     xlab = "Time", ylab = c("number of infections"),
     main = "Weekly human new infections, model with E")

#############################################################
## MODELLING VECTOR-BORNE DISEASES PRACTICAL SESSION PART 3##
#############################################################

# A model for dengue transmission - Part 3
# Some R code to numerically solve a set of ordinary differential equations
# for a model of dengue with an exposed period and seasonality. 
# Results are then plotted.
# Add seasonality parameters
parameters <- c(
  bites = 0.63, # number of bites per mosquito per day
  T_HM = 0.26, # probability of transmission - mosquito to human
  T_MH = 0.26, # probability of transmission - human to mosquito
  infectious_period = 5, # days. Duration in infectious compartment
  mu_M = 1/14, # per day. Mosquito birth and death rate.
  exposed_period_H = 5.5, # days. Duration in exposed compartment - human
  exposed_period_M = 10, # days. Duration in exposed compartment - mosquito
  death_amplitude = 0.6228, # from Ndii et al.
  death_phase = 20.61 # days, from Ndii et al.
)

# Initial conditions
human_population <- 150000 # population of Cairns in 2008
initial_human_susceptible <- 149998
initial_human_exposed <- 0
initial_human_infectious <- 2
initial_human_recovered <- human_population - initial_human_susceptible - initial_human_infectious - initial_human_exposed
mosquito_population <- 110000 # around half the maximum
initial_mosquito_susceptible <- 109990
initial_mosquito_exposed <- 0
initial_mosquito_infectious <- mosquito_population - initial_mosquito_susceptible - initial_mosquito_exposed

# Define model compartments, gather into a single variable to capture the state of the system and set to initial values
state <- c(Susceptible_human = initial_human_susceptible,
           Exposed_human = initial_human_exposed,
           Infectious_human = initial_human_infectious,
           Recovered_human = initial_human_recovered,
           Susceptible_mosquito = initial_mosquito_susceptible,
           Exposed_mosquito = initial_mosquito_exposed,
           Infectious_mosquito = initial_mosquito_infectious)
# Time window
start_date = as.Date("2008-11-02")
end_date = as.Date("2009-05-31")
times = seq(start_date, end_date, by = 1)

