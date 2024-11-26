library(shiny)
library(dplyr)
library(deSolve)
library(plotly)
library(tidyr)

# Define server logic required to draw plots
function(input, output, session) {
  
  values <- reactiveValues()
  
  # SIR model parameters update
  observeEvent(c(input$bites_rate, input$T_HM, input$T_MH, input$infectious_period,
                 input$birth_M,input$death_M), {
    values$bites_rate <- input$bites_rate
    values$T_HM <- input$T_HM
    values$T_MH <- input$T_MH
    values$infectious_period <- input$infectious_period
    values$birth_M <- input$birth_M
    values$death_M <- input$death_M
  })
  
  # Define the SIR model function
  dengue_base <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      Total_human_population <- Susceptible_human + Infectious_human + Recovered_human
      Total_mosquito_population <- Susceptible_mosquito + Infectious_mosquito
      
      force_of_infection_on_human <- bites * T_HM * Infectious_mosquito / Total_human_population
      force_of_infection_on_mosquito <- bites * T_MH * Infectious_human / Total_human_population
      
      Susceptible_human_change <- -force_of_infection_on_human * Susceptible_human
      Infectious_human_change <- force_of_infection_on_human * Susceptible_human - Infectious_human / infectious_period
      Recovered_human_change <- Infectious_human / infectious_period
      Susceptible_mosquito_change <- birth_M * Total_mosquito_population - (force_of_infection_on_mosquito + death_M) * Susceptible_mosquito
      Infectious_mosquito_change <- force_of_infection_on_mosquito * Susceptible_mosquito - death_M * Infectious_mosquito
      
      return(list(c(
        Susceptible_human_change, Infectious_human_change, Recovered_human_change,
        Susceptible_mosquito_change, Infectious_mosquito_change
      )))
    })
  }
  
  output$humanPlot <- renderPlotly({

    parameters <- c(
      bites = values$bites_rate,
      T_HM = values$T_HM,
      T_MH = values$T_MH,
      infectious_period = values$infectious_period,
      birth_M = values$birth_M,
      death_M = values$death_M
    )
    
    
    
    state <- c(
      Susceptible_human = 149998,
      Infectious_human = 2,
      Recovered_human = 0,
      Susceptible_mosquito = 224990,
      Infectious_mosquito = 10
    )
    
    times <- seq(0, 1000, by = 1)
    output <- ode(y = state, times = times, func = dengue_base, parms = parameters)
    output_df <- as.data.frame(output)
    
    human_df <- output_df %>%
      select(time, Susceptible_human, Infectious_human, Recovered_human) %>%
      pivot_longer(cols = -time, names_to = "Compartment", values_to = "Count")
    
    color_map <- c(Susceptible_human = "blue3", Infectious_human = "red2", Recovered_human = "green2")
    
    plot_ly(human_df, x = ~time, y = ~Count, color = ~Compartment, colors = color_map, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "SIR Human Population Dynamics",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Population Count")
      )
  })
  
  output$mosquitoPlot <- renderPlotly({
    parameters <- c(
      bites = values$bites_rate,
      T_HM = values$T_HM,
      T_MH = values$T_MH,
      infectious_period = values$infectious_period,
      birth_M = values$birth_M,
      death_M = values$death_M
    )
    
    state <- c(
      Susceptible_human = 149998,
      Infectious_human = 2,
      Recovered_human = 0,
      Susceptible_mosquito = 224990,
      Infectious_mosquito = 10
    )
    
    times <- seq(0, 1000, by = 1)
    output <- ode(y = state, times = times, func = dengue_base, parms = parameters)
    output_df <- as.data.frame(output)
    
    mosquito_df <- output_df %>%
      select(time, Susceptible_mosquito, Infectious_mosquito) %>%
      pivot_longer(cols = -time, names_to = "Compartment", values_to = "Count")
    
    color_map <- c(Susceptible_mosquito = "blue3", Infectious_mosquito = "red2")
    
    
    plot_ly(mosquito_df, x = ~time, y = ~Count, color = ~Compartment, colors = color_map, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "SIR Mosquito Population Dynamics",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Population Count")
      )
  })
  
  # SEIR model parameters update
  observeEvent(c(input$bites_rate_E, input$T_HM_E, input$T_MH_E, input$infectious_period_E, 
                 input$birth_M_E,input$death_M_E, input$exposed_period_H, input$exposed_period_M), {
                   values$bites_rate_E <- input$bites_rate_E
                   values$T_HM_E <- input$T_HM_E
                   values$T_MH_E <- input$T_MH_E
                   values$infectious_period_E <- input$infectious_period_E
                   values$birth_M_E <- input$birth_M_E
                   values$death_M_E <- input$death_M_E
                   values$exposed_period_H <- input$exposed_period_H
                   values$exposed_period_M <- input$exposed_period_M
                 })
  
  # Define the SEIR model function
  dengue_with_E <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      Total_human_population <- Susceptible_human + Exposed_human + Infectious_human + Recovered_human
      Total_mosquito_population <- Susceptible_mosquito + Exposed_mosquito + Infectious_mosquito
      
      force_of_infection_on_human <- bites * T_HM * Infectious_mosquito / Total_human_population
      force_of_infection_on_mosquito <- bites * T_MH * Infectious_human / Total_human_population
      
      Susceptible_human_change <- -force_of_infection_on_human * Susceptible_human
      Exposed_human_change <- force_of_infection_on_human * Susceptible_human - Exposed_human / exposed_period_H
      Infectious_human_change <- Exposed_human / exposed_period_H - Infectious_human / infectious_period
      Recovered_human_change <- Infectious_human / infectious_period
      Susceptible_mosquito_change <- birth_M_E * Total_mosquito_population - (force_of_infection_on_mosquito + death_M_E) * Susceptible_mosquito
      Exposed_mosquito_change <- force_of_infection_on_mosquito * Susceptible_mosquito - Exposed_mosquito * (1 / exposed_period_M + death_M_E)
      Infectious_mosquito_change <- Exposed_mosquito * (1 / exposed_period_M) - death_M_E * Infectious_mosquito
      
      return(list(c(
        Susceptible_human_change, Exposed_human_change, Infectious_human_change, Recovered_human_change,
        Susceptible_mosquito_change, Exposed_mosquito_change, Infectious_mosquito_change
      )))
    })
  }
  
  output$seirHumanPlot <- renderPlotly({
    parameters <- c(
      bites = values$bites_rate_E,
      T_HM = values$T_HM_E,
      T_MH = values$T_MH_E,
      infectious_period = values$infectious_period_E,
      birth_M_E = values$birth_M_E,
      death_M_E = values$death_M_E,
      exposed_period_H = values$exposed_period_H,
      exposed_period_M = values$exposed_period_M
    )
    
    state <- c(
      Susceptible_human = 149998,
      Exposed_human = 0,
      Infectious_human = 2,
      Recovered_human = 0,
      Susceptible_mosquito = 109990,
      Exposed_mosquito = 0,
      Infectious_mosquito = 10
    )
    
    times <- seq(0, 1000, by = 1)
    output <- ode(y = state, times = times, func = dengue_with_E, parms = parameters)
    output_df <- as.data.frame(output)
    
    human_df <- output_df %>%
      select(time, Susceptible_human, Exposed_human, Infectious_human, Recovered_human) %>%
      pivot_longer(cols = -time, names_to = "Compartment", values_to = "Count")
    
    color_map <- c(Susceptible_human = "blue3",Exposed_human = "yellow2", Infectious_human = "red2", Recovered_human = "green2")
    
    plot_ly(human_df, x = ~time, y = ~Count, color = ~Compartment, colors = color_map, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "SEIR Human Population Dynamics",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Population Count")
      )
  })
  
  output$seirMosquitoPlot <- renderPlotly({
    parameters <- c(
      bites = values$bites_rate_E,
      T_HM = values$T_HM_E,
      T_MH = values$T_MH_E,
      infectious_period = values$infectious_period_E,
      birth_M_E = values$birth_M_E,
      death_M_E = values$death_M_E,
      exposed_period_H = values$exposed_period_H,
      exposed_period_M = values$exposed_period_M
    )
    
    state <- c(
      Susceptible_human = 149998,
      Exposed_human = 0,
      Infectious_human = 2,
      Recovered_human = 0,
      Susceptible_mosquito = 109990,
      Exposed_mosquito = 0,
      Infectious_mosquito = 10
    )
    
    times <- seq(0, 1000, by = 1)
    output <- ode(y = state, times = times, func = dengue_with_E, parms = parameters)
    output_df <- as.data.frame(output)
    
    mosquito_df <- output_df %>%
      select(time, Susceptible_mosquito, Exposed_mosquito, Infectious_mosquito) %>%
      pivot_longer(cols = -time, names_to = "Compartment", values_to = "Count")
    
    color_map <- c(Susceptible_mosquito = "blue3",Exposed_mosquito = "yellow2", Infectious_mosquito = "red2")
    
    plot_ly(mosquito_df, x = ~time, y = ~Count, color = ~Compartment,colors=color_map, type = 'scatter', mode = 'lines') %>%
      layout(
        title = "SEIR Mosquito Population Dynamics",
        xaxis = list(title = "Time"),
        yaxis = list(title = "Population Count")
      )
  })
}