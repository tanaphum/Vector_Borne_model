
library(shiny)
library(plotly)

# Define UI for application that draws the population dynamics
fluidPage(
  
  # Application title
  titlePanel("Vector Borne Disease Dynamics"),
  
  tabsetPanel(
    # SIR Model Tab
    tabPanel(
      "SIR Model",
      sidebarLayout(
        sidebarPanel(
          sliderInput("bites_rate",
                      "Number of bites (per mosquito per day):",
                      min = 0,
                      max = 1,
                      value = 0.63,
                      step = 0.01),
          sliderInput("T_HM",
                      "Probability of transmission - mosquito to human:",
                      min = 0,
                      max = 1,
                      value = 0.3,
                      step = 0.01),
          sliderInput("T_MH",
                      "Probability of transmission - human to mosquito:",
                      min = 0,
                      max = 1,
                      value = 0.3,
                      step = 0.01),
          sliderInput("infectious_period",
                      "Duration in infectious compartment (days):",
                      min = 1,
                      max = 30,
                      value = 5,
                      step = 1),
          sliderInput("birth_M",
                      "Mosquito birth rate:",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001),
          sliderInput("death_M",
                      "Mosquito death rate:",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Human Population Dynamics", plotlyOutput("humanPlot")),
            tabPanel("Mosquito Population Dynamics", plotlyOutput("mosquitoPlot"))
          )
        )
      )
    ),
    
    # SEIR Model Tab
    tabPanel(
      "SEIR Model",
      sidebarLayout(
        sidebarPanel(
          sliderInput("bites_rate_E",
                      "Number of bites (per mosquito per day):",
                      min = 0,
                      max = 1,
                      value = 0.63,
                      step = 0.01),
          sliderInput("T_HM_E",
                      "Probability of transmission - mosquito to human:",
                      min = 0,
                      max = 1,
                      value = 0.3,
                      step = 0.01),
          sliderInput("T_MH_E",
                      "Probability of transmission - human to mosquito:",
                      min = 0,
                      max = 1,
                      value = 0.3,
                      step = 0.01),
          sliderInput("infectious_period_E",
                      "Duration in infectious compartment (days):",
                      min = 1,
                      max = 30,
                      value = 5,
                      step = 1),
          sliderInput("birth_M_E",
                      "Mosquito birth rate:",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001),
          sliderInput("death_M_E",
                      "Mosquito death rate:",
                      min = 0,
                      max = 0.1,
                      value = 0.01,
                      step = 0.001),
          sliderInput("exposed_period_H",
                      "Duration in exposed compartment - human (days):",
                      min = 1,
                      max = 30,
                      value = 10,
                      step = 1),
          sliderInput("exposed_period_M",
                      "Duration in exposed compartment - mosquito (days):",
                      min = 1,
                      max = 30,
                      value = 5,
                      step = 1)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Human Population Dynamics", plotlyOutput("seirHumanPlot")),
            tabPanel("Mosquito Population Dynamics", plotlyOutput("seirMosquitoPlot"))
          )
        )
      )
    )
  )
)