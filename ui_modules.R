
group_parameter_ui <- function(id) {
  
  # Function to display all control elements for the group parameter 
  
  ns <- NS(id)
  
  tagList(
    
    sliderInput(
      ns("exp_val"),
      label = "Expected Value:",
      min   = -10,
      max   =  10,
      value =  0.3,
      step  =  0.1 
    ),
    
    sliderInput(
      ns("std_dev"),
      label = "Standard Deviation:",
      min   = 0.1,
      max   = 10,
      value = 0.3,
      step  = 0.1
    ),
    
    sliderInput(
      ns("sample_size"),
      label = "Sample Size:",
      min   =   2,
      max   = 500,
      value = 100,
      step  =   1 
    ),
    
    selectInput(
      ns("distributiontype"),
      label    = "Distribution Type",
      selected = "norm",
      #class    = "custom-select",
      choices  = c(
        "Normal"      = "norm",
        "Uniform"     = "uni",
        "Exponential" = "exp",
        "Gamma"       = "gamma",
        "Logistic"    = "logis"
      )
    )
  )
}

test_sim_parameter_ui <- function(id) {
  
  # Function to display all control elements for the simulation parameter
  
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("relationship"),
      label = "Relationship:",
      choices = c(
        "Equal"   = "two.sided",
        "Less"    = "less",
        "Greater" = "greater"
      )
    ),
    
    radioButtons(
      ns("same_var"),
      label   = "The Variances are the same:",
      choices = c("No"  = "n",
                  "Yes" = "y")
    ),
    
    numericInput(
      ns("alpha"),
      label = "Probability of Error:",
      min   =  0.0001,
      max   = 99.9999,
      value =    0.05,
      step  =  0.0001 
    ),
    
    numericInput(
      ns("sim_steps"),
      label = "Number of Simulation Steps:",
      min   =     1,
      max   =   Inf,
      value =  1000,
      step  =     1 
    ),
    
    numericInput(
      ns("seed"),
      label = "Seed:",
      min   = -Inf,
      max   =  Inf,
      value =    5,
      step  =    1 
    )
  )
}

plot_dist_ui <- function(id) {
  
  # Function to display the density plot and the slider input where 
  # the slider changes the range of the x-axis 
  
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns("dist")),
    sliderTextInput(
      ns("x_range:"),
      label        = "x-Range",
      choices      = c(0:100),
      hide_min_max = TRUE,
      grid         = FALSE,
      width        = "100%"
    )
  )
}