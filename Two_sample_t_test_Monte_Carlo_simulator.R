

# install the required packages -------------------------------------------

if (!require('matrixTests')) 
  install.packages('matrixTests')

if (!require('shiny'))     
  install.packages('shiny')

if (!require('shinydashboard')) 
  install.packages('shinydashboard')

if (!require('shinyFeedback')) 
  install.packages('shinyFeedback')

if (!require('shinyjs')) 
  install.packages('shinyjs')

if (!require('shinyWidgets')) 
  install.packages('shinyWidgets')


#devtools::install_github('rstudio/leaflet')

# global.ui ---------------------------------------------------------------

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
      label = "Standard deviation:",
      min   = 0.1,
      max   = 10,
      value = 0.3,
      step  = 0.1
    ),
    
    sliderInput(
      ns("sample_size"),
      label = "Sample size:",
      min   =   2,
      max   = 500,
      value = 100,
      step  =   1 
    ),
    
    selectInput(
      ns("distributiontype"),
      label = "Distribution type",
      selected = "norm",
      choices = c(
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
      label   = "The variances are the same:",
      choices = c("No"  = "n",
                  "Yes" = "y")
    ),
    
    numericInput(
      ns("alpha"),
      label = "Probability of error:",
      min   =  0.0001,
      max   = 99.9999,
      value =    0.05,
      step  =  0.0001 
    ),
    
    numericInput(
      ns("sim_steps"),
      label = "Number of simulation steps:",
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
    plotOutput(ns("dist")),
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

# global.func -------------------------------------------------------------

group_parameter_server <- function(input, output, session) {
  
  # Update the slider input controls and in the meantime pause the simulation 
  # as well
  #
  # Returns:
  #   Logical value representing the "Update in progress"- flag
  
  # Initialize the "Update in progress"- flag 
  update_in_progress <- reactiveVal(FALSE)
  
  # Check if the distribution type has changed
  observeEvent(input$distributiontype, {
    
    # The distribution type has changed
    # Set the "Update in progress"- flag to TRUE
    update_in_progress(TRUE)
    
    # Activate the slider input control for the standard derivative
    # Some distribution types have no standard deviation, so the
    # slider input control is disabled for them
    enable("std_dev")  
    
    # Create empty lists
    exp_val <- list()
    
    std_dev <- list() 
    
    # Initialize the distribution parameter after changing the type
    switch(
      input$distributiontype,
      "norm" = {
        enable("std_dev")
        exp_val <- list(
          min   = -10,
          max   = 10,
          value = 0.3,
          step  = 0.1
        )
        std_dev <- list(
          min   =  0.1,
          max   = 10,
          value = 0.3,
          step  = 0.1
        )
      },
      "uni" = {
        enable("std_dev")
        exp_val <- list(
          min   = -10,
          max   = 10,
          value = 0.3,
          step  = 0.1
        )
        std_dev <- list(
          min   =  0.1,
          max   = 10,
          value = 0.3,
          step  = 0.1
        )
      },
      "exp" = {
        disable("std_dev")
        exp_val <- list(
          min   = 0.1,
          max   =  25,
          value =   0,
          steps = 0.01
        )
        std_dev <- list(
          min   = 0.1,
          max   =  25,
          value =   1,
          steps = 0.1
        )
      },
      "gamma" = {
        enable("std_dev")
        exp_val <- list(
          min   = 0.1,
          max   =  25,
          value =   1,
          step  = 0.1
        )
        std_dev <- list(
          min   = 0.1,
          max   =  25,
          value =   1,
          step  = 0.1
        )
      },
      "logis" = {
        enable("std_dev")
        exp_val <- list(
          value =     0,
          min   =   -25,
          max   =    25,
          step  =  0.01
        )
        std_dev <- list(
          min   = 0.1,
          max   =  25,
          value =   1,
          step  = 0.1
        )
      },
      input$distributiontype
    )
    
    # update the slider inputs with the initialized values
    updateSliderInput(
      session,
      "exp_val",
      min   = exp_val$min,
      max   = exp_val$max, 
      value = exp_val$value,
      step  = exp_val$step
    )
    
    updateSliderInput(
      session,
      "std_dev",
      min   = std_dev$min,
      max   = std_dev$max,
      value = std_dev$value,
      step  = std_dev$step
    )
  }, ignoreInit = TRUE)
  
  # Interrupt the simulation for 150ms
  updateTrigger <- debounce(reactive({
    req(input$distributiontype) 
  }), 150)
  
  # Update is done
  # Set the "Update in progress"- flag to FALSE
  observeEvent(req(updateTrigger()), {
    update_in_progress(FALSE)
  }, ignoreInit = TRUE)
  
  
  return(update_in_progress)
  
}

r_dist <- function(exp_val, std_dev, smpl_size, dist_type) {
  
  # Take random observations that follow the chosen distribution                          
  # 
  # Args:
  #   exp_val:   Numerical value representing the mean                                      
  #
  #   std_dev:   Numerical value representing the standard deviation                        
  #
  #   smpl_size: Numerical value representing the sample size                               
  #
  #   dist_type: Character value indicating the type of distribution 
  #              from which the random observations are taken 
  # 
  # Returns:
  #   Numerical vector representing the sample from the selected distribution                 
  
  switch (
    dist_type,
    "norm" = {
      y <- rnorm(n    = smpl_size,
                 mean = exp_val,
                 sd   = std_dev)
      return(y)
    },
    "uni" = {
      y <- runif(
        n   = smpl_size,
        min = (2 * exp_val - 3.464102 * std_dev) / 2,
        max = (2 * exp_val + 3.464102 * std_dev) / 2
      )
      return(y)
    },
    "exp" = {
      y <- rexp(n    = smpl_size,
                rate = 1 / exp_val)
      return(y)
    },
    "gamma" = {
      y <- rgamma(
        n     = smpl_size,
        shape = (exp_val / std_dev)^2,
        scale = std_dev^2 / exp_val
      )
      return(y)
    },
    "logis" = {
      y <- rlogis(n        = smpl_size,
                  location = exp_val,
                  scale    = 1.732051 * std_dev / pi)
      return(y)
    }
  )
  
}

d_dist <- function(exp_val, std_dev, x_range, dist_type) {
  
  # Calculate the density for the density plot
  # 
  # Args:
  #   exp_val:   Numerical value representing the mean                                      
  #
  #   std_dev:   Numerical value representing the standard deviation                    
  #
  #   x_range:   Numeric value representing the range of the x-axis of the          
  #              displayed distribution
  #
  #   dist_type: Character value for the distribution type to be displayed
  # 
  #
  # Returns:
  #   Data frame representing the density data
  
  switch (
    dist_type,
    "norm" = {
      x  <- seq(-(10 + x_range), 10 + x_range, 0.01)
      y  <- dnorm(x, 
                  mean = exp_val, 
                  sd   = std_dev)
      df <- data.frame(x, y)
      return(df)
    },
    "uni" = {
      x <- seq(-(10 + x_range), 10 + x_range, 0.01)
      y <- dunif(
        x,
        min = (2 * exp_val - 3.464102 * std_dev) / 2,
        max = (2 * exp_val + 3.464102 * std_dev) / 2
      )
      df <- data.frame(x, y)
      return(df)
    },
    "exp" = {
      x <- seq(0.01, 10 + x_range, 0.01)
      y <- dexp(x, 
                rate = 1 / exp_val)
      df <- data.frame(x, y)
      return(df)
    },
    "gamma" = {
      x <- seq(0.01, 10 + x_range, 0.01)
      y <- dgamma(x,
                  shape = (exp_val / std_dev)^2,
                  scale = std_dev^2 / exp_val)
      df <- data.frame(x, y)
      return(df)
    },
    "logis" = {
      x <- seq(-(10 + x_range), 10 + x_range, 0.01)
      y <- dlogis(x,
                  location = exp_val,
                  scale    = 1.732051 * std_dev / pi)
      df <- data.frame(x, y)
      return(df)
    }
  )
}

plot_dist_server <- function(input, output, session, data_input, update) {
  
  # Plot the density of the selected distribution                                           
  # 
  # Args:
  #   data_input: List representing the parameter of a group:                              
  #                 exp_val, 
  #                 std_dev, 
  #                 sample_size, 
  #                 distribution type
  #               
  #   update:     Logical value specifying the update flag to check 
  #               if an update is in progress caused by a change
  #               in the distribution type
  
  # Get the density data
  data_dist <- reactive({
    
    # Get the distribution type
    dist_type <- isolate(data_input$distributiontype())
    
    # Check if an update is running
    req(!update)
    
    # Get the density data
    data_dist <- d_dist(
      data_input$exp_val(),
      data_input$std_dev(),
      input$x_range,
      dist_type
    )
    
    return(data_dist)
    
  })
  
  # Plot the density of the chosen distribution
  output$dist <- renderPlot({
    plot(
      data_dist()$x,
      data_dist()$y,
      type = "l",
      main = "",
      xlab = "x",
      ylab = "f(x)",
      col  = "#337AB7"
    )
  })
}

run_monte_carlo_simulation <-
  function(x_param, y_param, test_sim_param) {
    
    # Calculate the density for the density plot
    # 
    # Args:
    #   x_param: List representing the parameter of a group: 
    #               exp_val, 
    #               std_dev, 
    #               sample_size, 
    #               distribution type
    #
    #   y_param: List representing the parameter of another group: 
    #               exp_val, 
    #               std_dev, 
    #               sample_size, 
    #               distribution type
    #
    #   test_sim_param: List representing the test parameter: 
    #
    #                     relationship: Character value representing the
    #                                   alternative hypothesis, 
    #
    #                     same_var: Character value indicating whether to 
    #                               treat the two variances as being equal,
    #                 
    #                     alpha: Numerical value representing the confidence                                
    #                            level of the interval, 
    #
    #                     sim_steps: Numerical value representing the simulation 
    #                                steps,
    #
    #                     seed (Numeric)
    #
    # Returns:
    #   Numeric vector representing the simulation results of the Monte Carlo 
    #   simulation
    
    
    # Get the data of group A
    dist_type_group_A <- isolate(x_param$distributiontype())
    
    # Get the data of group B
    dist_type_group_B <- isolate(y_param$distributiontype())
    
    # Is the variance equal?
    var_equal <-
      ifelse(test_sim_param$same_var() == 'n', FALSE, TRUE)
    
    # Set a seed 
    set.seed(test_sim_param$seed())
    
    # Get a random sample from group A
    data_group_A <-
      replicate(
        test_sim_param$sim_steps(),
        r_dist(
          x_param$exp_val(),
          x_param$std_dev(),
          x_param$sample_size(),
          dist_type_group_A
        ),
        simplify = TRUE
      )
    
    # Set a new seed
    set.seed(test_sim_param$seed() + 1)
    
    # Get a random sample from group B
    data_group_B <-
      replicate(
        test_sim_param$sim_steps(),
        r_dist(
          y_param$exp_val(),
          y_param$std_dev(),
          y_param$sample_size(),
          dist_type_group_B
        ),
        simplify = TRUE
      )
    
    # Check if the variance is equal or not
    if (var_equal == TRUE)
    {
      # Calculate the p-value if the variance is equal
      p_value <- col_t_equalvar(
        data_group_A,
        data_group_B,
        alternative = test_sim_param$relationship(),
        conf.level  = 1 - test_sim_param$alpha()
      )$pvalue
    } else {
      # Calculate the p-value if the variance is unequal
      p_value <- col_t_welch(
        data_group_A,
        data_group_B,
        alternative = test_sim_param$relationship(),
        conf.level  = 1 - test_sim_param$alpha()
      )$pvalue
    }
    
    return(p_value)
  }

group_data_server <- function(input, output, session) {
  
  # Function to get the group input data
  
  return(
    list(
      exp_val          = reactive({ input$exp_val }),
      std_dev          = reactive({ input$std_dev }),
      sample_size      = reactive({ input$sample_size }),
      distributiontype = reactive({ input$distributiontype })
    ) 
  )
}

test_sim_data_server <- function(input, output, session) {
  
  # Function to get the parameter input data
  
  return(
    list(
      relationship  = reactive({ input$relationship }),
      same_var      = reactive({ input$same_var }),
      alpha         = reactive({ input$alpha }),
      sim_steps     = reactive({ input$sim_steps }),
      seed          = reactive({ input$seed })
    )
  )
}

test_sim_validate_input_server <- function(input, output, session) {
  
  # Function for checking incorrect entries
  # 
  # Returns:
  #   Logical value representing the "input error"- flag                                             
  
  # Set the "input error"- flag to FALSE
  input_error <- reactiveVal(FALSE)
  
  # Check for an incorrect alpha
  input_error_alpha  <- reactive({
    if (is.na(input$alpha) | input$alpha <= 0 | input$alpha >= 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Check for incorrect simulation steps
  input_error_sim_steps  <- reactive({
    if (is.na(input$sim_steps) | input$sim_steps < 2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Check for an incorrect seed
  input_error_seed <- reactive({
    if (is.na(input$seed)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Issue a warning message
  observe({
    
    feedbackDanger("alpha",
                   input_error_alpha(),
                   "Please select a number between 0 and 1")
    
    feedbackDanger("sim_steps",
                   input_error_sim_steps(),
                   "Please select at least two simulation steps")
    
    feedbackDanger("seed",
                   input_error_seed(),
                   "Seed must be an integer")
    
    input_error(input_error_alpha() |
                  input_error_sim_steps() | input_error_seed())
    
  })
  
  return(input_error)
}

# dashboard.ui ------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Monte Carlo simulation for the unpaired two-sample t-test",
                  titleWidth = 550,
                  tags$li(
                    class = "dropdown",
                    tags$style(
                      ".bttn-simple.bttn-primary {
                          text-align: left; 
                          background: #337CA5; 
                          margin-bottom: 5px
                        }
                       .bttn-simple.bttn-lg {
                      background: #337CA5}"
                    ), 
                    
                    dropdown(
                      icon  = icon("align-justify"),
                      right = TRUE,
                      style = "simple",
                      size  = "lg",
                      width = 250,
                      
                      downloadBttn(
                        "save_data",
                        label = "Save data",
                        style = "simple",
                        color = "primary",
                        block = TRUE
                      ),
                      
                      downloadBttn(
                        "save_plot",
                        label = "Save plot",
                        style = "simple",
                        color = "primary",
                        block = TRUE
                      ),
                      
                      downloadBttn(
                        "save_parameter",
                        label = "Save parameter",
                        style = "simple",
                        color = "primary",
                        block = TRUE
                      )
                    )
                  )
  ), 
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyFeedback(),
    useShinyjs(),
    tags$style(".recalculating { opacity: inherit !important; }"),
    fluidRow(
      column(
        6,
        tabBox(
          width = "100%",
          title = "Distribution plot",
          tabPanel("Group A",
                   plot_dist_ui("dist_group_A")),
          tabPanel("Group B",
                   plot_dist_ui("dist_group_B"))
        )
      ),
      column(
        6,
        box(
          title       = "Histogram",
          width       = "100%",
          status      = "primary",
          solidHeader = TRUE,
          plotOutput("hist")
        ),
        box(
          title       = "Hits",
          width       = "100%",
          status      = "primary",
          solidHeader = TRUE,
          textOutput("hits")
        )
      )
    ),
    fluidRow(
      column(
        4,
        box(
          title       = "Parameter of group A",
          height      = 450,
          width       = "100%",
          solidHeader = TRUE,
          group_parameter_ui("parameter_group_A")
        )
      ),
      column(
        4,
        box(
          title       = "Parameter of group B",
          height      = 450,
          width       = "100%",
          solidHeader = TRUE,
          group_parameter_ui("parameter_group_B")
        )
      ),
      column(
        4,
        box(
          title       = "Test and simulation parameter",
          height      = 450,
          width       = "100%",
          solidHeader = TRUE,
          test_sim_parameter_ui("test_sim_parameter")
        )
      )
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Get the input data ----------------------------------------------------
  
  parameter_group_A <-
    callModule(group_data_server, "parameter_group_A") 
  
  parameter_group_B <-
    callModule(group_data_server, "parameter_group_B")
  
  test_sim_parameter <-
    callModule(test_sim_data_server, "test_sim_parameter")
  
  # Update the group input ui ---------------------------------------------
  
  update_in_progress_group_A <- 
    callModule(group_parameter_server, "parameter_group_A")
  
  update_in_progress_group_B <-
    callModule(group_parameter_server, "parameter_group_B")
  
  # Plot the distribution -------------------------------------------------
  
  callModule(
    plot_dist_server,
    "dist_group_A",
    parameter_group_A,
    update_in_progress_group_A()
  )
  
  callModule(
    plot_dist_server,
    "dist_group_B",
    parameter_group_B,
    update_in_progress_group_B()
  )
  
  # Validate the input ----------------------------------------------------
  
  input_error <-
    callModule(test_sim_validate_input_server, "test_sim_parameter")
  
  # Monte Carlo Simulation ------------------------------------------------
  
  monte_carlo_sim_result <- reactive({
    
    # Stop the simulation if an update is in progress or an error has occurred
    # and in the meantime save the last result
    req(!update_in_progress_group_A(),
        !update_in_progress_group_B(), 
        !input_error(),
        cancelOutput = TRUE)
    
    # Get the results       
    sim_result <-
      run_monte_carlo_simulation(parameter_group_A,
                                 parameter_group_B,
                                 test_sim_parameter) 
    
    # Calculate the hits
    hits <-
      length(which(sim_result < test_sim_parameter$alpha()))
    
    # Put the simulation results and the hits in a list 
    sum_sim_result <- list(plot = sim_result,
                           hits = hits)
    
    return(sum_sim_result)
  })
  
  # Plot the results ------------------------------------------------------
  
  # Get the plot data
  hist_plot <- function() {
    hist(
      monte_carlo_sim_result()$plot,
      breaks = "Scott",
      main   = "",
      xlab   = "p-Value",
      ylab   = "Counts",
      col    = "#337CA5",
      border = "#ffffff"
    )
  }
  
  # Plot the histogram
  output$hist <- renderPlot({
    hist_plot()
  })
  
  # Plot the hits
  output$hits <- renderText({
    monte_carlo_sim_result()$hits
  })
  
  
  # Download  ---------------------------------------------------------------
  
  # Save the simulation data as CSV
  output$save_data <- downloadHandler(
    filename = function() {
      paste0("monte_carlo_sim_data_", Sys.Date(), ".tsv", sep = "")
    },
    
    content = function(file) {
      write.csv(monte_carlo_sim_result()$plot, file)
    }
  )
  
  # Save the histogram of the simulation result as PNG
  output$save_plot <- downloadHandler(
    filename = function() {
      paste0("monte_carlo_sim_plot_", Sys.Date(), ".png")
    },
    
    content = function(file) {
      png(file)
      hist_plot()
      dev.off()
    }
  ) 
  
  # Save the simulation parameter of each group as CSV
  output$save_parameter <- downloadHandler(
    filename = function() {
      paste0("monte_carlo_sim_parameter_", Sys.Date(), ".tsv", sep = "")
    },
    
    content = function(file) {
      write.csv(inputs_to_save(), file)
    }
  ) 
  
  # Get the data that should be saved
  inputs_to_save <- reactive({
    
    # Get the parameter of group A
    group_A <- sapply(1:4, function(i)
      parameter_group_A[[i]]())
    names(group_A) <- names(parameter_group_A)
    
    # Get the parameter of group B
    group_B <- sapply(1:4, function(i)
      parameter_group_B[[i]]())
    names(group_B) <- names(parameter_group_B)
    
    # Get the simulation parameter
    test <- sapply(1:5, function(i)
      test_sim_parameter[[i]]())
    names(test) <- names(test_sim_parameter)
    
    return(c(
      Group_A = group_A,
      Group_B = group_B,
      Test    = test
    ))
  })
}

# Run the application 
shinyApp(ui, server)

