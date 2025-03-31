


# Modules -----------------------------------------------------------------

group_parameter_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
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
  
  # Interrupt the simulation 
  updateTrigger <- debounce(reactive({
    req(input$distributiontype) 
  }), 150)
  
  # Update is done
  # Set the "Update in progress"- flag to FALSE
  observeEvent(req(updateTrigger()), {
    update_in_progress(FALSE)
  }, ignoreInit = TRUE)
  
  
  return(update_in_progress)
  })
}

group_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  # Function to get the group input data
  
  return(
    list(
      exp_val          = reactive({ input$exp_val }),
      std_dev          = reactive({ input$std_dev }),
      sample_size      = reactive({ input$sample_size }),
      distributiontype = reactive({ input$distributiontype })
    ) 
  )
  })
}

test_sim_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
  })
}

plot_dist_server <- function(id, data_input, update) {
  moduleServer(id, function(input, output, session) {
  
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
  output$dist <- renderPlotly({
    plot_ly(
      x = ~ data_dist()$x,
      y = ~ data_dist()$y,
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#3c7cbf')
    ) %>% layout(
            xaxis = list(
                      title = 'x'
            ),
            yaxis = list(
                      title = 'f(x)'
            ), 
            autosize = T)
   })
  })
}

test_sim_validate_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
  })
}


# Functions ---------------------------------------------------------------

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



