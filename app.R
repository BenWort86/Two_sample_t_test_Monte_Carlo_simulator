
# install the required packages -------------------------------------------

if (!require('matrixTests')) 
  install.packages('matrixTests')

if (!require('shinyFeedback')) 
  install.packages('shinyFeedback')

if (!require('bslib')) 
  install.packages('bslib')

if (!require('shinyWidgets')) 
  install.packages('shinyWidgets')

if (!require('plotly')) 
  install.packages('plotly')

if (!require('shinyjs')) 
  install.packages('shinyjs')

source('ui.R')
source('functions.R')


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Get the input data ----------------------------------------------------
  
  parameter_group_A  <- group_data_server("parameter_group_A") 
  
  parameter_group_B  <- group_data_server("parameter_group_B")
  
  test_sim_parameter <- test_sim_data_server("test_sim_parameter")
  
  # Update the group input ui ---------------------------------------------
  
  update_in_progress_group_A <- group_parameter_server("parameter_group_A")
  
  update_in_progress_group_B <- group_parameter_server("parameter_group_B")
  
  # Plot the distribution -------------------------------------------------

  plot_dist_server("dist_group_A",
                   parameter_group_A,
                   update_in_progress_group_A())
  
  plot_dist_server("dist_group_B",
                   parameter_group_B,
                   update_in_progress_group_B())
  
  # Validate the input ----------------------------------------------------
  
  input_error <- test_sim_validate_input_server("test_sim_parameter")
  
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
  
  # Plot the histogram
  output$hist <- renderPlotly({
    plot_ly(
      x = ~monte_carlo_sim_result()$plot,
      type  = 'histogram', 
      color = I('#3c7cbf')
    ) %>% layout(
            bargap = 0.1,
            xaxis  = list(
                      title = 'p-Value'
            ),
            yaxis  = list(
                      title    = 'Counts',
                      showline = T, showgrid = F
            ),
            autosize   = T
    )
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

