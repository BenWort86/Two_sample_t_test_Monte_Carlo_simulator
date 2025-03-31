
source('ui_modules.R')

ui_elements <- list(
  
  'dist' = navset_card_tab(
    title = "Distribution Plot",
    nav_panel(title = "Group A", plot_dist_ui("dist_group_A")),
    nav_panel(title = "Group B", plot_dist_ui("dist_group_B"))
      
  ), 
  
  'hist' = card(
    card_header("Histogram", style = "height: 50px;"),
    card_body(plotlyOutput("hist"))
  ), 
  
  'hits' = card(
    card_header('Hits'),
    card_body(textOutput("hits"))
  ), 
  
  'param_group_A' = card(
    card_header("Parameter of Group A"),
    card_body(group_parameter_ui("parameter_group_A"))
  ), 
  
  'param_group_B' = card(
    card_header("Parameter of Group B"),
    card_body(group_parameter_ui("parameter_group_B"))
  ),
  
  'test_sim_param' = card(
    card_header("Test and Simulation Parameter"),
    card_body(test_sim_parameter_ui("test_sim_parameter"))
  )
  
)