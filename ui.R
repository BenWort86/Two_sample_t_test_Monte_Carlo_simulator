
source('ui_elements.R')

ui <- page_sidebar(
  
  useShinyFeedback(),
  
  title   = "Monte Carlo Simulation for the Unpaired Two-Sample t-Test",
  
  theme = bs_theme(bootswatch  = 'cosmo', 
                   primary     = '#3c7cbf', 
                   'navbar-bg' = '#3c7cbf'),
 
  sidebar = sidebar(
    
    title = 'Menu',
    
    downloadButton(
      "save_data",
      label = 'Save Data',
      style = 'background-color: #888a8b; border-color: #888a8b'
    ),
    
    downloadButton(
      "save_parameter",
      label = "Save Parameter",
      style = 'background-color: #888a8b; border-color: #888a8b'
    )
  ),

  layout_column_wrap(
    width = 1 / 2,
    ui_elements[['dist']],
    layout_column_wrap(
      width = 1,
      heights_equal = 'row',
      ui_elements[['hist']],
      ui_elements[['hits']]
    )
  ),
  
  layout_column_wrap(
    ui_elements[['param_group_A']], 
    ui_elements[['param_group_B']], 
    ui_elements[['test_sim_param']]
  )
)
