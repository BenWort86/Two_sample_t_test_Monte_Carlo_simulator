# Two sample t-test Monte Carlo simulator

This small app offers you the possibility to run the Monte Carlo 
simulation with two sample t-tests (unpaired) via a simple graphical interface.

## Description

<p float="left">
   <img src="images/Overview_Monte_Carlo_simulator_1.png" width= "400" >
   <img src="images/Overview_Monte_Carlo_simulator_2.png" width= "400" >
</p>

This app allows you to run the Monte Carlo simulation for the two sample 
t-test using a simple graphical interface. For each group you can change 
the parameters standard deviation, expected value, sample size and the following 
selectable distribution types:

* Normal
* Uniform
* Exponential
* Gamma
* Logistic
* Biplane (not available thus far)

Each distribution can be displayed for each group to get a small overview 
of the distribution type. You can change the known parameters of the
t-test and of course the simulation parameters like simulation steps and seed.
The simulation result is displayed as a simple histogram. If
you want to learn more about Monte Carlo simulation, 
[see here](https://tjmurphy.github.io/jabstb/ttestmc.html).
It is also possible to save your results. Just click on the 
menu in the upper right corner (three white lines). You can then save 
the following:

* Saving the simulation data (TSV)
* Saving the plot (PNG) (only the histogram!)
* Saving the parameter you used for each group and simulation (TSV)

## Getting Started

### Dependencies

It should run on both Windows 10 and Linux (only tested on Manjaro). You will need at least an R environment.                               
Besides the operating system I used the following programs and additional packages:                                                     

* Programs
   - [RStudio (2022.07.2 Build 576)](https://posit.co/download/rstudio-desktop/)
   - [R (Version 3.3.0+)](https://posit.co/download/rstudio-desktop/)

* Packages
   - [matrixTests (0.1.9.1)](https://cran.r-project.org/web/packages/matrixTests/index.html)
   - [shiny (1.7.3)](https://cran.r-project.org/web/packages/shiny/index.html)
   - [shinydashboard (0.7.2)](https://cran.r-project.org/web/packages/shinydashboard/index.html)
   - [shinyfeedback (0.4.0)](https://cran.rstudio.com/web/packages/shinyFeedback/index.html)
   - [shinyjs (2.1.0)](https://cran.r-project.org/web/packages/shinyjs/index.html)
   - [shinyWidgets (0.7.5)](https://cran.r-project.org/web/packages/shinyWidgets/index.html)

### Installing & Executing 

Just download the R file and run it in a shiny R environment. Nothing more is needed except the programs and 
packages I have given above.

## Help

The function ```group_parameter_server``` interrupts the simulation when you alter the distribution type. 
If you change the distribution type, you can stop the simulation without stopping. 
However, you have to make sure that all parameters are within acceptable limits before you run the simulation. 
When the distribution type is changed, the simulation will be interrupted and the parameter input controls will 
also be updated with the given values in order to avoid any kind of incorrect calculations. It takes some time 
to update. I used the ```debounce``` function to solve issue and picked 150ms (tested with an AMD Ryzen 7 3700X). 
Although it's not an ideal answer but I haven't found a better one yet. If the simulator produces NaN'S after 
changing the distribution type you can change the interrupt time in the code directly :

```
group_parameter_server <- function(input, output, session) {
  
  ...
  
  # Interrupt the simulation 
  updateTrigger <- debounce(reactive({
    req(input$distributiontype) 
  }), 150)
 
  ...
 
  return(update_in_progress)
  
}

```
However, I am considering a solution with a simple start and stop button to avoid this kind of issues.

## License

This project is licensed under the MIT License - see the LICENSE.md file for details
