# Two sample t-test Monte Carlo simulator

This little app offers the possibility to perform the two sample 
t-test (unpaired) Monte Carlo simulation by an easy graphical interface.

## Description

<p float="left">
   <img src="images/Overview_Monte_Carlo_simulator_1.png" width= "400" >
   <img src="images/Overview_Monte_Carlo_simulator_2.png" width= "400" >
</p>

This app is used to perform the Monte Carlo simulation for the two sample 
t-test by an easy graphical interface. For each group you can change the 
parameter standard deviation, expected value, sample size and the following 
selectable distributions types:

* Normal
* Uniform
* Exponential
* Gamma
* Logistic
* Biplane (not available thus far)

Each distribution can be displayed for each group to get a little overview 
for the distribution type. You can change the well-known parameter of the 
t-test and of course the simulation parameter like simulation steps and seed
as well. The simulation result will be displayed by a simple histogram. If 
you want to know more about the Monte Carlo simulation,
[see here](https://tjmurphy.github.io/jabstb/ttestmc.html).
It's also possible to save your results. You just need to klick on the on the 
menu on the upper right corner (three white lines). You will be able to save the 
followings:

* Saving the simulation data (TSV)
* Saving the plot (PNG) (only the histogram!)
* Saving the parameter you used for each group and simulation (TSV)

## Getting Started

### Dependencies

It should be run on Windows 10 as well as on Linux (only tested on Manjaro). You need at least a R enviroment.
Except the operating system I used the following programs and additional packages:

* Programs
   - [RStudio (2022.07.2 Build 576)](https://posit.co/download/rstudio-desktop/)
   - [R (Version 3.3.0+)](https://posit.co/download/rstudio-desktop/)

* Packages
   - [matrixTests (Version 0.1.9.1)](https://cran.r-project.org/web/packages/matrixTests/index.html)
   - [shiny (Version 1.7.3)](https://cran.r-project.org/web/packages/shiny/index.html)
   - [shinydashboard (Version 0.7.2)](https://cran.r-project.org/web/packages/shinydashboard/index.html)
   - [shinyfeedback (Version 0.4.0)](https://cran.rstudio.com/web/packages/shinyFeedback/index.html)
   - [shinyjs (Version 2.1.0)](https://cran.r-project.org/web/packages/shinyjs/index.html)
   - [shinyWidgets (Version 0.7.5)](https://cran.r-project.org/web/packages/shinyWidgets/index.html)

### Installing

Just download the R file and start it in a shiny R enviroment. More is not needed expcept the programs and 
packages I given above.

### Executing program

* How to run the program
* Step-by-step bullets
```
code blocks for commands
```

## Help

Any advise for common problems or issues.
```
command to run if program contains helper info
```

## Authors

Contributors names and contact info

ex. Dominique Pizzie  
ex. [@DomPizzie](https://twitter.com/dompizzie)

## Version History

* 0.1
    * Initial Release

## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details

## Acknowledgments

