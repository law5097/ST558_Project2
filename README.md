# ST 558 Project 2 - Lee Worthington

## Description
This Shiny app allows users to query the U.S. Government Revenue Collections API, define filters and columns, download data as a CSV, and generate summary plots/statistics based on the selected data.

## Packages Needed
The following packages are required to run the app:
- shiny
- tidyverse
- dplyr
- tidyr
- ggplot2
- scales
- httr
- jsonlite
- DT
- purrr

## Installation Code
To install all the necessary packages, run the following line of code in R:
```R
install.packages(c("shiny", "tidyverse", "dplyr", "tidyr", "ggplot2", "scales", "httr", "jsonlite",  "DT", "purrr"))
```

## Code to run the app in RStudio
```R
shiny::runGitHub("law5097/ST558_Project2", subdir = "ST558_Project2")
```
