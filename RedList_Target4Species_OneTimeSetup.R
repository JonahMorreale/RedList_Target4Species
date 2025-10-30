## Red List - Target 4 Species One Time Setup Utility
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 10/30/2025

### The four packages below are necessary for the RedList_Target4Species.R code to run. These
###     packages only need to be installed through R ONCE and then should run properly
###     there onward (barring major updates). If you have not already, run the code below 
###     to install the necessary packages.
install.packages(c("tidyverse", # basic utility functions for piping and manipulating data frames
                   "rredlist", # for scraping Red List API
                   "fuzzyjoin", # for matching lookup tables via regex
                   "writexl")) # for writing out to excel file
