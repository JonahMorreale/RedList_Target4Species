## Red List - Target 4 Species Assessment Tool
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 09/03/2025

### setup
# packages
library(tidyverse)
library(rredlist) # for scraping Red List API
library(fuzzyjoin) # for matching lookup tables via regex

# set working directory for top level folder - containing all scripts and subfolder for outputs
myDir <- "REPLACE PATH TO YOUR WORKING FOLDER HERE"
setwd(myDir)
# import helper functions from script in wd
source("RedList_Target4Species_helpers.R")

# Red List api key
rlapiKey <- "REPLACE REDLIST API KEY HERE"

# lookup tables for weights
table3DeclineLookup <- read.csv("RedList_Target4Species_Table3Lookup.csv")
table4RestrictionLookup <- read.csv("RedList_Target4Species_Table4Lookup.csv")

##------------------------ country of interest
selectedCountry <- "Fiji"


### run the assessment tool
selectedCountry %>%
  getCountryCode() %>%
  generatePrioritySpeciesList() %>%
  select(taxon_scientific_name, assessment_id, red_list_category_code, criteria,
         Risk, Endemic, Decline, Restriction,
         Priority1_PS, Priority1_Rank, Priority2_PS, Priority2_Rank) %>%
  assign(x = paste0("Target4SpeciesList_", selectedCountry),
         value = .,
         envir = .GlobalEnv)

## write it out to csv
get(paste0("Target4SpeciesList_", selectedCountry)) %>%
  write.csv(file = paste0("CountryAssessmentTables/Target4SpeciesList_",
                          selectedCountry, ".csv"))
