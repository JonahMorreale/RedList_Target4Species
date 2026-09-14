## Red List - Rescore EW for Resubmission
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 09/14/2026


### setup
# packages
library(tidyverse)
library(writexl) # for writing out to excel file

#------------------------ set working directory for top level folder containing all
#                               scripts and subfolder for outputs
myDir <- "REPLACE PATH TO YOUR WORKING FOLDER HERE"
setwd(myDir)


###
fileName <- 'CountryAssessmentTables/Target4SpeciesList_Fiji_26.02.12'

##
  # read in the file
paste0(fileName, '.csv') %>%
  read.csv() %>%
  select(-X) %>%
  # modify EW species' scores
  mutate(Priority1_PS = case_when(red_list_category_code == "EW" ~ NA,
                                  TRUE ~ Priority1_PS)) %>%
  mutate(Priority2_PS = case_when(red_list_category_code == "EW" ~ NA,
                                  TRUE ~ Priority2_PS)) %>%
  # make it a list for next input
  list() %>%
  # rescore the P1 and P2 scores now that EW is gone
  combineAndRerank() %>%
  # assign to var
  assign(x = paste0(fileName, '_EWresub'),
         value = .,
         envir = .GlobalEnv)

## write it out to csv
get(paste0(fileName, '_EWresub')) %>%
  write.csv(file = paste0(fileName, '_EWresub', ".csv"),
            row.names = FALSE)

## write it out to Excel file (to avoid auto-formatting problems)
get(paste0(fileName, '_EWresub')) %>%
  write_xlsx(path = paste0(fileName, '_EWresub', ".xlsx"))
