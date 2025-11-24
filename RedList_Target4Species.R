## Red List - Target 4 Species Country Assessment Tool
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 11/25/2025

### setup
# packages
library(tidyverse)
library(rredlist) # for scraping Red List API
library(fuzzyjoin) # for matching lookup tables via regex
library(writexl) # for writing out to excel file

#------------------------ set working directory for top level folder containing all
#                               scripts and subfolder for outputs
myDir <- "REPLACE PATH TO YOUR WORKING FOLDER HERE"
setwd(myDir)

#------------------------ Red List api key
rlapiKey <- "REPLACE REDLIST API KEY HERE"

# import helper functions from script in wd
source("RedList_Target4Species_helpers.R")

# lookup tables for weights and corrections
table3DeclineLookup <- read.csv("RedList_Target4Species_Table3Lookup.csv")
table4RestrictionLookup <- read.csv("RedList_Target4Species_Table4Lookup.csv")
tableVersionUpdateLookup <- read.csv("RedList_Target4Species_VersionUpdateLookup.csv")

##------------------------ countries of interest - include inside parentheses with c in front
##                              and with quote marks around each country.
##                              example: selectedCountryList <- c("Viet Nam", "Fiji", "Sweden")
##                              NOTE: the spelling of the country MUST match that used by the
##                              Red List API - see 'RL Country List.xlsx" for reference
selectedCountryList <- c("South Africa")


### run the assessment tool for each country in list
for (selectedCountry in selectedCountryList) {
  print(paste0("Begining assessment for ", selectedCountry))
  selectedCountry %>%
    getCountryCode() %>%
    generatePrioritySpeciesList_byCountry() %>%
    select(CommonName_English = commonName_PreferredLanguage,
           ScientificName = taxon_scientific_name, Phylum = taxon_phylum_name,
           Class = taxon_class_name, Order = taxon_order_name, Family = taxon_family_name,
           # red list criteria and metadata
           assessment_id, assessment_date, red_list_category_code,
           red_list_category_version, criteria_Listed = criteria, criteria_V3.1,
           # location filters
           focalCountryPresence, focalCountryOrigin,
           # Table 2 - Endemism
           NumberOfCountriesExtant,
           # Table 3 - Decline
           PopulationTrend = population_trend_description_en,
           ContDec_Population = supplementary_info_population_continuing_decline,
           ContDec_Area = supplementary_info_continuing_decline_in_area,
           ContDec_Location = supplementary_info_continuing_decline_in_number_of_locations,
           ContDec_EOO = supplementary_info_continuing_decline_in_extent_of_occurence,
           ContDec_AOO = supplementary_info_continuing_decline_in_area_of_occupancy,
           ContDec_Subpopulation = supplementary_info_continuing_decline_in_subpopulations,
           # Table 4 - Restriction
           AreaRestricted = supplementary_info_area_restricted_is_restricted,
           AOO = supplementary_info_estimated_area_of_occupancy, 
           EOO = supplementary_info_estimated_extent_of_occurence,
           PopulationSize = supplementary_info_population_size,
           LocationNumber = supplementary_info_number_of_locations,
           # Calculated Values for PS
           Risk, Endemic, Decline, Restriction,
           Priority1_PS, Priority1_Rank, Priority2_PS, Priority2_Rank) %>%
    assign(x = paste0("Target4SpeciesList_", selectedCountry),
           value = .,
           envir = .GlobalEnv)
  
  ## write it out to csv
  get(paste0("Target4SpeciesList_", selectedCountry)) %>%
    write.csv(file = paste0("CountryAssessmentTables/Target4SpeciesList_",
                            selectedCountry, ".csv"))
  
  ## write it out to Excel file (to avoid auto-formatting problems)
  get(paste0("Target4SpeciesList_", selectedCountry)) %>%
    write_xlsx(path = paste0("CountryAssessmentTables/Target4SpeciesList_",
                              selectedCountry, ".xlsx"))
}
