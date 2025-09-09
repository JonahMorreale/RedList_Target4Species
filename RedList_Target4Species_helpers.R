## Red List - Target 4 Species Helper Functions
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 09/03/2025


### packages
library(tidyverse)
library(rredlist) # for scraping Red List API
library(fuzzyjoin) # for matching lookup tables via regex


### set wait time here for API speed - the RL API recommends .5 seconds, and threatens
##      limiting your access key if you exceed this too much
rlapiWaitTime = .05 # set time here in seconds


### functions -----------------------------------------------------------------
## function: fetch the two letter RL country code for a given country
getCountryCode <- function(countryName) {
  # fetch all countries
  rl_countries(key = rlapiKey)$countries %>%
    # return is formatted with named list as column
    unnest_wider(description) %>%
    # filter to selected country
    filter(en == countryName) %>%
    # get the code and return it
    pull(code) %>%
    return()
}


## function: for given RL Category and Criteria strings, parse the subcriteria into all unique
##      subcriteria combinations and output as a data table
rlCriteriaParser <- function(rlAssessmentID, rlCategory, rlFullCriteria) {
  # check for NA
  if (is.na(rlFullCriteria)) {
    print(paste0("IMPROPER RL CRITERIA (",
                 rlFullCriteria,
                 ") - PARSER RETURNING NA FOR ASSESSMENT ID: ",
                 rlAssessmentID))
    return(NA)
  }
  # start with empty list to hold rows of df
  masterRowsList <- list()
  ## split criteria when multiple are met (separated by a ; character)
  multiCritList <- str_split(substring(rlFullCriteria, 1, 100), pattern = "[;]")[[1]]
  #  loop through
  for (eachL0 in multiCritList) {
    # strip off spaces
    eachL0 <- str_trim(eachL0)
    # first level criteria
    firstCrit <- substring(eachL0, 1, 1)
    # if there is ONLY a main criteria (capital letter) add the row here
    if (nchar(substring(eachL0, 2, 100)) < 1) {
      # create the row for it
      masterRowsList[[length(masterRowsList) + 1]] <- list(RL_Criteria = firstCrit,
                                                           RL_SubcriteriaL1 = ".*",
                                                           RL_SubcriteriaL2 = ".*",
                                                           RL_SubcriteriaL3 = ".*")
    } else {
      # split subcriteria when multiple are met (separated by + character)
      multiSubCritList <- strsplit(substring(eachL0, 2, 100), split = "\\+")[[1]]
      # loop through
      for (eachL1 in multiSubCritList) {
        # first subcriteria (number)
        subcritL1 <- substring(eachL1, 1, 1)
        # split subcriteria where multiple are met (each has own parenthetical subsubcriteria)
        multiSubCritList <- str_split(substr(eachL1, 2, 100),
                                      pattern = "(?<=\\))",
                                      simplify = FALSE)[[1]]
        # if there is only a main and subcriteria (capital letter plus number) add row
        if (nchar(substr(eachL1, 2, 100)) == 0) {
          # create the row for it
          masterRowsList[[length(masterRowsList) + 1]] <- list(RL_Criteria = firstCrit,
                                                               RL_SubcriteriaL1 = subcritL1,
                                                               RL_SubcriteriaL2 = ".*",
                                                               RL_SubcriteriaL3 = ".*")
        } else {
          # loop through
          for (eachL2 in multiSubCritList) {
            ## get standalone subcriteria (lowercase with no roman numerals)
            # case 1 - two lowercase in a row, take the first
            noRome_1 <- substr(regmatches(eachL2, gregexpr("[a-e]{2}", eachL2))[[1]], 1, 1)
            # case 2 - three lowercase in a row, take the second (the first is grabbed above)
            noRome_2 <- substr(regmatches(eachL2, gregexpr("[a-e]{3}", eachL2))[[1]], 2, 2)
            # case 3 - one lowercase followed by end of string
            noRome_3 <- regmatches(eachL2, gregexpr("[a-e]$", eachL2))
            # put them together
            noRomeTot <- c(noRome_1, noRome_2, noRome_3)
            for (eachL2_minus in noRomeTot) {
              # if there were any matches to the above regexps
              if (length(eachL2_minus) > 0) {
                # create the row for it
                masterRowsList[[length(masterRowsList) + 1]] <- list(RL_Criteria = firstCrit,
                                                                     RL_SubcriteriaL1 = subcritL1,
                                                                     RL_SubcriteriaL2 = eachL2_minus,
                                                                     RL_SubcriteriaL3 = ".*")
              }
            }
            ## get the L3 subcriteria (roman numerals) where present
            withRome <- regmatches(eachL2, gregexpr("[a-e]\\(([^)]+)\\)", eachL2))[[1]]
            # loop through
            for (eachL2_plus in withRome) {
              # get the L2 (lowercase letter)
              subcritL2 <- substr(eachL2_plus, 1, 1)
              # loop through all L3 (roman numerals) - matching b/w the parentheses
              multiRomanList <- str_match(eachL2_plus, "\\(([^)]+)\\)")[[2]] %>%
                strsplit(",")
              for(eachL3 in multiRomanList) {
                # create the row for it
                masterRowsList[[length(masterRowsList) + 1]] <- list(RL_Criteria = firstCrit,
                                                                     RL_SubcriteriaL1 = subcritL1,
                                                                     RL_SubcriteriaL2 = subcritL2,
                                                                     RL_SubcriteriaL3 = eachL3)
              }
            }
          }
        }
      }
    }
  }
  ## convert rows to df and return
  # catch NA and NULLs (if the criteria string was formatted badly)
  if (!exists("masterRowsList") || is.null(masterRowsList) || length(masterRowsList) == 0) {return(NA)}
  else {
    do.call(rbind, lapply(masterRowsList, as.data.frame)) %>%
      mutate(RL_Category = rlCategory)
  }
}


## function: for a given RL category and criteria string, output the highest matching
##      Decline weight per Table 3
declineWeightHelper <- function(rlAssessmentID, rlCategory, rlFullCriteria,
                                rlPopTrend, rowNumber, detailed = FALSE) {
  # run the parser to get a table of all criteria met
  criteriaDF <- rlCriteriaParser(rlAssessmentID, rlCategory, rlFullCriteria)
  # if parser comes back with NA, the criteria is poorly formatted - end
  if (!is.data.frame(criteriaDF)) {
    print(paste0("IMPROPER RL CRITERIA (",
                 rlFullCriteria,
                 ") - DECLINE WEIGHT SET TO NA FOR ASSESSMENT ID: ",
                 rlAssessmentID))
    return(NA)
  }
  # use regular expressions to join the criteria table  to the weight lookup table
  #     - allows for wild card matching when all subcriteria of a given criteria are weighted
  #     the same
  weightTable <- regex_left_join(x = criteriaDF, y = table3DeclineLookup,
                                 by = c("RL_Criteria", "RL_SubcriteriaL1", "RL_SubcriteriaL2",
                                        "RL_SubcriteriaL3", "RL_Category"))
  # get max weight
  weightTable %>%
    pull(Decline) %>%
    # will be -Inf if no matches - include an "otherwise 0" here
    max(0, na.rm = TRUE) -> greatestWeight
  ## additional criteria - population trend
  addCrit <- c()
  if (!is.na(rlPopTrend)) { # catch NAs and skip
    if (rlPopTrend == "1") {addCrit[length(addCrit) + 1] <- 2}  # code: 1 = "Decreasing"
    if (rlPopTrend == "3") {addCrit[length(addCrit) + 1] <- .5} # code: 3 = "Unknown"
  }
  greatestWeight <- max(addCrit, greatestWeight)
  ## progress bar tracker
  if ((rowNumber %% 50) == 0) {print(paste("Decline Progress:", rowNumber, "done"))}
  ## return
  if (detailed) {print(weightTable)}
  return(greatestWeight)
}


## function: for a given RL category and criteria string, output the highest matching
##      Restriction weight per Table 4
restrictionWeightHelper <- function(rlAssessmentID,
                                    rlCategory, rlFullCriteria, 
                                    rlPopSize, rlAreaRestricted,
                                    rlAOO, rlEOO,
                                    rlLocNumber,
                                    rowNumber, detailed = FALSE) {
  # run the parser to get a table of all criteria met
  criteriaDF <- rlCriteriaParser(rlAssessmentID, rlCategory, rlFullCriteria)
  # if parser comes back with non-DF, the criteria is poorly formatted - end
  if (!is.data.frame(criteriaDF)) {
    print(paste0("IMPROPER RL CRITERIA (",
                 rlFullCriteria,
                 ") - RESTRICTION WEIGHT SET TO NA FOR ASSESSMENT ID: ",
                 rlAssessmentID))
    return(NA)
  }
  # use regular expressions to join the criteria table  to the weight lookup table
  #     - allows for wild card matching when all subcriteria of a given criteria are weighted
  #     the same
  weightTable <- regex_left_join(x = criteriaDF, y = table4RestrictionLookup,
                                 by = c("RL_Criteria", "RL_SubcriteriaL1", "RL_SubcriteriaL2",
                                        "RL_SubcriteriaL3", "RL_Category"))
  # get max weight
  weightTable %>%
    pull(Restriction) %>%
    # will be -Inf if no matches - include an "otherwise 0" here
    max(0, na.rm = TRUE) -> greatestWeight
  ## additional criteria
  addCrit <- c()
  # population size - the minimum listed
  rlPopSize <- min(as.numeric(str_split(rlPopSize,
                                        pattern = "[ ,\\-;]",
                                        simplify = FALSE)[[1]]),
                   na.rm = TRUE)
  if (!is.infinite(rlPopSize) & !is.na(rlPopSize)) {
    if (rlPopSize < 50) {addCrit[length(addCrit) + 1] <- 10}
    if (rlPopSize < 100) {addCrit[length(addCrit) + 1] <- 9}
    if (rlPopSize < 250) {addCrit[length(addCrit) + 1] <- 8}
    if (rlPopSize < 1000) {addCrit[length(addCrit) + 1] <- 6}
    if (rlPopSize < 2500) {addCrit[length(addCrit) + 1] <- 5}
    if (rlPopSize < 5000) {addCrit[length(addCrit) + 1] <- 4}
    if (rlPopSize < 10000) {addCrit[length(addCrit) + 1] <- 2}
  }
  # location number
  if (rlLocNumber == 1) {addCrit[length(addCrit) + 1] <- 6}
  if (rlLocNumber == 2) {addCrit[length(addCrit) + 1] <- 5}
  if (rlLocNumber <= 5) {addCrit[length(addCrit) + 1] <- 4}
  if (rlLocNumber <= 10) {addCrit[length(addCrit) + 1] <- 2}
  # area of occupancy - the minimum listed
  rlAOO <- min(as.numeric(str_split(rlAOO,
                                    pattern = "[ ,\\-;]+",
                                    simplify = FALSE)[[1]]),
               na.rm = TRUE)
  if (!is.infinite(rlAOO) & !is.na(rlAOO)) {
    if (rlAOO < 10) {addCrit[length(addCrit) + 1] <- 9}
    if (rlAOO < 500) {addCrit[length(addCrit) + 1] <- 3}
    if (rlAOO < 2000) {addCrit[length(addCrit) + 1] <- 1}
  }
  # extent of occurence - the minimum listed
  rlEOO <- min(as.numeric(str_split(rlEOO,
                                    pattern = "[ ,\\-;]+",
                                    simplify = FALSE)[[1]]),
               na.rm = TRUE)
  if (!is.infinite(rlEOO) & !is.na(rlEOO)) {
    if (rlEOO < 100) {addCrit[length(addCrit) + 1] <- 9}
    if (rlEOO < 5000) {addCrit[length(addCrit) + 1] <- 3}
    if (rlEOO < 20000) {addCrit[length(addCrit) + 1] <- 1}
  }
  # area restricted #$#$
  #$#$
  # max of these additional criteria
  greatestWeight <- max(addCrit, greatestWeight)
  ## progress bar tracker
  if ((rowNumber %% 50) == 0) {print(paste("Restriction Progress:", rowNumber, "done"))}
  ## return
  if (detailed) {print(addCrit)}
  return(greatestWeight)
}


## function: for a given two letter RL country code, output a table including Priority 1 and
##      Priority 2 scores and ranks
generatePrioritySpeciesList <- function(countryCode) {
  ### scrape redlist for species for given country
  ##      if not in memory already
  assessmentName <- paste0("countryAssessment_", countryCode)
  if (!exists(assessmentName)) {
    assign(x = assessmentName,
           value = rl_countries(code = countryCode,
                                latest = TRUE, # only the latest assessment for each taxon
                                key = rlapiKey)$assessments,
           envir = .GlobalEnv)
  }
  ## use that list to generate Target 4 priority species according to criteria in doc
  ##      if not in memory already
  speciesListName <- paste0("speciesList_", countryCode)
  if (!exists(speciesListName)) {
    assign( x = speciesListName,
            envir = .GlobalEnv,
            value = get(assessmentName) %>%
        # filter to "Known Threatened Species" (CR, EN, VU, EW)
        filter(red_list_category_code %in% c("EW", "CR", "EN", "VU")) %>%
        #$ head(30) %>% #$ recommend uncommenting for testing purposes
        ## scrape Red List assessment data for each target species
        pull(assessment_id) %>%
        rl_assessment_list(key = rlapiKey, wait_time = rlapiWaitTime) %>%
        # convert to df from nested lists
        do.call(rbind, .)  %>% 
        as.data.frame() %>%
        # drop a few unnecessary columns
        select(-c(starts_with("documentation"),
                  starts_with("faos"),
                  starts_with("habitats"),
                  starts_with("use_and_trade"),
                  starts_with("threats"),
                  starts_with("references"))) %>%
        # unnest first 12 columns
        unnest(col = 1:12) %>%
        # expand out columns of interest
        unnest_wider(col = 13:26, names_sep = "_")
    ) # end of assignment
  }
    
    ### now use that species list to apply Target 4 scoring criteria
  get(speciesListName) %>%
    ## add a row number column for progress tracking
    mutate(rowNumber = row_number()) %>%
    ## Risk column based on rl code -- weights from Table 1
    mutate(Risk = case_when(red_list_category_code == "EW" ~ 8,
                            red_list_category_code == "CR" ~ 4,
                            red_list_category_code == "EN" ~ 2,
                            red_list_category_code == "VU" ~ 1,
                            TRUE ~ 0)) %>%
    ## Endemic column:
    # Table 2, condition 2 -- by number of countries occupied
    rowwise() %>%
    mutate(NumLocationsExtant = length(locations_code)) %>%
    mutate(Endemic = 10 / NumLocationsExtant) %>%
    # ...and Table 2, condition 3 -- if Extinct in the Wild
    mutate(Endemic = case_when(red_list_category_code == "EW" ~ 1,
                               TRUE ~ Endemic)) %>%
    ## Decline column:
    mutate(Decline = declineWeightHelper(rlAssessmentID = assessment_id,
                                         rlCategory = red_list_category_code,
                                         rlFullCriteria = criteria,
                                         rlPopTrend = population_trend_code,
                                         rowNumber = rowNumber)) %>%
    ## Restriction column:
    mutate(Restriction =
             restrictionWeightHelper(rlAssessmentID = assessment_id,
                                     rlCategory = red_list_category_code,
                                     rlFullCriteria = criteria,
                                     rlPopSize = supplementary_info_population_size,
                                     rlAreaRestricted = NA, #$
                                     rlAOO = supplementary_info_estimated_area_of_occupancy,
                                     rlEOO = supplementary_info_estimated_extent_of_occurence,
                                     rlLocNumber = NumLocationsExtant,
                                     rowNumber = rowNumber)) %>%
    ## Priority Scores - Equations 1 and 2
    mutate(Priority1_PS = Risk * Endemic * Decline * Restriction,
           Priority2_PS = Risk * Endemic * max(Decline, Restriction)) %>%
    ## convert to rank
    # priority 1 rank
    arrange(desc(Priority1_PS)) %>% 
    group_by() %>% # needs this for if_else to operate rowwise
    mutate(Priority1_Rank = if_else(Priority1_PS > 0, row_number(), NA)) %>% 
    ungroup() %>%
    # priority 2 rank
    arrange(desc(Priority2_PS)) %>%
    group_by(Priority1_PS) %>%
    mutate(Priority2_Rank = if_else(Priority1_PS == 0, row_number(), NA)) %>% 
    ungroup() %>%
    # arrange by P1 and P2
    arrange(Priority1_Rank, Priority2_Rank) ->
    # return the final table jm
    priorityTable
  return(priorityTable)
}