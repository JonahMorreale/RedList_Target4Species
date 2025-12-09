## Red List - Target 4 Species Helper Functions
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## updated: 12/8/2025


### packages
library(tidyverse)
library(rredlist) # for scraping Red List API
library(fuzzyjoin) # for matching lookup tables via regex


### set wait time here for API speed - the RL API recommends .5 seconds, and threatens to
##      limit your access key if you exceed this too much
rlapiWaitTime = .05 # set time here in seconds


### list of 'country' codes used by Red List
countryCodeList <- rl_countries(key = rlapiKey)$countries$code


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


## function: for a given RL numeric range (from AOO, EOO, or PopulationSize), select the appropriate value
##      based on format
rlRangeReader <- function(rlString) {
  # if the value is NA, just return NA
  if (is.na(rlString)) {return(NA)}
  # check for comma - after commas is the "best estimate value"
  if (grepl(",", rlString)) {
    str_split(rlString, pattern = ",", simplify = FALSE)[[1]] %>%
      str_trim() %>%
      last() %>%
      as.numeric() %>%
      return()
  # else check for dash - take mean of this range
  } else if (grepl("-", rlString)) {
    str_split(rlString, pattern = "-", simplify = FALSE)[[1]] %>%
      str_trim() %>%
      as.numeric() %>%
      mean() %>%
      return()
  # check if can be coerced to a single number
  } else if (!is.na(suppressWarnings(as.numeric(rlString)))) {
    rlString %>%
      as.numeric() %>%
      return()
  # otherwise return NA
  } else {return(NA)}
}


## function: for given RL Criteria and Version, update to the version 3.1 format if necessary
rlVersionUpdater <- function(rlAssessmentID, rlCategory, rlFullCriteria, rlVersion, detailed = FALSE) {
  if (is.na(rlVersion) | !is.character(rlVersion)) {return(NA_character_)}
  if (rlVersion == "3.1") {
    return(rlFullCriteria)
  } else if (rlVersion == "2.3") {
    rlFullCriteria %>%
      # replace commas with semicolons
      str_replace_all(pattern = ",", replacement = ";") %>%
      # replace A1 with A2 and A2 with A3 KEEPING subcriteria
      str_replace_all(pattern = "A2", replacement = "A3") %>% # latter first so it doesn't chain
      str_replace_all(pattern = "A1", replacement = "A2") %>%
      # parse the string into df
      rlCriteriaParser(rlAssessmentID = rlAssessmentID, 
                       rlCategory = rlCategory,
                       rlFullCriteria = .) -> parsedCritDF
    # check if that it is actually a data frame (and not for instance NA)
    if (!is.data.frame(parsedCritDF)) {return(NA)}
    # else update the string to new criteria
    parsedCritDF %>%
      select(-RL_Category) %>%
      # fuzzy join to our update table to get the new code for the rest
      regex_left_join(y = tableVersionUpdateLookup,
                      by = c("RL_Criteria", "RL_SubcriteriaL1",
                             "RL_SubcriteriaL2", "RL_SubcriteriaL3")) %>%
      # if new code is blank (not matching a replacement case) use old code
      mutate(NewCode = case_when(is.na(NewCode) ~ paste0(RL_Criteria.x,
                                                         RL_SubcriteriaL1.x,
                                                         RL_SubcriteriaL2.x,
                                                         RL_SubcriteriaL3.x) %>%
                                                  str_replace_all(pattern = "\\.\\*",
                                                                  replacement = ""),
                                 TRUE ~ NewCode)) %>%
      # parse the new codes out again
      pull(NewCode) %>%
      map_df(rlCriteriaParser, rlAssessmentID = -1, rlCategory = "X") %>%
      # and deparse it back together into one
      rlCriteriaDeparser() -> critString
      # report and return this code
      if (detailed) {
        print(paste("Outdated Assessment ID:",
                    rlAssessmentID,
                    "Updated to V3.1"))
      }
      return(critString)
  } else {
    return(NA_character_)
  }
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


## function: for a given parsed criteria string data frame (ie the return of rlCriteriaParser)
##      output a de-parsed character string of criteria in RL format
rlCriteriaDeparser <- function(rlParsedCriteriaDF) {
  # check for improper format (includes NA)
  if (!is.data.frame(rlParsedCriteriaDF)) {
    return(NA)
  }
  rlParsedCriteriaDF %>%
    # ensure they are in hierarchical order
    arrange(across(everything())) %>%
    # match up the L3 subcriteria (parenthetical roman numerals)
    group_by(RL_Criteria, RL_SubcriteriaL1, RL_SubcriteriaL2) %>%
    reframe(RL_SubcriteriaL3 = str_c(RL_SubcriteriaL3, collapse = ",")) %>%
    # remove blanks (.*) and fold L3 into L2 as parenthetical
    mutate(RL_SubcriteriaL3 = str_replace_all(RL_SubcriteriaL3, "\\.\\*", "")) %>%
    mutate(RL_SubcriteriaL2 = paste0(RL_SubcriteriaL2, "(", RL_SubcriteriaL3, ")")) %>%
    select(-RL_SubcriteriaL3) %>%
    # in L2, remove blanks and empty parentheses
    mutate(RL_SubcriteriaL2 = str_replace_all(RL_SubcriteriaL2, "\\.\\*", "")) %>%
    mutate(RL_SubcriteriaL2 = str_replace_all(RL_SubcriteriaL2, "\\(\\)", "")) %>%
    # match up L2 subcriteria (lowercase letters) and combine
    group_by(RL_Criteria, RL_SubcriteriaL1) %>%
    reframe(RL_SubcriteriaL2 = str_c(RL_SubcriteriaL2, collapse = "")) %>%
    # fold L2 into L1
    mutate(RL_SubcriteriaL1 = paste0(RL_SubcriteriaL1, RL_SubcriteriaL2)) %>%
    select(-RL_SubcriteriaL2) %>%
    # in L1, remove blanks and empty parentheses
    mutate(RL_SubcriteriaL1 = str_replace_all(RL_SubcriteriaL1, "\\.\\*", "")) %>%
    # match up L1 subcriteria (numbers) and combine with "+" symbol
    group_by(RL_Criteria) %>%
    reframe(RL_SubcriteriaL1 = str_c(RL_SubcriteriaL1, collapse = "+")) %>%
    # fold L1 subcriteria into criteria and combine with ";" symbol
    mutate(RL_Criteria = paste0(RL_Criteria, RL_SubcriteriaL1)) %>%
    select(-RL_SubcriteriaL1) %>%
    reframe(RL_FullString = str_c(RL_Criteria, collapse = ";")) %>%
    # convert to char
    as.character() %>%
    # return it
    return()
}


## function: for a given RL category and criteria string, output the highest matching
##      Decline weight per Table 3
declineWeightHelper <- function(rlAssessmentID, rlCategory, rlFullCriteria,
                                rlPopTrend, rowNumber,
                                rlContDecPop, rlContDecArea, rlContDecLoc,
                                rlContDecEOO, rlContDecAOO, rlContDecSubpop,
                                detailed = FALSE) {
  # if the class is "EW" its going to be NA for criteria - just return weight of ten
  if (rlCategory == "EW") {return(10)}
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
  ## additional criteria - continuing declines (population, area, location, EOO, AOO, subpop)
  if (!is.na(rlContDecPop) & rlContDecPop == "Yes") {addCrit[length(addCrit) + 1] <- 2}
  if (!is.na(rlContDecArea) & rlContDecArea == "Yes") {addCrit[length(addCrit) + 1] <- 1}
  if (!is.na(rlContDecLoc) & rlContDecLoc == "Yes") {addCrit[length(addCrit) + 1] <- 1}
  if (!is.na(rlContDecEOO) & rlContDecEOO == "Yes") {addCrit[length(addCrit) + 1] <- 1}
  if (!is.na(rlContDecAOO) & rlContDecAOO == "Yes") {addCrit[length(addCrit) + 1] <- 1}
  if (!is.na(rlContDecSubpop) & rlContDecSubpop == "Yes") {addCrit[length(addCrit) + 1] <- 1}
  ## max of these additional criteria
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
  # if the class is "EW" its going to be NA for criteria - just return weight of ten
  if (rlCategory == "EW") {return(10)}
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
  # population size
  rlPopSize <- rlRangeReader(rlPopSize)
  if (!is.na(rlPopSize)) {
    if (rlPopSize < 50) {addCrit[length(addCrit) + 1] <- 10}
    if (rlPopSize < 100) {addCrit[length(addCrit) + 1] <- 9}
    if (rlPopSize < 250) {addCrit[length(addCrit) + 1] <- 8}
    if (rlPopSize < 500) {addCrit[length(addCrit) + 1] <- 7}
    if (rlPopSize < 1000) {addCrit[length(addCrit) + 1] <- 6}
    if (rlPopSize < 2500) {addCrit[length(addCrit) + 1] <- 5}
    if (rlPopSize < 5000) {addCrit[length(addCrit) + 1] <- 4}
    if (rlPopSize < 10000) {addCrit[length(addCrit) + 1] <- 2}
  }
  # location number
  rlLocNumber <- rlRangeReader(rlLocNumber)
  if (!is.na(rlLocNumber)) {
    if (rlLocNumber == 1) {addCrit[length(addCrit) + 1] <- 6}
    if (rlLocNumber == 2) {addCrit[length(addCrit) + 1] <- 5}
    if (rlLocNumber <= 5) {addCrit[length(addCrit) + 1] <- 4}
    if (rlLocNumber <= 10) {addCrit[length(addCrit) + 1] <- 2}
  }
  # area of occupancy - the minimum listed
  rlAOO <- rlRangeReader(rlAOO)
  if (!is.na(rlAOO)) {
    if (rlAOO < 10) {addCrit[length(addCrit) + 1] <- 9}
    if (rlAOO < 500) {addCrit[length(addCrit) + 1] <- 3}
    if (rlAOO < 2000) {addCrit[length(addCrit) + 1] <- 1}
  }
  # extent of occurence - the minimum listed
  rlEOO <- rlRangeReader(rlEOO)
  if (!is.na(rlEOO)) {
    if (rlEOO < 100) {addCrit[length(addCrit) + 1] <- 9}
    if (rlEOO < 5000) {addCrit[length(addCrit) + 1] <- 3}
    if (rlEOO < 20000) {addCrit[length(addCrit) + 1] <- 1}
  }
  # area restricted
  if (!is.na(rlAreaRestricted) & rlAreaRestricted == "Yes") {addCrit[length(addCrit) + 1] <- 4}
  # max of these additional criteria
  greatestWeight <- max(addCrit, greatestWeight)
  ## progress bar tracker
  if ((rowNumber %% 50) == 0) {print(paste("Restriction Progress:", rowNumber, "done"))}
  ## return
  if (detailed) {print(addCrit)}
  return(greatestWeight)
}


## function: from a given RL assessment list NAME, pull the full assessments for all IDs in list
###     and clean up API return for format as data table - only performs if not already in memory
assessmentListToSpeciesList <- function(assessmentName) {
  ## pull target name from assessment name
  targetName = strsplit(assessmentName, "_")[[1]][2]
  speciesListName <- paste0("speciesList_", targetName)
  ## loop throught assessment list pulling assessments, and modify
  if (!exists(speciesListName)) {
    assign( x = speciesListName,
            envir = .GlobalEnv,
            value = get(assessmentName) %>%
              # filter to "Known Threatened Species" (CR, EN, VU, EW)
              filter(red_list_category_code %in% c("EW", "CR", "EN", "VU")) %>%
              #$head(60) %>% #$ recommend uncommenting for testing purposes
              ## scrape Red List assessment data for each target species
              pull(assessment_id) %>%
              rl_assessment_list(key = rlapiKey, wait_time = rlapiWaitTime, times = 5) %>%
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
              unnest_wider(col = 13:26, names_sep = "_") %>%
              # expand out common name column and pick the 'main' common name
              unnest_wider(col = taxon_common_names, names_sep = "_") %>%
              rowwise() %>%
              mutate(commonName_PreferredLanguage = {
                mainNameIndex <- which(taxon_common_names_main == TRUE)
                if (length(mainNameIndex) > 0) {taxon_common_names_name[mainNameIndex]} else {NA}
              }) %>%
              # and fix population trend description
              unnest_wider(col = population_trend_description, names_sep = "_") %>%
              # make assessment date a simpler column
              mutate(assessment_date = as.Date(assessment_date))
    ) # end of assignment
  }
  return(get(speciesListName))
}


## function: from a given species list generate Target 4 priority species
###     according to criteria in doc
speciesListToPriorityTable <- function(speciesList) {
  speciesList %T>%
    # report out progress
    {print("Beginning Scoring"); .} %>%
    #$ head(60) %>% #$ recommend uncommenting for testing purposes
    ## keep only global assessments
    rowwise() %>%
    filter("1" %in% scopes_code) %>%
    ## remove species with multiple assessments that weren't caught by the latest = TRUE flag
    ##      keeping only the latest assessment per species
    group_by(taxon_scientific_name) %>%
    filter(assessment_date == max(assessment_date)) %>%
    ungroup() %>%
    ## add a row number column for progress tracking
    mutate(rowNumber = row_number()) %>%
    # update the old criteria version to align with the new
    rowwise() %>%
    mutate(criteria_V3.1 = rlVersionUpdater(rlAssessmentID = assessment_id,
                                            rlCategory = red_list_category_code,
                                            rlFullCriteria = criteria,
                                            rlVersion = red_list_category_version)) %>%
    ## Risk column based on rl code -- weights from Table 1
    mutate(Risk = case_when(red_list_category_code == "EW" ~ 8,
                            red_list_category_code == "CR" ~ 4,
                            red_list_category_code == "EN" ~ 2,
                            red_list_category_code == "VU" ~ 1,
                            TRUE ~ 0)) %>%
    ## Endemic column:
    # Table 2, condition 2 -- by number of countries occupied where OCCUPIED means
    #       presence = 'extant' or 'possibly extinct' for CR, EN, VU. "Extinct Post-1500" for "EW".
    mutate(NumberOfCountriesExtant = {
      if (red_list_category_code == "EW") {
        actuallyPresent <- which(locations_presence == "Extinct Post-1500")
        if (length(actuallyPresent) > 0) {length(actuallyPresent)} else {NA}
      } else {
        actuallyPresent <- which(locations_presence %in% c("Extant", "Possibly Extinct"))
        if (length(actuallyPresent) > 0) {length(actuallyPresent)} else {NA}
      }
    }) %>%
    # now calculate
    mutate(Endemic = 10 / NumberOfCountriesExtant) %>%
    # ...and Table 2, condition 3 -- if Extinct in the Wild
    mutate(Endemic = case_when(red_list_category_code == "EW" ~ 1,
                               TRUE ~ Endemic)) %>%
    ## Decline column:
    rowwise() %>%
    mutate(Decline = 
             declineWeightHelper(rlAssessmentID = assessment_id,
                                 rlCategory = red_list_category_code,
                                 rlFullCriteria = criteria_V3.1,
                                 rlPopTrend = population_trend_code,
                                 rowNumber = rowNumber,
                                 rlContDecPop = supplementary_info_population_continuing_decline,
                                 rlContDecArea = supplementary_info_continuing_decline_in_area,
                                 rlContDecLoc = supplementary_info_continuing_decline_in_number_of_locations,
                                 rlContDecEOO = supplementary_info_continuing_decline_in_extent_of_occurence,
                                 rlContDecAOO = supplementary_info_continuing_decline_in_area_of_occupancy,
                                 rlContDecSubpop = supplementary_info_continuing_decline_in_subpopulations)) %>%
    ## Restriction column:
    mutate(Restriction =
             restrictionWeightHelper(rlAssessmentID = assessment_id,
                                     rlCategory = red_list_category_code,
                                     rlFullCriteria = criteria_V3.1,
                                     rlPopSize = supplementary_info_population_size,
                                     rlAreaRestricted = supplementary_info_area_restricted_is_restricted,
                                     rlAOO = supplementary_info_estimated_area_of_occupancy,
                                     rlEOO = supplementary_info_estimated_extent_of_occurence,
                                     rlLocNumber = as.numeric(supplementary_info_number_of_locations),
                                     rowNumber = rowNumber)) %>%
    ## Priority Scores - Equations 1 and 2
    mutate(Priority1_PS = Risk * Endemic * Decline * Restriction,
           Priority2_PS = Risk * Endemic * max(Decline, Restriction)) %>%
    ## convert to rank
    # priority 1 rank
    group_by() %>% # needs this for if_else to operate rowwise
    mutate(Priority1_Rank = if_else(Priority1_PS > 0, min_rank(desc(Priority1_PS)), NA)) %>% 
    ungroup() %>%
    # priority 2 rank
    group_by(Priority1_PS) %>%
    mutate(Priority2_Rank = if_else(Priority1_PS == 0, min_rank(desc(Priority2_PS)), NA)) %>% 
    ungroup() %>%
    # arrange by P1 and P2
    arrange(Priority1_Rank, Priority2_Rank) ->
    # return the final table jm
    priorityTable
  return(priorityTable)
}


## function: for a given two letter RL country code, output a table including Priority 1 and
##      Priority 2 scores and ranks
generatePrioritySpeciesList_byCountry <- function(countryCode) {
  ## scrape redlist for assessments for given country if not in memory already
  assessmentName <- paste0("countryAssessment_", countryCode)
  if (!exists(assessmentName)) {
    assign(x = assessmentName,
           value = rl_countries(code = countryCode,
                                latest = TRUE, # only the latest assessment for each taxon
                                key = rlapiKey)$assessments,
           envir = .GlobalEnv)
  }
  ## use that assessment list to generate the Target 4 Priority species list
  assessmentListToSpeciesList(assessmentName) %>%
    rowwise() %>%
    ## filter all species to origin = native, reintroduced, or assisted colonization
    # add column for focal country origin status
    mutate(focalCountryOrigin = {
      focalCountryIndex <- which(locations_code == countryCode)
      if (length(focalCountryIndex) > 0) {locations_origin[focalCountryIndex]} else {NA}
    }) %>%
    # filter
    filter(focalCountryOrigin %in% c("Native", "Reintroduced", "Assisted Colonisation")) %>%
    ## filter CR, EN, VU species to presence = extant or possibly extant
    # add column for focal country presence
    mutate(focalCountryPresence = {
      focalCountryIndex <- which(locations_code == countryCode)
      if (length(focalCountryIndex) > 0) {locations_presence[focalCountryIndex]} else {NA}
    }) %>%
    filter((red_list_category_code == "EW") |
             (focalCountryPresence %in% c("Extant", "Possibly Extinct"))) %>%
  speciesListToPriorityTable() -> priorityTable
  return(priorityTable)
}


## function: for a given taxonomic group and its matching taxonomic level, output a table
##      including Priority 1 and Priority 2 scores and ranks. Allowable taxonomic levels include
##      c("class", "order", "family")
generatePrioritySpeciesList_byTaxa <- function(selectedTaxa, selectedTaxonomicGroup) {
  ## select appropriate RL scraper function for selected taxonomic group
  if (selectedTaxonomicGroup == "class") {
    appropriateTaxaScraper <- rl_class
  } else if (selectedTaxonomicGroup == "order") {
    appropriateTaxaScraper <- rl_order
  } else if (selectedTaxonomicGroup == "family") {
    appropriateTaxaScraper <- rl_family
  } else if (selectedTaxonomicGroup == "growth_forms") {
    appropriateTaxaScraper <- rl_growth_forms
  } else {break}
  ## scrape redlist for assessments for given taxa if not in memory already
  assessmentName <- paste0("taxaAssessment_", selectedTaxa)
  if (!exists(assessmentName)) {
    assign(x = assessmentName,
           value = appropriateTaxaScraper(selectedTaxa,
                                          latest = TRUE,
                                          key = rlapiKey)$assessments,
           envir = .GlobalEnv)
  }
  ## use that assessment list to generate the Target 4 Priority species list
  if (!is.null(get(assessmentName)) & nrow(get(assessmentName)) > 0) {
    assessmentListToSpeciesList(assessmentName) %>%
      speciesListToPriorityTable() -> priorityTable
    return(priorityTable)
  } else {return(NA)}
}
