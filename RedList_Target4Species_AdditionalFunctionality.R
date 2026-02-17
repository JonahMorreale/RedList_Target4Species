## Red List - Target 4 Species Additional Functionality
## author: Jonah Morreale - jonah.morreale@stonybrook.edu
## description: Additional functions to add advanced functionality for modifying and
##      combining Target4SpeciesList outputs
## updated: 02/16/2026


### packages
library(tidyverse)


## function: Given a LIST (not vector) of multiple Target4SpeciesList tables (the output of main code, or
##      the read in csv of same), combine all tables in the vector and re-calculate the PS1 and PS2
##      rankings based on PS1 and PS2 scores.
##      NOTE: If multiple assessments contain the same species (say, across multiple countries),
##      only the one with the NEWEST ASSESSMENT will be kept in terms of focal country calculations.
combineAndRerank <- function(Target4SpeciesList_List) {
  do.call(rbind, Target4SpeciesList_List) %>%
    ## remove duplicates (say, if multiple countries contain the same species)
    group_by(ScientificName) %>%
    filter(assessment_date == max(assessment_date)) %>%
    ungroup() %>%
    ## convert to rank
    # priority 1 rank
    group_by() %>% # needs this for if_else to operate rowwise
    mutate(Priority1_Rank = if_else(Priority1_PS > 0, min_rank(desc(Priority1_PS)), NA)) %>% 
    ungroup() %>%
    # priority 2 rank jm
    group_by(Priority1_PS) %>%
    mutate(Priority2_Rank = if_else(Priority1_PS == 0, min_rank(desc(Priority2_PS)), NA)) %>% 
    ungroup() %>%
    # arrange by P1 and P2
    arrange(Priority1_Rank, Priority2_Rank) %>%
    # return the final table
    return()
}


## function: Given a data frame with the four Target 4 methodology scoring columns (Risk, Endemic, Decline,
##      Restriction), recalculate the PS1 and PS2 ranks and scores. This allows for rescoring after manual
##      modification of individual fields or species, for instance.
recalculatePriorityScores <- function(Target4SpeciesList) {
  Target4SpeciesList %>%
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
    arrange(Priority1_Rank, Priority2_Rank) %>%
    # return the final table
    return()
}
