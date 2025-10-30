# RedList_Target4Species Country Assessment Tool  
## Description  
## Instructions  
### One-Time Setup  
1. Download the entire RedList_Target4Species project to a location on your machine.
2. Open "RedList_Target4Species_OneTimeSetup.R" in R Studio.
3. Run the code within to install the four packages necessary for executing the main script. To run this installation code, highlight the entire script (CTRL/COMMAND + A) and press "Run" at the top of the window or hit (CTRL/COMMAND + ENTER) to execute it.  

### Generating New Country Assessments  
1. Open RedList_Target4Species.R in R Studio.
2. Manually replace the following three lines at the top of the code, each marked with a string of dashes in the preceding line:
    * Where it says "REPLACE PATH TO YOUR WORKING FOLDER HERE", replace those words inside the quotation marks (leaving the quotes)
    with the full system path to the RedList_Target4Species folder containing the project, as downloaded during One-Time Setup.  
        - Note: Use FORWARD SLASHES to separate folders.  
        - Example:  
        ```myDir <- "C:/JohnSmith/WorkScripts/RedList_Target4Species"```
    * Where it says "REPLACE REDLIST API KEY HERE", replace those words inside the quotation marks (leaving the quotes) with the
    API key code provided by IUCN when registering an account.
    * Replace the countries in the "selectedCountryList" with the target countries for Target 4 species assessments.
        - Note: The country names must each be within the parentheses, within their own quotation marks, and separated by commas when more
        than one targeted. Country names must be spelled according to the ISO country name standard used by Red List - see the file
        "RL Country List.xlsx" for spellings.  
        - Example:
        ```selectedCountryList <- c("Viet Nam", "Fiji", "Indonesia")```
        - Example:
        ```selectedCountryList <- c("India")```
3. Run the entire script. To do so, highlight the entire script (CTRL/COMMAND + A) and press "Run" at the top of the window or
hit (CTRL/COMMAND + ENTER) to execute it.
4. That's it! If it is running properly code, the console will display various trackers and progress updates as the tool works through the steps
of pulling the Red List data and generating the assessment. Once it's done, the console will return to an idle state (the ">" symbol will return)
and a slew of WARNING messages will generate - you can safely ignore these. Any ERROR messages means the tool has encountered a problem and did not
complete properly.  
The assessments for the specified countries will be output to the "CountryAssessmentTables" subfolder within the main directory. The output files
include both .csv and .xlsx files for each country, named in the format "Target4SpeciesList_COUNTRYNAME.csv"  

## Other Files and Folders Within  
* Folder "CountryAssessmentTables" will contain the country assessment files output by the tool.  
* File "RedList_Target4Species_helpers.R" contains functions used by the main R script to perform the country assessments, but should not be run or modified directly itself.  
* Files "RedList_Target4Species_TableXLookup.csv" and "._VersionUpdateLookup.csv" contain criteria necessary for Target 4 scoring and should not be
modified.  
* File "RL Country List.xlsx" contains a table of country names and codes as recognized by the Red List API - consult this for spelling when
entering countries to be run via the assessment tool.  