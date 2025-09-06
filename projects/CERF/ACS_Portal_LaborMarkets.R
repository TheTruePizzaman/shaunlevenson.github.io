# --------------------------------------------------------------------------- #
# # ACS_Portal_LaborMarket.R: Time-Series 1yr and 5yr ACS PULL                #
# S.LEVENSON(2025)															                              #
# --------------------------------------------------------------------------- #

####################
#---INSTRUCTIONS---#
####################

# Summary of manual adjustments to be made as required:
# 1. Preliminary Work:
# a. Change the `table_focus` variable to the year you want to focus on.
# b. Adjust the `year_focus` variable to the range of years you want to include in your analysis.
# c. Modify the `geo_list` to include the geographies you want to pull data for.
# d. Modify `metadata_geo` with the geography you want to Quality check with 
# e. Change the file path and sheet name as you see fit

# 2. Variable Code Specification:
# a. Update the variable codes in X_vars_by_year to match the year_focus range and ensure the variable codes are correct for the new year.
# b. Ensure that the remapping in x_vars_remap reflects the correct variable names for each year (they can change).
# c. Add any additional tables or variables as needed using the same format.

# 3. GENERAL ACS TABLE RETRIEVAL AND PROCESSING
# a. If including new tables, update the `tables` list to include the new table names. Format should be obvious
#b. Do the same for "metadata_all_years"

# 4. Data Derivation:
# a. Modify the `regions` object as necessary
# b. run the function below it
# c. copy the output and paste into the mutate operation below that

###################################
#-------------Notes---------------#
###################################

#--------------------------------------#

##################################
#---INSTALL PACKAGES & API KEY---#
##################################
# Define the list of packages to install and load
packages <- c("tidycensus", "tidyverse", "httr", "jsonlite", "openxlsx2", "dplyr","stringr","purrr","tidyr")

# Install only packages that are not already installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load the libraries
lapply(packages, library, character.only = TRUE)

#---API KEY---#
#census_api_key("3ce6f083bdafa6331368a608fa56f25c66cb1c28", overwrite=TRUE,install= TRUE)

######################
## Preliminary Work ##	
######################
#-- Table Focus --#
table_focus <- 2023  # This is the table you want to focus on, change as needed

# Changes the year loop that will be used in the functions
year_focus <- 2005:2023

#code to identify variable nmemonics, change table_focus year to manually search in R......
subject_vars <- load_variables(table_focus, dataset = "acs1/subject")
profile_vars <- load_variables(table_focus, dataset = "acs1/profile")
detailed_vars <- load_variables(table_focus, dataset = "acs1")

# Specify the survey type, can be "acs1" for 1-year or "acs5" for 5-year data
survey = c("acs1", "acs5") 

# Geographies to pull data for, if you want more geographies, you have to add them here
geo_list <- list(
  "United States" = list(geography = "us"),
  "California" = list(geography = "state", state = "CA"),
  "Los Angeles County" = list(geography = "county", state = "CA", county = "037"),
  "Ventura County" = list(geography = "county", state = "CA", county = "111")
)

# Change the geography based on your quality check specifications on the metadata tab
metadata_geo <- "us"

#-- Excel Save Path and Sheet Name --#
file_path_1yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_LaborMarkets_1yr.xlsx"
file_path_5yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_LaborMarkets_5yr.xlsx"
sheet_name_1yr <- "LaborMarkets_1yr"
sheet_name_5yr <- "LaborMarkets_5yr"

#-----------------------------------------------------------------------------------------------------------------------

###############################################
##--- VARIABLE CODE SPECIFICATION BY YEAR ---##
###############################################

#-- Table DP03 "All Ethnicity"-- # 

# Get the variable names for the table from 2010 to 2023
DP03_vars_by_year <- list(
  "2010" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2011" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2012" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2013" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2014" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2015" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2016" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2017" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2018" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2019" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2021" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2022" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E"),
  "2023" = c("DP03_0001E","DP03_0003E","DP03_0004E","DP03_0006E")
)

DP03_vars_remap <- list(
  "2010" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2011" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2012" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2013" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2014" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2015" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2016" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2017" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2018" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2019" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2021" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2022" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces"),
  "2023" = c("DP03_0001E" = "Total Population 16+","DP03_0003E" = "Total 16+ In Civilian Labor Force","DP03_0004E" = "Total 16+ Civilian Employed","DP03_0006E" = "Total 16+ In Armed Forces")
)

#-- Table C23002i "Hispanic" -- #

# Get the variable names for the table from 2010 to 2023
C23002I_vars_by_year <- list(
  "2010" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2011" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2012" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2013" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2014" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2015" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2016" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2017" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2018" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2019" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2021" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2022" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E"),
  "2023" = c("C23002I_001E","C23002I_004E","C23002I_005E","C23002I_007E","C23002I_011E","C23002I_012E","C23002I_017E","C23002I_018E","C23002I_020E","C23002I_024E","C23002I_025E")
)

C23002I_vars_remap <- list(
  "2010" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2011" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2012" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2013" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2014" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2015" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2016" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2017" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2018" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2019" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2021" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2022" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed"),
  "2023" = c("C23002I_001E"="Hispanic Population 16+","C23002I_004E"="Male 16-64 Hispanic In Labor Force","C23002I_005E"="Male 16-64 Hispanic In Armed Forces","C23002I_007E"="Male 16-64 Hispanic Employed","C23002I_011E"="Male 65+ Hispanic In Labor Force","C23002I_012E"="Male 65+ Hispanic Employed","C23002I_017E"="Female 16-64 Hispanic In Labor Force","C23002I_018E"="Female 16-64 Hispanic In Armed Forces","C23002I_020E"="Female 16-64 Hispanic Employed","C23002I_024E"="Female 65+ Hispanic In Labor Force","C23002I_025E"="Female 65+ Hispanic Employed")
)

###########################################################
##--- GENERAL ACS TABLE DATA RETRIEVAL AND PROCESSING ---##
###########################################################
# -- Function to detect dataset type based on variable codes -- #
detect_dataset <- function(codes) {
  prefixes <- unique(sub("_.*", "", gsub("E$", "", codes)))
  
  if (all(grepl("^DP", prefixes))) {
    return("profile")  # used in acs1/profile etc.
  } else if (all(grepl("^S", prefixes))) {
    return("subject")  # used in acs1/subject etc.
  } else if (all(grepl("^B", prefixes) | grepl("^C", prefixes))) {
    return("")  # summary tables don't get a suffix
  } else {
    stop("Unable to determine dataset from prefixes: ", paste(prefixes, collapse = ", "))
  }
}


# -- Function to retrieve metadata for a given year, survey type, and list of codes -- #
get_metadata_for_year <- function(codes, year, survey = "acs1", geography = metadata_geo) {
  dataset_type <- detect_dataset(codes)
  
  # Correct dataset path
  dataset <- if (dataset_type == "") {
    survey
  } else {
    paste0(survey, "/", dataset_type)
  }
  
  message("Loading metadata for year ", year, " from ", dataset)
  
  tryCatch({
    code_clean <- gsub("E$", "", codes)
    
    # Load metadata
    vars <- load_variables(as.integer(year), dataset = dataset, cache = TRUE)
    
    meta <- vars %>%
      filter(name %in% code_clean) %>%
      select(code = name, label, concept) %>%
      mutate(year = year, dataset = dataset) %>%
      relocate(year, dataset, code, label, concept)
    
    # Get actual estimates
    acs_data <- tryCatch({
      get_acs(
        geography = geography,
        variables = code_clean,
        year = as.integer(year),
        survey = survey
      ) %>%
        mutate(code = gsub("E$", "", variable)) %>%
        select(code, estimate) %>%
        rename(!!paste0(metadata_geo, " estimate") := estimate)
    }, error = function(e) {
      message("  → Could not fetch ACS estimates: ", e$message)
      tibble(code = character(), !!paste0(metadata_geo, " estimate") := numeric())
    })
    
    meta <- left_join(meta, acs_data, by = "code")
    return(meta)
    
  }, error = function(e) {
    message("  → Error loading year ", year, ": ", e$message)
    return(NULL)
  })
}


# -- Loop to collect for both 1-year and 5-year surveys -- #

# Replace DP05_vars_by_year with your actual list of codes by year
metadata_1yr_all <- bind_rows(
  imap_dfr(DP03_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs1")),
  imap_dfr(C23002I_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs1")),
)

metadata_5yr_all <- bind_rows(
  imap_dfr(DP03_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs5")),
  imap_dfr(C23002I_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs5")),
)

#---------------------------------------------------------------------------# 
#-- Function to retrieve ACS data for a specific table, year, and survey --#
get_census_table <- function(table_id, vars_by_year, year, geography, survey, state = NULL, county = NULL) {
  # Get the variable codes for the specified year from vars_by_year
  vars <- vars_by_year[[as.character(year)]]
  
  # Return NULL if there are no variables to pull for this year
  if (is.null(vars) || all(vars == 0)) return(NULL)
  
  # Prepare arguments for get_acs call
  args <- list(
    geography = geography,
    variables = vars,
    year = year,
    survey = survey,  # Use the passed survey ("acs1" or "acs5")
    output = "wide"
  )
  
  if (!is.null(state)) args$state <- state
  if (!is.null(county)) args$county <- county
  
  df <- do.call(get_acs, args)
  df$year <- year
  
  return(df)
}

#-- Function to process census data for a specific region, year range, and survey type --#
process_region_census_table <- function(
    table_id, 
    geo_label, 
    geography,
    vars_by_year, 
    remap_by_year,
    year_focus,
    survey = "acs1",   # Add survey argument with default
    state = NULL, 
    county = NULL
) {
  df <- map_dfr(year_focus, function(y) {
    get_census_table(table_id, vars_by_year, y, geography, survey, state, county)
  }) %>%
    arrange(year) %>%
    select(-matches(paste0("^", table_id, ".*M$|^", table_id, ".*PE$|GEOID"))) %>%
    group_split(year) %>%
    map_dfr(function(df_year) {
      yr <- unique(df_year$year)
      remap <- remap_by_year[[as.character(yr)]]
      
      if (is.null(remap) || all(remap == 0)) return(df_year)
      
      for (var_code in names(remap)) {
        if (var_code %in% names(df_year)) {
          df_year[[remap[[var_code]]]] <- df_year[[var_code]]
        }
      }
      
      return(df_year)
    }) %>%
    select(year, all_of(unique(unlist(remap_by_year)))) %>%
    mutate(Region = geo_label) %>%
    relocate(Region, .before = everything())
  
  return(df)
}

# Define ACS tables with their variable mappings by year
###############		###############		###############
##-- MANUALLY CHANGE THIS IF ADDING NEW TABLES --##
###############		###############		###############
tables <- list(
  DP03 = list(vars_by_year = DP03_vars_by_year, remap_by_year = DP03_vars_remap),
  C23002I = list(vars_by_year = C23002I_vars_by_year, remap_by_year = C23002I_vars_remap)
)

# Create empty lists for storing 1-year and 5-year results
results_1yr <- list()
results_5yr <- list()

survey_list <- c("acs1", "acs5")  # define this explicitly

for (svy in survey_list) {
  valid_years <- if (svy == "acs5") {
    year_focus[year_focus >= 2009]
  } else {
    year_focus
  }
  
  for (tbl in names(tables)) {
    cat("Processing table:", tbl, "for survey:", svy, "\n")
    
    for (region in names(geo_list)) {
      geo_info <- geo_list[[region]]
      
      res <- process_region_census_table(
        table_id = tbl,
        geo_label = region,
        geography = geo_info$geography,
        vars_by_year = tables[[tbl]]$vars_by_year,
        remap_by_year = tables[[tbl]]$remap_by_year,
        year_focus = valid_years,
        survey = svy,
        state = geo_info$state %||% NULL,
        county = geo_info$county %||% NULL
      )
      
      res$table_id <- tbl
      res$survey <- svy
      key <- paste(tbl, region, svy, sep = "_")
      
      if (svy == "acs1") {
        results_1yr[[key]] <- res
      } else {
        results_5yr[[key]] <- res
      }
    }
  }
}

#--- Combine Data --- #
# -- Function to clean and pivot results -- #
clean_and_pivot_results <- function(results_list) {
  bind_rows(results_list) %>%
    select(-table_id, -survey) %>%
    pivot_longer(
      cols = -c(year, Region),
      names_to = "Variable",
      values_to = "Value"
    ) %>%
    filter(!is.na(Value)) %>%
    distinct(year, Region, Variable, .keep_all = TRUE) %>%
    mutate(Var_Geo = paste(Variable, Region, sep = " - ")) %>%
    select(year, Var_Geo, Value) %>%
    pivot_wider(names_from = Var_Geo, values_from = Value) %>%
    arrange(year)
}

# -- Generate final cleaned datasets for each survey type -- #
final_result_1yr <- clean_and_pivot_results(results_1yr)
final_result_5yr <- clean_and_pivot_results(results_5yr)
#########################
### Data Derivations ####
#########################
### Provide Manual Specifications Here ####
regions <- c("United States", "California", "Los Angeles County", "Ventura County")
genders <- c("Male", "Female")
ages <- c("16-64", "65+")

# Run This Chunk of code and then copy and paste the output into the R script 
###########################################################################
# Code generation
for (region in regions) {
  clean_region <- region  # keep original name for columns
  
  # --- Hispanic Civilian Labor Force ---
  civ_lf_parts <- c()
  for (gender in genders) {
    for (age in ages) {
      civ_lf_parts <- c(civ_lf_parts, paste0("`", gender, " ", age, " Hispanic In Labor Force - ", region, "`"))
    }
  }
  hisp_civ_lf <- paste0("`Hispanic 16+ In Civilian Labor Force - ", clean_region, "`")
  cat(paste0(hisp_civ_lf, " = ", paste(civ_lf_parts, collapse = " + "), ",\n"))
  
  # --- Hispanic Armed Forces Labor Force ---
  armed_lf_parts <- c(
    paste0("`Male 16-64 Hispanic In Armed Forces - ", region, "`"),
    paste0("`Female 16-64 Hispanic In Armed Forces - ", region, "`")
  )
  hisp_armed_lf <- paste0("`Hispanic 16+ In Armed Forces - ", clean_region, "`")
  cat(paste0(hisp_armed_lf, " = ", paste(armed_lf_parts, collapse = " + "), ",\n"))
  
  # --- Hispanic Total Labor Force = Civilian + Armed Forces ---
  hisp_total_lf <- paste0("`Hispanic 16+ In Labor Force - ", clean_region, "`")
  cat(paste0(hisp_total_lf, " = ", hisp_civ_lf, " + ", hisp_armed_lf, ",\n"))
  
  # --- Hispanic Civilian Employed ---
  civ_emp_parts <- c()
  for (gender in genders) {
    for (age in ages) {
      civ_emp_parts <- c(civ_emp_parts, paste0("`", gender, " ", age, " Hispanic Employed - ", region, "`"))
    }
  }
  hisp_civ_emp <- paste0("`Hispanic 16+ Civilian Employed - ", clean_region, "`")
  cat(paste0(hisp_civ_emp, " = ", paste(civ_emp_parts, collapse = " + "), ",\n"))
  
  # --- Hispanic Total Employed = Civilian Employed + Armed Forces ---
  armed_emp_parts <- c(
    paste0("`Male 16-64 Hispanic In Armed Forces - ", region, "`"),
    paste0("`Female 16-64 Hispanic In Armed Forces - ", region, "`")
  )
  hisp_total_emp <- paste0("`Hispanic 16+ Employed - ", clean_region, "`")
  cat(paste0(hisp_total_emp, " = ", hisp_civ_emp, " + ", paste(armed_emp_parts, collapse = " + "), ",\n"))
  
  # === Define Total (Non-Ethnicity Specific) values ===
  total_civ_lf <- paste0("`Total 16+ In Civilian Labor Force - ", clean_region, "`")
  total_armed_lf <- paste0("`Total 16+ In Armed Forces - ", clean_region, "`")
  total_total_lf <- paste0("`Total 16+ In Labor Force - ", clean_region, "`")
  cat(paste0(total_total_lf, " = ", total_civ_lf, " + ", total_armed_lf, ",\n"))
  
  total_civ_emp <- paste0("`Total 16+ Civilian Employed - ", clean_region, "`")
  total_total_emp <- paste0("`Total 16+ Employed - ", clean_region, "`")
  cat(paste0(total_total_emp, " = ", total_civ_emp, " + ", total_armed_lf, ",\n"))
  
  # === Non-Hispanic Derivations (Backed Out) ===
  nonhisp_civ_lf <- paste0("`Non Hispanic 16+ In Civilian Labor Force - ", clean_region, "`")
  cat(paste0(nonhisp_civ_lf, " = ", total_civ_lf, " - ", hisp_civ_lf, ",\n"))
  
  nonhisp_armed_lf <- paste0("`Non Hispanic 16+ In Armed Forces - ", clean_region, "`")
  cat(paste0(nonhisp_armed_lf, " = ", total_armed_lf, " - ", hisp_armed_lf, ",\n"))
  
  nonhisp_total_lf <- paste0("`Non Hispanic 16+ In Labor Force - ", clean_region, "`")
  cat(paste0(nonhisp_total_lf, " = ", total_total_lf, " - ", hisp_total_lf, ",\n"))
  
  nonhisp_civ_emp <- paste0("`Non Hispanic 16+ Civilian Employed - ", clean_region, "`")
  cat(paste0(nonhisp_civ_emp, " = ", total_civ_emp, " - ", hisp_civ_emp, ",\n"))
  
  nonhisp_total_emp <- paste0("`Non Hispanic 16+ Employed - ", clean_region, "`")
  cat(paste0(nonhisp_total_emp, " = ", total_total_emp, " - ", hisp_total_emp, ",\n\n"))
}

###########################################################################

# Paste Below
add_labor_force_vars <- function(df) {
  df %>%
    mutate(
      `Hispanic 16+ In Civilian Labor Force - United States` = `Male 16-64 Hispanic In Labor Force - United States` + `Male 65+ Hispanic In Labor Force - United States` + `Female 16-64 Hispanic In Labor Force - United States` + `Female 65+ Hispanic In Labor Force - United States`,
      `Hispanic 16+ In Armed Forces - United States` = `Male 16-64 Hispanic In Armed Forces - United States` + `Female 16-64 Hispanic In Armed Forces - United States`,
      `Hispanic 16+ In Labor Force - United States` = `Hispanic 16+ In Civilian Labor Force - United States` + `Hispanic 16+ In Armed Forces - United States`,
      `Hispanic 16+ Civilian Employed - United States` = `Male 16-64 Hispanic Employed - United States` + `Male 65+ Hispanic Employed - United States` + `Female 16-64 Hispanic Employed - United States` + `Female 65+ Hispanic Employed - United States`,
      `Hispanic 16+ Employed - United States` = `Hispanic 16+ Civilian Employed - United States` + `Male 16-64 Hispanic In Armed Forces - United States` + `Female 16-64 Hispanic In Armed Forces - United States`,
      `Total 16+ In Labor Force - United States` = `Total 16+ In Civilian Labor Force - United States` + `Total 16+ In Armed Forces - United States`,
      `Total 16+ Employed - United States` = `Total 16+ Civilian Employed - United States` + `Total 16+ In Armed Forces - United States`,
      `Non Hispanic 16+ In Civilian Labor Force - United States` = `Total 16+ In Civilian Labor Force - United States` - `Hispanic 16+ In Civilian Labor Force - United States`,
      `Non Hispanic 16+ In Armed Forces - United States` = `Total 16+ In Armed Forces - United States` - `Hispanic 16+ In Armed Forces - United States`,
      `Non Hispanic 16+ In Labor Force - United States` = `Total 16+ In Labor Force - United States` - `Hispanic 16+ In Labor Force - United States`,
      `Non Hispanic 16+ Civilian Employed - United States` = `Total 16+ Civilian Employed - United States` - `Hispanic 16+ Civilian Employed - United States`,
      `Non Hispanic 16+ Employed - United States` = `Total 16+ Employed - United States` - `Hispanic 16+ Employed - United States`,
      
      `Hispanic 16+ In Civilian Labor Force - California` = `Male 16-64 Hispanic In Labor Force - California` + `Male 65+ Hispanic In Labor Force - California` + `Female 16-64 Hispanic In Labor Force - California` + `Female 65+ Hispanic In Labor Force - California`,
      `Hispanic 16+ In Armed Forces - California` = `Male 16-64 Hispanic In Armed Forces - California` + `Female 16-64 Hispanic In Armed Forces - California`,
      `Hispanic 16+ In Labor Force - California` = `Hispanic 16+ In Civilian Labor Force - California` + `Hispanic 16+ In Armed Forces - California`,
      `Hispanic 16+ Civilian Employed - California` = `Male 16-64 Hispanic Employed - California` + `Male 65+ Hispanic Employed - California` + `Female 16-64 Hispanic Employed - California` + `Female 65+ Hispanic Employed - California`,
      `Hispanic 16+ Employed - California` = `Hispanic 16+ Civilian Employed - California` + `Male 16-64 Hispanic In Armed Forces - California` + `Female 16-64 Hispanic In Armed Forces - California`,
      `Total 16+ In Labor Force - California` = `Total 16+ In Civilian Labor Force - California` + `Total 16+ In Armed Forces - California`,
      `Total 16+ Employed - California` = `Total 16+ Civilian Employed - California` + `Total 16+ In Armed Forces - California`,
      `Non Hispanic 16+ In Civilian Labor Force - California` = `Total 16+ In Civilian Labor Force - California` - `Hispanic 16+ In Civilian Labor Force - California`,
      `Non Hispanic 16+ In Armed Forces - California` = `Total 16+ In Armed Forces - California` - `Hispanic 16+ In Armed Forces - California`,
      `Non Hispanic 16+ In Labor Force - California` = `Total 16+ In Labor Force - California` - `Hispanic 16+ In Labor Force - California`,
      `Non Hispanic 16+ Civilian Employed - California` = `Total 16+ Civilian Employed - California` - `Hispanic 16+ Civilian Employed - California`,
      `Non Hispanic 16+ Employed - California` = `Total 16+ Employed - California` - `Hispanic 16+ Employed - California`,
      
      `Hispanic 16+ In Civilian Labor Force - Los Angeles County` = `Male 16-64 Hispanic In Labor Force - Los Angeles County` + `Male 65+ Hispanic In Labor Force - Los Angeles County` + `Female 16-64 Hispanic In Labor Force - Los Angeles County` + `Female 65+ Hispanic In Labor Force - Los Angeles County`,
      `Hispanic 16+ In Armed Forces - Los Angeles County` = `Male 16-64 Hispanic In Armed Forces - Los Angeles County` + `Female 16-64 Hispanic In Armed Forces - Los Angeles County`,
      `Hispanic 16+ In Labor Force - Los Angeles County` = `Hispanic 16+ In Civilian Labor Force - Los Angeles County` + `Hispanic 16+ In Armed Forces - Los Angeles County`,
      `Hispanic 16+ Civilian Employed - Los Angeles County` = `Male 16-64 Hispanic Employed - Los Angeles County` + `Male 65+ Hispanic Employed - Los Angeles County` + `Female 16-64 Hispanic Employed - Los Angeles County` + `Female 65+ Hispanic Employed - Los Angeles County`,
      `Hispanic 16+ Employed - Los Angeles County` = `Hispanic 16+ Civilian Employed - Los Angeles County` + `Male 16-64 Hispanic In Armed Forces - Los Angeles County` + `Female 16-64 Hispanic In Armed Forces - Los Angeles County`,
      `Total 16+ In Labor Force - Los Angeles County` = `Total 16+ In Civilian Labor Force - Los Angeles County` + `Total 16+ In Armed Forces - Los Angeles County`,
      `Total 16+ Employed - Los Angeles County` = `Total 16+ Civilian Employed - Los Angeles County` + `Total 16+ In Armed Forces - Los Angeles County`,
      `Non Hispanic 16+ In Civilian Labor Force - Los Angeles County` = `Total 16+ In Civilian Labor Force - Los Angeles County` - `Hispanic 16+ In Civilian Labor Force - Los Angeles County`,
      `Non Hispanic 16+ In Armed Forces - Los Angeles County` = `Total 16+ In Armed Forces - Los Angeles County` - `Hispanic 16+ In Armed Forces - Los Angeles County`,
      `Non Hispanic 16+ In Labor Force - Los Angeles County` = `Total 16+ In Labor Force - Los Angeles County` - `Hispanic 16+ In Labor Force - Los Angeles County`,
      `Non Hispanic 16+ Civilian Employed - Los Angeles County` = `Total 16+ Civilian Employed - Los Angeles County` - `Hispanic 16+ Civilian Employed - Los Angeles County`,
      `Non Hispanic 16+ Employed - Los Angeles County` = `Total 16+ Employed - Los Angeles County` - `Hispanic 16+ Employed - Los Angeles County`,
      
      `Hispanic 16+ In Civilian Labor Force - Ventura County` = `Male 16-64 Hispanic In Labor Force - Ventura County` + `Male 65+ Hispanic In Labor Force - Ventura County` + `Female 16-64 Hispanic In Labor Force - Ventura County` + `Female 65+ Hispanic In Labor Force - Ventura County`,
      `Hispanic 16+ In Armed Forces - Ventura County` = `Male 16-64 Hispanic In Armed Forces - Ventura County` + `Female 16-64 Hispanic In Armed Forces - Ventura County`,
      `Hispanic 16+ In Labor Force - Ventura County` = `Hispanic 16+ In Civilian Labor Force - Ventura County` + `Hispanic 16+ In Armed Forces - Ventura County`,
      `Hispanic 16+ Civilian Employed - Ventura County` = `Male 16-64 Hispanic Employed - Ventura County` + `Male 65+ Hispanic Employed - Ventura County` + `Female 16-64 Hispanic Employed - Ventura County` + `Female 65+ Hispanic Employed - Ventura County`,
      `Hispanic 16+ Employed - Ventura County` = `Hispanic 16+ Civilian Employed - Ventura County` + `Male 16-64 Hispanic In Armed Forces - Ventura County` + `Female 16-64 Hispanic In Armed Forces - Ventura County`,
      `Total 16+ In Labor Force - Ventura County` = `Total 16+ In Civilian Labor Force - Ventura County` + `Total 16+ In Armed Forces - Ventura County`,
      `Total 16+ Employed - Ventura County` = `Total 16+ Civilian Employed - Ventura County` + `Total 16+ In Armed Forces - Ventura County`,
      `Non Hispanic 16+ In Civilian Labor Force - Ventura County` = `Total 16+ In Civilian Labor Force - Ventura County` - `Hispanic 16+ In Civilian Labor Force - Ventura County`,
      `Non Hispanic 16+ In Armed Forces - Ventura County` = `Total 16+ In Armed Forces - Ventura County` - `Hispanic 16+ In Armed Forces - Ventura County`,
      `Non Hispanic 16+ In Labor Force - Ventura County` = `Total 16+ In Labor Force - Ventura County` - `Hispanic 16+ In Labor Force - Ventura County`,
      `Non Hispanic 16+ Civilian Employed - Ventura County` = `Total 16+ Civilian Employed - Ventura County` - `Hispanic 16+ Civilian Employed - Ventura County`,
      `Non Hispanic 16+ Employed - Ventura County` = `Total 16+ Employed - Ventura County` - `Hispanic 16+ Employed - Ventura County`
    )
}

final_result_1yr <- add_labor_force_vars(final_result_1yr)
final_result_5yr <- add_labor_force_vars(final_result_5yr)


########################
#--- Reorder        ---#
########################
process_final_result <- function(df) {
  regions_order <- c("United States", "California", "Los Angeles County", "Ventura County")
  
  extract_region <- function(colname) {
    for (region in regions_order) {
      if (grepl(region, colname, fixed = TRUE)) return(region)
    }
    return(NA)
  }
  
  extract_ethnicity <- function(colname) {
    if (grepl("Total", colname, fixed = TRUE)) return("Total")
    else if (grepl("Hispanic", colname, fixed = TRUE)) return("Hispanic")
    else if (grepl("Non Hispanic", colname, fixed = TRUE)) return("Non Hispanic")
    else return(NA)
  }
  
  region_tags <- sapply(colnames(df), extract_region)
  ethnicity_tags <- sapply(colnames(df), extract_ethnicity)
  
  region_factor <- factor(region_tags, levels = regions_order)
  ethnicity_factor <- factor(ethnicity_tags, levels = c("Total", "Hispanic", "Non Hispanic"))
  
  ordered_cols <- colnames(df)[order(region_factor, ethnicity_factor)]
  
  df <- df[, ordered_cols]
  
  # Define components to select
  indicators <- c(
    "Population 16+",
    "16+ In Civilian Labor Force",
    "16+ In Labor Force",
    "16+ Civilian Employed",
    "16+ Employed"
  )
  
  ethnicities <- c("Total", "Hispanic", "Non Hispanic")
  regions <- regions_order
  
  # Build ordered column names
  ordered_cols_subset <- c()
  for (region in regions) {
    for (eth in ethnicities) {
      for (ind in indicators) {
        col_name <- paste(eth, ind, "-", region)
        ordered_cols_subset <- c(ordered_cols_subset, col_name)
      }
    }
  }
  
  # Select only columns that exist in df
  df <- df %>%
    select(year, any_of(ordered_cols_subset))
  
  return(df)
}

# Apply to both datasets:
final_result_1yr <- process_final_result(final_result_1yr)
final_result_5yr <- process_final_result(final_result_5yr)

#######################	
#--- Save to Excel ---#
#######################
#--- Create or load workbook ---#
save_data_and_metadata <- function(file_path, data_sheet_name, data_df, metadata_df) {
  # Load or create workbook
  wb <- if (file.exists(file_path)) wb_load(file_path) else wb_workbook()
  
  # Helper to remove sheet if exists
  remove_if_exists <- function(sheet) {
    if (sheet %in% wb_get_sheet_names(wb)) {
      wb$remove_worksheet(sheet)
    }
  }
  
  # Remove old data and metadata sheets
  remove_if_exists(data_sheet_name)
  remove_if_exists("metadata")
  
  # Add new sheets with data
  wb$add_worksheet(data_sheet_name)
  wb$add_data(sheet = data_sheet_name, x = data_df, start_row = 1, col_names = TRUE)
  
  wb$add_worksheet("metadata")
  wb$add_data(sheet = "metadata", x = metadata_df, start_row = 1, col_names = TRUE)
  
  # Save workbook
  wb$save(file = file_path)
}
save_data_and_metadata(file_path_1yr, sheet_name_1yr, final_result_1yr, metadata_1yr_all)
save_data_and_metadata(file_path_5yr, sheet_name_5yr, final_result_5yr, metadata_5yr_all)


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------