# --------------------------------------------------------------------------- #
# # ACS_Portal_Homeownership.R: Time-Series 1yr and 5yr ACS PULL              #
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
packages <- c("tidycensus", "tidyverse", "httr", "jsonlite", "openxlsx2", "dplyr","stringr","purrr")

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
file_path_1yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_Homeownership_1yr.xlsx"
file_path_5yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_Homeownership_5yr.xlsx"
sheet_name_1yr <- "homeownership_Latino_1yr"
sheet_name_5yr <- "homeownership_Latino_5yr"

#-----------------------------------------------------------------------------------------------------------------------

###############################################
##--- VARIABLE CODE SPECIFICATION BY YEAR ---##
###############################################

#-- Table S2502 -- # 

# Assign the variable mnemonics by year (variables can change from year to year) 
S2502_vars_by_year <- list(
  "2010" = c("S2502_C01_001E","S2502_C02_001E"),
  "2011" = c("S2502_C01_001E","S2502_C02_001E"),
  "2012" = c("S2502_C01_001E","S2502_C02_001E"),
  "2013" = c("S2502_C01_001E","S2502_C02_001E"),
  "2014" = c("S2502_C01_001E","S2502_C02_001E"),
  "2015" = c("S2502_C01_001E","S2502_C02_001E"),
  "2016" = c("S2502_C01_001E","S2502_C02_001E"),
  "2017" = c("S2502_C01_001E","S2502_C03_001E"),
  "2018" = c("S2502_C01_001E","S2502_C03_001E"),
  "2019" = c("S2502_C01_001E","S2502_C03_001E"),
  "2021" = c("S2502_C01_001E","S2502_C03_001E"),
  "2022" = c("S2502_C01_001E","S2502_C03_001E"),
  "2023" = c("S2502_C01_001E","S2502_C03_001E")
)

# Manually rename variables (WORK ON AUTOMATING THIS IN THE FUTURE)
S2502_vars_remap <- list(
  "2010" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2011" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2012" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2013" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2014" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2015" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2016" = c("S2502_C01_001E" = "Total Occupied", "S2502_C02_001E" = "Owner Occupied"),
  "2017" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied"),
  "2018" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied"),
  "2019" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied"),
  "2021" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied"),
  "2022" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied"),
  "2023" = c("S2502_C01_001E" = "Total Occupied", "S2502_C03_001E" = "Owner Occupied")
)

#-- Table B25003i -- # 

B25003I_vars_by_year <- list(
  "2010" = c("B25003I_001E", "B25003I_002E"),
  "2011" = c("B25003I_001E", "B25003I_002E"),
  "2012" = c("B25003I_001E", "B25003I_002E"),
  "2013" = c("B25003I_001E", "B25003I_002E"),
  "2014" = c("B25003I_001E", "B25003I_002E"),
  "2015" = c("B25003I_001E", "B25003I_002E"),
  "2016" = c("B25003I_001E", "B25003I_002E"),
  "2017" = c("B25003I_001E", "B25003I_002E"),
  "2018" = c("B25003I_001E", "B25003I_002E"),
  "2019" = c("B25003I_001E", "B25003I_002E"),
  "2021" = c("B25003I_001E", "B25003I_002E"),
  "2022" = c("B25003I_001E", "B25003I_002E"),
  "2023" = c("B25003I_001E", "B25003I_002E")
)

B25003I_vars_remap <- list(
  "2010" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2011" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2012" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2013" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2014" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2015" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2016" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2017" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2018" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2019" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2021" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2022" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied"),
  "2023" = c("B25003I_001E" = "Hispanic Total Occupied", "B25003I_002E" = "Hispanic Owner Occupied")
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
  imap_dfr(S2502_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs1")),
  imap_dfr(B25003I_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs1")),
)

metadata_5yr_all <- bind_rows(
  imap_dfr(S2502_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs5")),
  imap_dfr(B25003I_vars_by_year, ~ get_metadata_for_year(.x, .y, survey = "acs5")),
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
  S2502 = list(vars_by_year = S2502_vars_by_year, remap_by_year = S2502_vars_remap),
  B25003I = list(vars_by_year = B25003I_vars_by_year, remap_by_year = B25003I_vars_remap)
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

###########################################################################
# Run This Chunk of code and then copy and paste the output into the R script
for (region in regions) {
  region_clean <- str_remove(region, " County")
  
  total_occupied     <- paste0("`Total Occupied - ", region, "`")
  hispanic_total     <- paste0("`Hispanic Total Occupied - ", region, "`")
  owner_occupied     <- paste0("`Owner Occupied - ", region, "`")
  hispanic_owner     <- paste0("`Hispanic Owner Occupied - ", region, "`")
  
  non_latino_total   <- paste0("`Non-Latino Total Occupied - ", region, "`")
  non_latino_owner   <- paste0("`Non-Latino Owner Occupied - ", region, "`")
  
  cat(paste0(non_latino_total, " = ", total_occupied, " - ", hispanic_total, ",\n"))
  cat(paste0(non_latino_owner, " = ", owner_occupied, " - ", hispanic_owner, ",\n\n"))
}
###########################################################################
# Paste Below

#--- Derive Non-Latino Data ---#
add_non_latino_columns <- function(df) {
  df %>%
    mutate(
      `Non-Latino Total Occupied - United States` = `Total Occupied - United States` - `Hispanic Total Occupied - United States`,
      `Non-Latino Owner Occupied - United States` = `Owner Occupied - United States` - `Hispanic Owner Occupied - United States`,
      
      `Non-Latino Total Occupied - California` = `Total Occupied - California` - `Hispanic Total Occupied - California`,
      `Non-Latino Owner Occupied - California` = `Owner Occupied - California` - `Hispanic Owner Occupied - California`,
      
      `Non-Latino Total Occupied - Los Angeles County` = `Total Occupied - Los Angeles County` - `Hispanic Total Occupied - Los Angeles County`,
      `Non-Latino Owner Occupied - Los Angeles County` = `Owner Occupied - Los Angeles County` - `Hispanic Owner Occupied - Los Angeles County`,
      
      `Non-Latino Total Occupied - Ventura County` = `Total Occupied - Ventura County` - `Hispanic Total Occupied - Ventura County`,
      `Non-Latino Owner Occupied - Ventura County` = `Owner Occupied - Ventura County` - `Hispanic Owner Occupied - Ventura County`
    )
}

final_result_1yr <- add_non_latino_columns(final_result_1yr)
final_result_5yr <- add_non_latino_columns(final_result_5yr)


##########################
### Output formatting ###	
##########################

#--- Re-Order Columns #---
reorder_columns <- function(df, regions) {
  metrics <- c(
    "Total Occupied", "Owner Occupied", 
    "Hispanic Total Occupied", "Hispanic Owner Occupied",
    "Non-Latino Total Occupied", "Non-Latino Owner Occupied"
  )
  
  ordered_columns <- c("year", unlist(lapply(regions, function(r) {
    paste0(metrics, " - ", r)
  })))
  
  df %>%
    select(all_of(ordered_columns)) %>%
    arrange(year)
}

final_result_1yr <- reorder_columns(final_result_1yr, regions)
final_result_5yr <- reorder_columns(final_result_5yr, regions)


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
