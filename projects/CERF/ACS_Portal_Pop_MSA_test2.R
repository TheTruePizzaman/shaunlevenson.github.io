# --------------------------------------------------------------------------- #
# ACS_Portal_Pop_MSA.R: Time Series 1yr and 5yr ACS pull                        #
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
# a.N/A

###################################
#-------------Notes---------------#
###################################
# Need to make sure it works for both ACS1 and ACS5
# If it isn't fully generalizable, we need to make sure it is so that the functions can be directly pasted into labor markets, education, and homeownership
# so far has only been tested for LA Metro
# also need to rename the columns to be "x - LA Metro" or something so that they are usable in "final_results" objects from the other programs
# if we dont mind running different programs for metros, just leave it here and it shouldnt be a problem
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
# Run only once per machine
#census_api_key("3ce6f083bdafa6331368a608fa56f25c66cb1c28", overwrite=TRUE,install= TRUE)

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

######################
## Preliminary Work ##	
######################
#-- Table Focus --#
table_focus <- 2023  # This is the table you want to focus on, change as needed
#code to identify variable nmemonics, change table_focus year to manually search in R......
#subject_vars <- load_variables(table_focus, dataset = "acs1/subject")
#profile_vars <- load_variables(table_focus, dataset = "acs1/profile")
#detailed_vars <- load_variables(table_focus, dataset = "acs1")

# Specify the survey type, can be "acs1" for 1-year or "acs5" for 5-year data
survey = c("acs1","acs5") 

# Geographies to pull data for, if you want more geographies, you have to add them here
geo_list <- list(
  "United States" = list(geography = "us"),
  "California" = list(geography = "state", state = "CA"),
  "Los Angeles County" = list(geography = "county", state = "CA", county = "037"),
  "Ventura County" = list(geography = "county", state = "CA", county = "111")
)

# Change the geography based on your quality check specifications on the metadata tab
metadata_geo <- "us"

# ───────────────────────────────────────────────────────────────────────────────
# User-defined settings (edit these only) -------------------------------------
table_name    <- c("DP05", "DP03")            # 
survey_type   <- c("acs1", "acs5")           # Change to "acs5" for 5-year data
year_focus    <- 2010:2023           # Specify the years you want to focus on 
metro_filter  <- "Los Angeles"       # Filter for a specific metro area, can be a name or a GEOID

file_path_1yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_Pop_1yr.xlsx"
file_path_5yr <- "v:/CERF/Consulting/Latino_GDP_Info/Programming/R_Programming/Output/ACS_Portal_Pop_5yr.xlsx"
sheet_name_1yr <- "Pop_1yr"
sheet_name_5yr <- "Pop_5yr"
# ───────────────────────────────────────────────────────────────────────────────
###############################################
##--- VARIABLE CODE SPECIFICATION BY YEAR ---##
###############################################

#-- Table DP05 -- # 

# Get the variable CODES for the table from 2010 to 2023
DP05_vars_by_year <- list(
  "2005" = c("DP01_0001E", "DP01_0066E", "DP01_0071E"),
  "2006" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2007" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2008" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2009" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2010" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2011" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2012" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2013" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2014" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2015" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2016" = c("DP05_0001E", "DP05_0066E", "DP05_0071E"),
  "2017" = c("DP05_0001E", "DP05_0071E", "DP05_0076E"),
  "2018" = c("DP05_0001E", "DP05_0071E", "DP05_0076E"),
  "2019" = c("DP05_0001E", "DP05_0071E", "DP05_0076E"),
  "2021" = c("DP05_0001E", "DP05_0071E", "DP05_0076E"),
  "2022" = c("DP05_0001E", "DP05_0073E", "DP05_0078E"),
  "2023" = c("DP05_0001E", "DP05_0076E", "DP05_0081E")
)

DP05_vars_remap<- list(
  "2005" = c("DP01_0001E"="Total Population", "DP01_0066E"="Hispanic", "DP01_0071E"="Non-Hispanic"),
  "2006" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2007" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2008" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2009" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2010" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"), 
  "2011" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2012" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2013" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2014" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2015" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2016" = c("DP05_0001E"="Total Population", "DP05_0066E"="Hispanic", "DP05_0071E"="Non-Hispanic"),
  "2017" = c("DP05_0001E"="Total Population", "DP05_0071E"="Hispanic", "DP05_0076E"="Non-Hispanic"),
  "2018" = c("DP05_0001E"="Total Population", "DP05_0071E"="Hispanic", "DP05_0076E"="Non-Hispanic"),
  "2019" = c("DP05_0001E"="Total Population", "DP05_0071E"="Hispanic", "DP05_0076E"="Non-Hispanic"),
  "2021" = c("DP05_0001E"="Total Population", "DP05_0071E"="Hispanic", "DP05_0076E"="Non-Hispanic"),
  "2022" = c("DP05_0001E"="Total Population", "DP05_0073E"="Hispanic", "DP05_0078E"="Non-Hispanic"),
  "2023" = c("DP05_0001E"="Total Population", "DP05_0076E"="Hispanic", "DP05_0081E"="Non-Hispanic")
)


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

# ───────────────────────────────────────────────────────────────────────────────
# User-defined settings (edit these only) -------------------------------------
vars_by_year  <- list(DP05 = DP05_vars_by_year, DP03 = DP03_vars_by_year)   # <— add this line
remap_list    <- list(DP05 = DP05_vars_remap, DP03 = DP03_vars_remap)     # <— add this line
# ───────────────────────────────────────────────────────────────────────────────

get_metro_table_wide <- function(
    table_name,
    survey,
    years,
    metro_filter,
    remap_list,
    vars_by_year
) {
  fetch_year <- function(yr) {
    df <- tryCatch({
      get_acs(
        geography   = "cbsa",
        table       = table_name,
        year        = yr,
        survey      = survey,
        output      = "tidy",
        cache_table = TRUE
      )
    }, error = function(e) {
      warning("Year ", yr, " failed: ", e$message)
      return(NULL)
    })
    if (is.null(df)) return(NULL)
    
    # Filter to metro
    df <- if (nchar(metro_filter) == 5) {
      filter(df, GEOID == metro_filter)
    } else {
      filter(df, str_detect(NAME, metro_filter))
    }
    
    # Keep only variables defined for this year
    codes_E <- vars_by_year[[as.character(yr)]]
    codes   <- sub("E$", "", codes_E)
    df      <- filter(df, variable %in% codes)
    
    # Remap or default rename
    year_chr <- as.character(yr)
    if (!is.null(remap_list[[year_chr]])) {
      raw_map   <- remap_list[[year_chr]]
      clean_map <- setNames(raw_map, sub("E$", "", names(raw_map)))
      keep_map  <- intersect(names(clean_map), df$variable)
      df <- df %>% mutate(Variable = recode(variable, !!!clean_map[keep_map]))
    } else {
      df <- df %>% rename(Variable = variable)
    }
    
    df %>% transmute(
      Year     = yr,
      CBSA     = GEOID,
      MSA_Name = NAME,
      Variable,
      Estimate = estimate
    )
  }
  
  long_df <- map_dfr(years, fetch_year)
  if (nrow(long_df) == 0) return(long_df)
  
  long_df %>%
    # prefix each variable with "metro_filter MSA "
    mutate(Variable = paste0(metro_filter, " MSA ", Variable)) %>%
    pivot_wider(
      id_cols     = c(Year, CBSA, MSA_Name),
      names_from  = Variable,
      values_from = Estimate
    )
}


# Run it!
# 1-Year Pull
metro_1yr <- map_dfr(table_name, function(tbl) {
  get_metro_table_wide(
    table_name   = tbl,
    survey       = "acs1",
    years        = year_focus,
    metro_filter = metro_filter,
    remap_list   = remap_list[[tbl]],
    vars_by_year = vars_by_year[[tbl]]
  ) %>%
    mutate(Table = tbl)
})

metro_1yr <- metro_1yr %>%
  select(-Table) %>%
  group_by(Year, CBSA, MSA_Name) %>%
  summarise(
    across(
      .cols = where(is.numeric),        # all your DP05_*, DP03_* columns
      .fns  = ~ sum(.x, na.rm = TRUE)   # non-NA value will be the sum
    ),
    .groups = "drop"
  )
# 5-Year Pull
metro_5yr <- map_dfr(table_name, function(tbl) {
  get_metro_table_wide(
    table_name   = tbl,
    survey       = "acs5",
    years        = year_focus,
    metro_filter = metro_filter,
    remap_list   = remap_list[[tbl]],
    vars_by_year = vars_by_year[[tbl]]
  ) %>%
    mutate(Table = tbl)
})

metro_5yr <- metro_5yr %>%
  select(-Table) %>%
  group_by(Year, CBSA, MSA_Name) %>%
  summarise(
    across(
      .cols = where(is.numeric),        # all your DP05_*, DP03_* columns
      .fns  = ~ sum(.x, na.rm = TRUE)   # non-NA value will be the sum
    ),
    .groups = "drop"
  )