#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Impact of drought spells and extreme heat days on child well-being and development >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and creating SPEI treatment variables---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Load Google Earth Engine data and create SPEI treatment indicator
#                  3) Create SPEI treatment indicators for LGAs
# 
# 
# 
#
# Author: Kai Foerster
#
# Version date:	  29/09/2024
#=======================================================================================================



# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

data_path<- "/data/"
input_path <- "/input/"

# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "haven", "SPEI", "ggplot2", "lubridate") 

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)

# Load functions
#source("R/funcs/wrangling_function.R")

# ===========================================================================
#   2) Load Google Earth Engine data and create SPEI treatment indicator
# ===========================================================================

weather_data <- data.table(read_dta(paste0(wd, input_path, "era5_land_daily_mean_LGA_Nigeria_raw_anonym.dta")))
weather_data[, date := substr(systemindex, 1, 8)]
weather_data$date <- as.Date(as.character(weather_data$date), format = "%Y%m%d")

# =============================================================================
# 2.1) What is SPEI?
# =============================================================================

# SPEI (Standardized Precipitation-Evapotranspiration Index) is a multi-scalar drought index
# that incorporates both precipitation and potential evapotranspiration (PET) to assess drought
# severity. It improves upon other indices like SPI (Standardized Precipitation Index) by accounting
# for the water demand due to temperature, which affects evaporation. SPEI is calculated based on 
# the difference between precipitation and PET over various timescales (e.g., 1 month, 3 months, 12 months)
# to capture both short-term and long-term droughts.

# =============================================================================
# 2.2) Calculate water balance
# =============================================================================

# Calculate water balance: Precipitation - Evaporation
weather_data[, water_balance := total_precipitation_sum + potential_evaporation_sum]
# Create the year-month column
weather_data[, year_month := format(date, "%Y-%m")]
# Create monthly dataset for SPEI package
monthly_weather_data <- weather_data[, .(monthly_water_balance = sum(water_balance, na.rm = TRUE)), 
                             by = .(lga_code_anonym, year_month)]

# --------------------------------------------------------------------------------------------------
# Some checks
summary(weather_data$water_balance) # sensible results showing that Nigeria has water deficit, but there is variation between lga and over time.
summary(weather_data$potential_evaporation_sum)
# weird that there are positive evaporation values, so investigate
# Filter rows with positive PET values
positive_pet <- weather_data[potential_evaporation_sum > 0]

# Check the summary or distribution of these positive values
summary(positive_pet$potential_evaporation_sum)
rm(positive_pet)
# Seems like there are only 114 out of 11 Mio observations that are positive.
# This alleviates concerns, might mean anormal conditions like fog maybe? or measurement error.
# --------------------------------------------------------------------------------------------------

# =============================================================================
# 2.3) Calculate SPEI for one LGA 
# =============================================================================

# Example for one district (filtering by lga_code)
lga_data <- monthly_weather_data[lga_code_anonym == 14]

# Ensure the data is sorted by date
lga_data <- lga_data[order(year_month)]

# Create time series object for water balance
water_balance_ts <- ts(lga_data$monthly_water_balance, start = c(1981, 1), frequency = 12)

# Calculate SPEI for a 3-month timescale
spei_1_months <- spei(water_balance_ts, scale = 1)
spei_3_months <- spei(water_balance_ts, scale = 3)
spei_6_months <- spei(water_balance_ts, scale = 6)
spei_12_months <- spei(water_balance_ts, scale = 12)

# The output is an object containing the SPEI values and other information
spei_1values <- spei_1_months$fitted
spei_3values <- spei_3_months$fitted
spei_6values <- spei_6_months$fitted
spei_12values <- spei_12_months$fitted
# Add the SPEI values back to the original data (ensure it matches the date order)
lga_data[, spei_1m := spei_1values]
lga_data[, spei_3m := spei_3values]
lga_data[, spei_6m := spei_6values]
lga_data[, spei_12m := spei_12values]

# Convert year_month to Date format for proper plotting
lga_data[, year_month := as.Date(paste0(year_month, "-01"))]

# Plot the SPEI for LGA 14 over time
ggplot(lga_data, aes(x = year_month)) +
  geom_area(aes(y = spei_12m, fill = "SPEI 12-Months"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = spei_6m, color = "SPEI 6-Months"), size = 0.1) +  # Define color legend for SPEI 6-month
  geom_line(aes(y = spei_3m, color = "SPEI 3-Months"), size = 0.1) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = spei_1m, color = "SPEI 1-Month"), size = 0.1) + 
  geom_hline(yintercept = -1.5, linetype = "longdash", color = "black", size = 1) + 
  labs(title = "SPEI (1/3/6/12 months) for LGA 14 (Jan 2020 – Dec 2021)",
       x = "Date",
       y = "SPEI") +
  scale_fill_manual(values = c("SPEI 12-Months" = "red"), name = "Legend") +  # Customize the fill legend
  scale_color_manual(values = c("SPEI 6-Months" = "yellow", "SPEI 3-Months" = "blue", "SPEI 1-Month" = "green"), name = "Legend") +  # Customize the color legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top",  # Position the legend on top of the graph
        legend.direction = "horizontal",  # Align the legend items horizontally
        legend.title = element_blank(),  # Optionally remove the legend title if not needed
        legend.background = element_blank())  # Remove the background around the legend for a cleaner look

# Plot the SPEI for LGA 14 between 2020-2021 with suvery date as red vertical line

ggplot(lga_data, aes(x = year_month)) +
  geom_area(aes(y = spei_12m, fill = "SPEI 12-Months"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = spei_6m, color = "SPEI 6-Months"), size = 0.6) +  # Define color legend for SPEI 6-month
  geom_line(aes(y = spei_3m, color = "SPEI 3-Months"), size = 0.6) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = spei_1m, color = "SPEI 1-Month"), size = 0.6) + 
  geom_vline(xintercept = as.Date("2021-10-01"), linetype = "solid", color = "red", size = 1) +  # Mark 1st Oct 2021
  geom_hline(yintercept = -1.5, linetype = "longdash", color = "black", size = 1) + 
  labs(title = "SPEI (1/3/12 months) for LGA 14 (Jan 2020 – Dec 2021)",
       x = "Date",
       y = "SPEI") +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2021-12-31")), date_labels = "%b %Y") +  # Set time range and date labels
  scale_fill_manual(values = c("SPEI 12-Months" = "red"), name = "Legend") +  # Customize the fill legend
  scale_color_manual(values = c("SPEI 6-Months" = "yellow", "SPEI 3-Months" = "blue", "SPEI 1-Month" = "green"), name = "Legend") +  # Customize the color legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top",  # Position the legend on top of the graph
        legend.direction = "horizontal",  # Align the legend items horizontally
        legend.title = element_blank(),  # Optionally remove the legend title if not needed
        legend.background = element_blank())  # Remove the background around the legend for a cleaner look



# =============================================================================
# 3) Create SPEI treatment indicators for LGAs
# =============================================================================

# =============================================================================
# 3.1) Create SPEI table for one LGA 2021 survey date
# =============================================================================

# Define survey date
survey_date <- as.Date("2021-09-01")

# Also calculate 12 months leading up to the survey for 1-month SPEI
months_before_survey <- seq(survey_date, by = "-1 month", length.out = 13)


# Filter for the dates of interest
lga_data_filtered <- lga_data[year_month %in% c(months_before_survey)]

# Create columns for 1-month SPEI for 12 months leading up to the survey
for (i in 1:12) {
  col_name <- paste0("spei_1m_", i, "_month_pre_survey21")
  lga_data_filtered[, (col_name) := spei_1m[year_month == (survey_date - months(i))]]
}

lga_data_filtered <- lga_data_filtered[year_month %in% survey_date]


# =============================================================================
# 3.2) Create SPEI table for one LGA for all survey dates
# =============================================================================

# Define all survey dates
survey_dates <- as.Date(c("2021-09-01", "2016-09-01", "2011-09-01", "2007-09-01"))

# Loop through each survey date
for (survey_date in survey_dates) {
  
  # Convert the survey_date to Date format (to ensure it remains a date in the loop)
  survey_date <- as.Date(survey_date, origin = "1970-01-01")
  
  print(paste0("working on ",survey_date))
  
  # Create a variable for the year (used in the column names)
  survey_year <- format(survey_date, "%Y")
  
  # Calculate 12 months leading up to the survey for 1-month SPEI
  months_before_survey <- seq(survey_date, by = "-1 month", length.out = 13)
  
  # Create columns for 1-month SPEI for 12 months leading up to each survey
  for (i in 1:12) {
    col_name <- paste0("spei_1m_", i, "_month_pre_survey_", survey_year)
    lga_data[, (col_name) := spei_1m[year_month == (survey_date - months(i))]]
  }
  rm(survey_date, survey_year, months_before_survey, col_name)
}

# Filter lga_data for survey dates (just to keep the survey data in the final table)
lga_data_filtered <- lga_data[year_month %in% survey_dates]


# =============================================================================
# 3.3) Create SPEI table for all LGA for all survey dates
# =============================================================================

## First step: calculate all the SPEI values

# Get a vector of unique LGAs
lga_codes <- unique(monthly_weather_data$lga_code_anonym)
full_lga_data <- data.table()

# Loop over each LGA to calculate SPEI values
for (lga in lga_codes) {
  
  # Example for one district (filtering by lga_code)
  lga_data <- monthly_weather_data[lga_code_anonym == lga]
  
  # Ensure the data is sorted by date
  lga_data <- lga_data[order(year_month)]
  
  # Create time series object for water balance
  water_balance_ts <- ts(lga_data$monthly_water_balance, start = c(1981, 1), frequency = 12)
  
  # Calculate SPEI for a 3-month timescale
  spei_1_months <- spei(water_balance_ts, scale = 1)
  spei_3_months <- spei(water_balance_ts, scale = 3)
  spei_6_months <- spei(water_balance_ts, scale = 6)
  spei_12_months <- spei(water_balance_ts, scale = 12)
  
  # Add the SPEI values back to the original data (ensure it matches the date order)
  lga_data[, spei_1m := spei_1_months$fitted]
  lga_data[, spei_3m := spei_3_months$fitted]
  lga_data[, spei_6m := spei_6_months$fitted]
  lga_data[, spei_12m := spei_12_months$fitted]
  
  # Convert year_month to Date format for proper plotting
  lga_data[, year_month := as.Date(paste0(year_month, "-01"))]
  
  # Append the current LGA data (with SPEI values) to the combined data.table
  full_lga_data <- rbind(full_lga_data, lga_data, fill = TRUE)
}

## Step 2: Filter and create wide dataset

# Initialize an empty data.table to store the results
spei_dt <- data.table()
lga_codes <- unique(full_lga_data$lga_code_anonym)
# Define all survey dates
survey_dates <- as.Date(c("2021-10-01", "2016-10-01", "2011-10-01", "2007-10-01"))

# Loop over each LGA
for (lga in lga_codes) {
  
  # Filter the data for the current LGA
  lga_filtered_data <- full_lga_data[lga_code_anonym == lga]
  
  # Loop through each survey date
  for (i in 1:length(survey_dates)) {
    
    # Current survey date
    survey_date <- as.Date(survey_dates[i], origin = "1970-01-01")
    
    print(paste0("Working on LGA ", lga, " for survey date ", survey_date))
    
    # Create a variable for the year (used in the column names)
    survey_year <- format(survey_date, "%Y")
    
    # Calculate 12 months leading up to the survey for 1-month SPEI
    months_before_survey <- seq(survey_date, by = "-1 month", length.out = 13)
    
    # Create columns for 1-month SPEI for 12 months leading up to each survey
    for (j in 1:12) {
      col_name <- paste0("spei_1m_", j, "_month_pre_survey_", survey_year)
      
      # Assign SPEI values to the corresponding column
      lga_filtered_data[, (col_name) := spei_1m[year_month == (survey_date - months(j))]]
    }
    rm(col_name)
    
    # Use ifelse to set survey_interval_start: check if there's a previous date, else 4 years prior
    survey_interval_start <- as.Date(ifelse(i < 4, survey_dates[i + 1], survey_date - years(4)), origin = "1970-01-01")
    
    #Name column
    col_name <- paste0("spei_over_survey_interval", survey_year)
    
    # Calculate the mean SPEI over the survey interval period
    lga_filtered_data[, (col_name) := mean(spei_1m[year_month >= survey_interval_start & year_month <= survey_date], na.rm = TRUE)]
    
    # Drop column name
    rm(col_name)
    
  }
  # Filter lga_filtered_data for relevant survey dates to keep in the final table
  lga_drop_irrelevant_dates <- lga_filtered_data[year_month %in% survey_dates]
  
  # Append the current LGA data (with SPEI values) to the combined data.table
  spei_dt <- rbind(spei_dt, lga_drop_irrelevant_dates, fill = TRUE)
}

# Create the folder if it doesn't exist
if (!dir.exists(file.path(getwd(), "data"))) {
  dir.create(file.path(getwd(), "data"))
}

write.csv(spei_dt, paste0(wd, data_path, "spei_dt (wide-format)",".csv"), row.names = FALSE, na = "")

### Step 3: Turn into long format

spei_dt_long <- copy(spei_dt)

# Create a new column for "spei_over_survey_interval" that pulls from the correct year columns
spei_dt_long[, spei_over_survey_interval := fifelse(year_month == "2021-10-01", spei_over_survey_interval2021,
                                               fifelse(year_month == "2016-10-01", spei_over_survey_interval2016,
                                                       fifelse(year_month == "2011-10-01", spei_over_survey_interval2011,
                                                               fifelse(year_month == "2007-10-01", spei_over_survey_interval2007, NA_real_))))]

# Drop the redundant spei_over_survey_interval columns
spei_dt_long[, c("spei_over_survey_interval2021", 
            "spei_over_survey_interval2016", 
            "spei_over_survey_interval2011", 
            "spei_over_survey_interval2007") := NULL]

# Now do the same for each of the 1-month SPEI columns (e.g., spei_1m_1_month, spei_1m_2_month, ...)
for (i in 1:12) {
  col_name <- paste0("spei_1m_", i, "_month_pre_survey")  # New column name for each month
  
  # Dynamically create new column values based on year_month
  spei_dt_long[, (col_name) := fifelse(year_month == "2021-10-01", get(paste0("spei_1m_", i, "_month_pre_survey_2021")),
                                  fifelse(year_month == "2016-10-01", get(paste0("spei_1m_", i, "_month_pre_survey_2016")),
                                          fifelse(year_month == "2011-10-01", get(paste0("spei_1m_", i, "_month_pre_survey_2011")),
                                                  fifelse(year_month == "2007-10-01", get(paste0("spei_1m_", i, "_month_pre_survey_2007")), NA_real_)))) ]
}

# Drop the redundant spei_1m_*_pre_survey_* columns for all years
for (i in 1:12) {
  spei_dt_long[, c(paste0("spei_1m_", i, "_month_pre_survey_2021"),
              paste0("spei_1m_", i, "_month_pre_survey_2016"),
              paste0("spei_1m_", i, "_month_pre_survey_2011"),
              paste0("spei_1m_", i, "_month_pre_survey_2007")) := NULL]
}

write.csv(spei_dt_long, paste0(wd, data_path, "spei_dt (long-format)",".csv"), row.names = FALSE, na = "")

