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

weather_data <- data.table(read_dta(paste0(wd, input_path, "era5_land_weather_buffers_all_2010_2022.dta")))
#weather_data <- head(weather_data, 300000)
weather_data$date <- as.Date(as.character(weather_data$readable_date_era5), format = "%Y-%m-%d")

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
weather_data[, water_balance_10 := total_precipitation_sum_10 + potential_evaporation_sum_10]
weather_data[, water_balance_25 := total_precipitation_sum_25 + potential_evaporation_sum_25]
weather_data[, water_balance_50 := total_precipitation_sum_50 + potential_evaporation_sum_50]
weather_data[, water_balance_100 := total_precipitation_sum_100 + potential_evaporation_sum_100]
# Create the year-month column
weather_data[, year_month := format(date, "%Y-%m")]
# Create monthly dataset for SPEI package
# Define a list of water balance variables
water_balance_vars <- c("water_balance_10", "water_balance_25", "water_balance_50", "water_balance_100")

# Calculate monthly water balances for each water_balance variable
monthly_weather_data <- lapply(water_balance_vars, function(var) {
  # Calculate monthly water balance and add a column for the variable name
  weather_data[, .(monthly_water_balance = sum(get(var), na.rm = TRUE)), 
               by = .(id_coordinates_num, year_month, interview_date)][, .(id_coordinates_num, year_month, interview_date, water_balance_type = var, monthly_water_balance)]
})

# Combine results into a single data.table
temp <- rbindlist(monthly_weather_data)

# Cast to wide format, with each water balance variable as a separate column
monthly_weather_data <- dcast(temp, id_coordinates_num + year_month + interview_date ~ water_balance_type, 
                                   value.var = "monthly_water_balance")

# Clean up temporary variables
rm(temp)


# =============================================================================
# 2.3) Calculate SPEI for one LGA 
# =============================================================================
# Define the list of water balance variables and timescales for SPEI calculation
water_balance_vars <- c("water_balance_10", "water_balance_25", "water_balance_50", "water_balance_100")
spei_scales <- c(1, 3, 6, 12, 60)

# Example for one district (filtering by lga_code)
id_data <- monthly_weather_data[id_coordinates_num == 1]

# Ensure the data is sorted by date
id_data <- id_data[order(year_month)]

# Loop over each water balance variable to calculate SPEI at different timescales
for (balance_var in water_balance_vars) {
  
  # Create time series object for the current water balance variable
  water_balance_ts <- ts(id_data[[balance_var]], start = c(2010, 1), frequency = 12)
  
  # Calculate SPEI for each timescale and add to id_data
  for (scale in spei_scales) {
    spei_result <- spei(water_balance_ts, scale = scale)
    spei_values <- spei_result$fitted
    
    # Create a column name for each combination of balance variable and SPEI scale
    col_name <- paste0(balance_var, "_spei_", scale, "m")
    
    # Add SPEI values to id_data
    id_data[[col_name]] <- spei_values
  }
}

# Convert year_month to Date format for proper plotting
id_data[, year_month := as.Date(paste0(year_month, "-01"))]


# Plot the SPEI for LGA 14 over time
ggplot(id_data, aes(x = year_month)) +
  geom_area(aes(y = water_balance_10_spei_12m, fill = "SPEI 12-Months"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = water_balance_10_spei_6m, color = "SPEI 6-Months"), size = 0.1) +  # Define color legend for SPEI 6-month
  geom_line(aes(y = water_balance_10_spei_3m, color = "SPEI 3-Months"), size = 0.1) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = water_balance_10_spei_1m, color = "SPEI 1-Month"), size = 0.1) + 
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

ggplot(id_data, aes(x = year_month)) +
  geom_area(aes(y = water_balance_10_spei_12m, fill = "SPEI 12-Months"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = water_balance_10_spei_6m, color = "SPEI 6-Months"), size = 0.6) +  # Define color legend for SPEI 6-month
  geom_line(aes(y = water_balance_10_spei_3m, color = "SPEI 3-Months"), size = 0.6) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = water_balance_10_spei_1m, color = "SPEI 1-Month"), size = 0.6) + 
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
# Convert interview_date to Date format, extracting unique dates
survey_date <- as.Date(as.character(substr(unique(id_data$interview_date), 1, 10)), format = "%Y-%m-%d")

# Apply rounding based on whether the day of the month is before or after the 15th
survey_date <- ifelse(day(survey_date) > 15, 
                      as.Date(format(survey_date + months(1), "%Y-%m-01")),  # Round up to the 1st of the next month
                      as.Date(format(survey_date, "%Y-%m-01")))             # Round down to the 1st of the current month

survey_date <- as.Date(survey_date, origin = "1970-01-01")

# Also calculate 12 months leading up to the survey for 1-month SPEI
months_before_survey <- seq(survey_date, by = "-1 month", length.out = 13)


# Filter for the dates of interest
id_data_filtered <- id_data[year_month %in% c(months_before_survey)]

# Create columns for 1-month SPEI for 12 months leading up to the survey
for (i in 1:12) {
  col_name <- paste0("spei_1m_", i, "_month_pre_survey21")
  id_data_filtered[, (col_name) := spei_1m[year_month == (survey_date - months(i))]]
}

id_data_filtered <- id_data_filtered[year_month %in% survey_date]


# =============================================================================
# 3.2) Create SPEI table for one ID for all survey dates
# =============================================================================

# Define all survey dates
# Convert interview_date to Date format, extracting unique dates
survey_dates <- unique(as.Date(substr(id_data$interview_date, 1, 10), format = "%Y-%m-%d"))

# Apply rounding based on whether the day of the month is before or after the 15th for each unique date
survey_dates <- sapply(survey_dates, function(date) {
  if (day(date) > 15) {
    as.Date(format(date + months(1), "%Y-%m-01"))  # Round up to the 1st of the next month
  } else {
    as.Date(format(date, "%Y-%m-01"))              # Round down to the 1st of the current month
  }
})

survey_dates <- as.Date(survey_dates, origin = "1970-01-01")

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
    col_name <- paste0("buffer_10_spei_1m_", i, "_month_pre_survey_", survey_year)
    id_data[, (col_name) := water_balance_10_spei_1m[year_month == (survey_date - months(i))]]
  }

for (i in 1:12) {
  col_name <- paste0("buffer_25_spei_1m_", i, "_month_pre_survey_", survey_year)
  id_data[, (col_name) := water_balance_25_spei_1m[year_month == (survey_date - months(i))]]
}

for (i in 1:12) {
  col_name <- paste0("buffer_50_spei_1m_", i, "_month_pre_survey_", survey_year)
  id_data[, (col_name) := water_balance_50_spei_1m[year_month == (survey_date - months(i))]]
}

for (i in 1:12) {
  col_name <- paste0("buffer_100_spei_1m_", i, "_month_pre_survey_", survey_year)
  id_data[, (col_name) := water_balance_100_spei_1m[year_month == (survey_date - months(i))]]
}
rm(survey_date, survey_year, months_before_survey, col_name)

}

# Filter lga_data for survey dates (just to keep the survey data in the final table)
id_data_filtered <- id_data[year_month %in% survey_dates]


# =============================================================================
# 3.3) Create SPEI table for all ID for all survey dates
# =============================================================================

## First step: calculate all the SPEI values

# Define the list of water balance variables and timescales for SPEI calculation
water_balance_vars <- c("water_balance_10", "water_balance_25", "water_balance_50", "water_balance_100")
spei_scales <- c(1, 3, 6, 12, 60)

# Get a vector of unique LGAs
id_codes <- unique(monthly_weather_data$id_coordinates_num)
full_id_data <- data.table()

# Loop over each LGA to calculate SPEI values
for (id in id_codes) {
  
  # Example for one district (filtering by lga_code)
  id_data <- monthly_weather_data[id_coordinates_num == id]
  
  # Ensure the data is sorted by date
  id_data <- id_data[order(year_month)]
  
  # Loop over each water balance variable to calculate SPEI at different timescales
  for (balance_var in water_balance_vars) {
    
    # Create time series object for the current water balance variable
    water_balance_ts <- ts(id_data[[balance_var]], start = c(2010, 1), frequency = 12)
    
    # Calculate SPEI for each timescale and add to id_data
    for (scale in spei_scales) {
      spei_result <- spei(water_balance_ts, scale = scale)
      spei_values <- spei_result$fitted
      
      # Create a column name for each combination of balance variable and SPEI scale
      col_name <- paste0(balance_var, "_spei_", scale, "m")
      
      # Add SPEI values to id_data
      id_data[[col_name]] <- spei_values
    }
  }
  
  # Convert year_month to Date format for proper plotting
  id_data[, year_month := as.Date(paste0(year_month, "-01"))]
  
  # Append the current LGA data (with SPEI values) to the combined data.table
  full_id_data <- rbind(full_id_data, id_data, fill = TRUE)
}

## Step 2: Filter and create wide dataset

# Initialize an empty data.table to store the results
spei_dt <- data.table()
id_codes <- unique(full_id_data$id_coordinates_num)

# Loop over each LGA
for (id in id_codes) {
  
  # Filter the data for the current LGA
  id_filtered_data <- full_id_data[id_coordinates_num == id]
  
  # Define all survey dates
  # Convert interview_date to Date format, extracting unique dates
  survey_dates <- unique(as.Date(substr(id_filtered_data$interview_date, 1, 10), format = "%Y-%m-%d"))
  
  # Apply rounding based on whether the day of the month is > 28, > 15, or <= 15
  survey_dates <- sapply(survey_dates, function(date) {
    if (day(date) > 28) {
      as.Date(format(date - days(5) + months(1), "%Y-%m-01"))  # Round up to the 1st of the next month
    } else if (day(date) > 15) {
      as.Date(format(date + months(1), "%Y-%m-01"))  # Round up to the 1st of the next month
    } else {
      as.Date(format(date, "%Y-%m-01"))              # Round down to the 1st of the current month
    }
  })
  
  survey_dates <- as.Date(survey_dates, origin = "1970-01-01")
  
  # Loop through each survey date
  for (survey_date in survey_dates) {
    
    # Convert the survey_date to Date format (to ensure it remains a date in the loop)
    survey_date <- as.Date(survey_date, origin = "1970-01-01")
    
    print(paste0("working on id ",id," for date ",survey_date))
    
    # Create a variable for the year (used in the column names)
    survey_year <- format(survey_date, "%Y")
    
    # Calculate 12 months leading up to the survey for 1-month SPEI
    months_before_survey <- seq(survey_date, by = "-1 month", length.out = 13)
    
    # Create columns for 1-month SPEI for 12 months leading up to each survey
    for (i in 1:12) {
      col_name <- paste0("buffer_10_spei_1m_", i, "_month_pre_survey_", survey_year)
      id_filtered_data[, (col_name) := water_balance_10_spei_1m[year_month == (survey_date - months(i))]]
    }
    
    for (i in 1:12) {
      col_name <- paste0("buffer_25_spei_1m_", i, "_month_pre_survey_", survey_year)
      id_filtered_data[, (col_name) := water_balance_25_spei_1m[year_month == (survey_date - months(i))]]
    }
    
    for (i in 1:12) {
      col_name <- paste0("buffer_50_spei_1m_", i, "_month_pre_survey_", survey_year)
      id_filtered_data[, (col_name) := water_balance_50_spei_1m[year_month == (survey_date - months(i))]]
    }
    
    for (i in 1:12) {
      col_name <- paste0("buffer_100_spei_1m_", i, "_month_pre_survey_", survey_year)
      id_filtered_data[, (col_name) := water_balance_100_spei_1m[year_month == (survey_date - months(i))]]
    }
    rm(survey_date, survey_year, months_before_survey, col_name)
    
  }
  
  # Filter lga_filtered_data for relevant survey dates to keep in the final table
  id_drop_irrelevant_dates <- id_filtered_data[year_month %in% survey_dates]
  
  # Append the current LGA data (with SPEI values) to the combined data.table
  spei_dt <- rbind(spei_dt, id_drop_irrelevant_dates, fill = TRUE)
  
}

# Create the folder if it doesn't exist
if (!dir.exists(file.path(getwd(), "data"))) {
  dir.create(file.path(getwd(), "data"))
}

write.csv(spei_dt, paste0(wd, data_path, "spei_id_dt (wide-format)",".csv"), row.names = FALSE, na = "")

