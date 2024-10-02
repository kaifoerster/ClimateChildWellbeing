#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Impact of drought spells and extreme heat days on child well-being and development >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and creating treatment variables---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) x
#                  3) x
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

###

# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "haven", "SPEI", "ggplot2") 

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

weather_data <- data.table(read_dta("era5_land_daily_mean_LGA_Nigeria_raw_anonym.dta"))
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

# ==============================================================================

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

# Example for one district (filtering by lga_code)
lga_data <- monthly_weather_data[lga_code_anonym == 14]

# Ensure the data is sorted by date
lga_data <- lga_data[order(year_month)]

# Create time series object for water balance
water_balance_ts <- ts(lga_data$monthly_water_balance, start = c(1981, 1), frequency = 12)

# Calculate SPEI for a 3-month timescale
spei_1_months <- spei(water_balance_ts, scale = 1)
spei_3_months <- spei(water_balance_ts, scale = 3)
spei_12_months <- spei(water_balance_ts, scale = 12)

# The output is an object containing the SPEI values and other information
spei_1values <- spei_1_months$fitted
spei_values <- spei_3_months$fitted
spei_12values <- spei_12_months$fitted
# Add the SPEI values back to the original data (ensure it matches the date order)
lga_data[, spei_1_month := spei_1values]
lga_data[, spei_3_month := spei_values]
lga_data[, spei_12_month := spei_12values]

# Convert year_month to Date format for proper plotting
lga_data[, year_month := as.Date(paste0(year_month, "-01"))]

# Plot the SPEI for LGA 14 over time
ggplot(lga_data, aes(x = year_month)) +
  geom_area(aes(y = spei_12_month, fill = "SPEI 12-Month"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = spei_3_month, color = "SPEI 3-Month"), size = 0.1) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = spei_1_month, color = "SPEI 1-Month"), size = 0.1) + 
  labs(title = "SPEI (1/3/12 months) for LGA 14 (Jan 2020 – Dec 2021)",
       x = "Date",
       y = "SPEI") +
  scale_fill_manual(values = c("SPEI 12-Month" = "red"), name = "Legend") +  # Customize the fill legend
  scale_color_manual(values = c("SPEI 3-Month" = "blue", "SPEI 1-Month" = "green"), name = "Legend") +  # Customize the color legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top",  # Position the legend on top of the graph
        legend.direction = "horizontal",  # Align the legend items horizontally
        legend.title = element_blank(),  # Optionally remove the legend title if not needed
        legend.background = element_blank())  # Remove the background around the legend for a cleaner look

# Plot the SPEI for LGA 14 between 2020-2021 with suvery date as red vertical line

ggplot(lga_data, aes(x = year_month)) +
  geom_area(aes(y = spei_12_month, fill = "SPEI 12-Month"), alpha = 0.4) +  # Define fill legend for SPEI 12-month
  geom_line(aes(y = spei_3_month, color = "SPEI 3-Month"), size = 0.6) +  # Define color legend for SPEI 3-month
  geom_line(aes(y = spei_1_month, color = "SPEI 1-Month"), size = 0.6) + 
  geom_vline(xintercept = as.Date("2021-10-01"), linetype = "solid", color = "red", size = 1) +  # Mark 1st Oct 2021
  labs(title = "SPEI (1/3/12 months) for LGA 14 (Jan 2020 – Dec 2021)",
       x = "Date",
       y = "SPEI") +
  scale_x_date(limits = c(as.Date("2020-01-01"), as.Date("2021-12-31")), date_labels = "%b %Y") +  # Set time range and date labels
  scale_fill_manual(values = c("SPEI 12-Month" = "red"), name = "Legend") +  # Customize the fill legend
  scale_color_manual(values = c("SPEI 3-Month" = "blue", "SPEI 1-Month" = "green"), name = "Legend") +  # Customize the color legend
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "top",  # Position the legend on top of the graph
        legend.direction = "horizontal",  # Align the legend items horizontally
        legend.title = element_blank(),  # Optionally remove the legend title if not needed
        legend.background = element_blank())  # Remove the background around the legend for a cleaner look


# Loop over unique LGA codes
for (lga in unique(weather_data$lga_code_anonym)) {
  lga_data <- weather_data[lga_code_anonym == lga]
  lga_data <- lga_data[order(date)]
  
  # Create time series for water balance
  water_balance_ts <- ts(lga_data$water_balance, start = c(1981, 1), frequency = 12)
  
  # Calculate SPEI
  spei_3_months <- spei(water_balance_ts, scale = 3)
  
  # Store SPEI values
  weather_data[lga_code_anonym == lga, spei_3_month := spei_3_months$fitted]
}


