#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Impact of drought spells and extreme heat days on child well-being and development >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and creating ACLED treatment variables---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) xx
#                  3) xx
# 
# 
# 
#
# Author: Kai Foerster
#
# Version date:	  19/10/2024
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
pckg_to_load <- c("data.table", "haven", "ggplot2", "lubridate", "sf", "tmap") 

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)

# Load functions
source("functions.R")

# ===========================================================================
#   2) Load MICS and ACLED dataset
# ===========================================================================

MICS_data <- data.table(read_dta(paste0(wd, input_path, "Nigeria_MICS_5_6_household_level_gps_only_fin.dta")))
MICS_data[, date := substr(interview_date, 1, 10)]
MICS_data$date <- as.Date(as.character(MICS_data$date), format = "%Y-%m-%d")
MICS_data <- MICS_data[!is.na(date)] # drops one row which was probably an empty row 
MICS_data <- MICS_data[!is.na(gps_longitude)] # drops 673 HH where cluster could not be displaced in 10km radius

ACLED_data <- as.data.table(read.csv(paste0(wd, input_path,"ACLED.csv"), sep = ";"))
ACLED_data[, date := as.Date(event_date, format = "%d %B %Y")]


# ===========================================================================
#   3) Calculate distances
# ===========================================================================

# Create sf objects for the coordinates in both datasets
MICS_sf <- st_as_sf(MICS_data, coords = c("gps_longitude", "gps_latitude"), crs = 4326)
ACLED_sf <- st_as_sf(ACLED_data, coords = c("longitude", "latitude"), crs = 4326)

# Transform to Web Mercator (EPSG 3857) for distance calculations
MICS_sf <- st_transform(MICS_sf, 3857)
ACLED_sf <- st_transform(ACLED_sf, 3857)

# check how it looks like on a map

# Set tmap to interactive mode (optional)
tmap_mode("view")

# Create a hexbin map for ACLED data
tm_shape(ACLED_sf) +
  tm_bubbles(size = 1, col = "red", border.col = "darkred", 
             scale = 2, alpha = 0.6, style = "quantile", 
             title.size = "Event Density") +
  tm_shape(MICS_sf) +
  tm_bubbles(size = 1, col = "blue", border.col = "darkblue", 
             scale = 2, alpha = 0.6, style = "quantile", 
             title.size = "Household Density") +
  tm_basemap("OpenStreetMap")

# ===========================================================================
#   3.1) Create table for all events
# ===========================================================================

# Define time frames (in days)
time_frames <- list(
  "past_6_months" = c(0, 6 * 30),    # 0 days to 6 months
  "past_12_months" = c(0, 12 * 30),  # 0 days to 12 months
  "past_5_years" = c(0, 5 * 365)     # 0 days to 5 years
)

# Define distance bands
distances <- list(
  "within_10km" = 10,
  "within_25km" = 25,
  "within_50km" = 50,
  "within_100km" = 100,
  "within_200km" = 200,
  "between_51_100km" = c(51, 100),
  "between_101_150km" = c(101, 150),
  "between_151_200km" = c(151, 200)
)

# Initialize an empty list to store results
results <- list()

# Iterate through each household
for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  hh_id <- MICS_data[i, id_hh][1] # store hh_id
  
  hh_results <- list()
  # Add the household ID to the results
  hh_results$hh_id <- hh_id
  
  # Iterate through each time frame
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    # Iterate through each distance band
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        # Single radius distance (e.g., 10km, 25km, etc.)
        radius <- distances[[dist]]
        n_events <- count_events_in_radius(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        # Distance range (e.g., 51-100km, 101-150km, etc.)
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      # Store the result
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  # Store the results for the household
  results[[i]] <- hh_results
}

# Convert results to data.table for easy viewing
results_dt <- rbindlist(lapply(results, as.data.table), fill = TRUE)



# ===========================================================================
#   3.2) Create table for Boku Haram events
# ===========================================================================


# Filter ACLED events for "Boko Haram" in actor1 or assoc_actor_1
boko_in_actor1 <- grepl("Boko Haram", ACLED_sf$actor1, ignore.case = TRUE)
boko_in_assoc_actor1 <- grepl("Boko Haram", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_boko_haram <- ACLED_sf[boko_in_actor1 | boko_in_assoc_actor1, ]


# Now run the same process as before using the `count_events_in_radius_boko` function
results_boko_haram <- list()

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  hh_id <- MICS_data[i, id_hh][1]  # Store hh_id
  
  hh_results <- list()
  hh_results$hh_id <- hh_id
  
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        radius <- distances[[dist]]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius_boko(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  results_boko_haram[[i]] <- hh_results
}

# Convert to data.table
results_boko_haram_dt <- rbindlist(lapply(results_boko_haram, as.data.table), fill = TRUE)


# ===========================================================================
#   3.2) Create table for Boku Haram events
# ===========================================================================


# Filter ACLED events for "Boko Haram" in actor1 or assoc_actor_1
boko_in_actor1 <- grepl("Boko Haram", ACLED_sf$actor1, ignore.case = TRUE)
boko_in_assoc_actor1 <- grepl("Boko Haram", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_boko_haram <- ACLED_sf[boko_in_actor1 | boko_in_assoc_actor1, ]


# Now run the same process as before using the `count_events_in_radius_boko` function
results_boko_haram <- list()

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  hh_id <- MICS_data[i, id_hh][1]  # Store hh_id
  
  hh_results <- list()
  hh_results$hh_id <- hh_id
  
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        radius <- distances[[dist]]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius_boko(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  results_boko_haram[[i]] <- hh_results
}

# Convert to data.table
results_boko_haram_dt <- rbindlist(lapply(results_boko_haram, as.data.table), fill = TRUE)

# ===========================================================================
#   3.2) Create table for Boku Haram events
# ===========================================================================


# Filter ACLED events for "Boko Haram" in actor1 or assoc_actor_1
boko_in_actor1 <- grepl("Boko Haram", ACLED_sf$actor1, ignore.case = TRUE)
boko_in_assoc_actor1 <- grepl("Boko Haram", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_boko_haram <- ACLED_sf[boko_in_actor1 | boko_in_assoc_actor1, ]


# Now run the same process as before using the `count_events_in_radius_boko` function
results_boko_haram <- list()

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  hh_id <- MICS_data[i, id_hh][1]  # Store hh_id
  
  hh_results <- list()
  hh_results$hh_id <- hh_id
  
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        radius <- distances[[dist]]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius_boko(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius_boko(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  results_boko_haram[[i]] <- hh_results
}

# Convert to data.table
results_boko_haram_dt <- rbindlist(lapply(results_boko_haram, as.data.table), fill = TRUE)


# ===========================================================================
#   3.3) Create table for Fulani events
# ===========================================================================


# Filter ACLED events for "Fulani" in actor1 or assoc_actor_1
fulani_in_actor1 <- grepl("Fulani", ACLED_sf$actor1, ignore.case = TRUE)
fulani_in_assoc_actor1 <- grepl("Fulani", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_fulani <- ACLED_sf[fulani_in_actor1 | fulani_in_assoc_actor1, ]


# Now run the same process as before using the `count_events_in_radius_boko` function
results_fulani <- list()

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  hh_id <- MICS_data[i, id_hh][1]  # Store hh_id
  
  hh_results <- list()
  hh_results$hh_id <- hh_id
  
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        radius <- distances[[dist]]
        n_events <- count_events_in_radius_fulani(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius_fulani(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius_fulani(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  results_fulani[[i]] <- hh_results
}

# Convert to data.table
results_fulani_dt <- rbindlist(lapply(results_fulani, as.data.table), fill = TRUE)


