#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Impact of drought spells and extreme heat days on child well-being and development >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and creating ACLED treatment variables---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Load MICS and ACLED dataset
#                  3) Create ACLED treatment variables
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
pckg_to_load <- c("data.table", "haven", "ggplot2", "lubridate", "sf", "tmap", "future.apply") 

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

MICS_data <- data.table(read_dta(paste0(wd, input_path, "Nigeria_MICS_5_6_cluster_level_gps_only_fin.dta")))
MICS_data[, date := substr(interview_date, 1, 10)]
MICS_data$date <- as.Date(as.character(MICS_data$date), format = "%Y-%m-%d")
MICS_data <- MICS_data[!is.na(date)] # drops one row which was probably an empty row 
MICS_data <- MICS_data[!is.na(gps_longitude)] # drops 673 HH where cluster could not be displaced in 10km radius

ACLED_data <- as.data.table(read.csv(paste0(wd, input_path,"ACLED.csv"), sep = ";"))
ACLED_data[, date := as.Date(event_date, format = "%d %B %Y")]


# ===========================================================================
#   3) Create ACLED treatment variables
# ===========================================================================

# Create sf objects for the coordinates in both datasets
MICS_sf <- st_as_sf(MICS_data, coords = c("gps_longitude", "gps_latitude"), crs = 4326)
ACLED_sf <- st_as_sf(ACLED_data, coords = c("longitude", "latitude"), crs = 4326)

# Transform to Web Mercator (EPSG 3857) for distance calculations
MICS_sf <- st_transform(MICS_sf, 3857)
ACLED_sf <- st_transform(ACLED_sf, 3857)

# check how it looks like on a map

# Set tmap to interactive mode (optional)
# tmap_mode("view")

# Create a hexbin map for ACLED data
# tm_shape(ACLED_sf) +
#   tm_bubbles(size = 1, col = "red", border.col = "darkred", 
#              scale = 2, alpha = 0.6, style = "quantile", 
#              title.size = "Event Density") +
#   tm_shape(MICS_sf) +
#   tm_bubbles(size = 1, col = "blue", border.col = "darkblue", 
#              scale = 2, alpha = 0.6, style = "quantile", 
#              title.size = "Household Density") +
#   tm_basemap("OpenStreetMap")

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
  "between_50_100km" = c(50, 100),
  "between_100_150km" = c(100, 150),
  "between_150_200km" = c(150, 200)
)



# Use 'multicore' for Linux/macOS or 'multisession' for Windows
plan(multisession)

# Initialize results and progress tracking
results <- list()
progress_file <- "progress_results.rds"
completed_ids <- c()

# Check if a progress file exists, and load it if it does
if (file.exists(progress_file)) {
  saved_data <- readRDS(progress_file)
  results <- saved_data$results
  completed_ids <- saved_data$completed_ids
}

# Iterate through each household
for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  cluster_id <- MICS_data[i, id_coordinates_num][1]  # Store cluster_id
  
  # Skip already completed households
  if (cluster_id %in% completed_ids) next
  
  hh_results <- list()
  hh_results$cluster_id <- cluster_id
  
  print(paste0("Running for household number: ",cluster_id))
  
  # Iterate through each time frame
  for (tf in names(time_frames)) {
    tf_start <- time_frames[[tf]][1]
    tf_end <- time_frames[[tf]][2]
    
    # Iterate through each distance band
    for (dist in names(distances)) {
      if (length(distances[[dist]]) == 1) {
        radius <- distances[[dist]]
        n_events <- count_events_in_radius(hh_sf, int_date, radius, tf_start, tf_end)
      } else {
        lower_radius <- distances[[dist]][1]
        upper_radius <- distances[[dist]][2]
        n_events <- count_events_in_radius(hh_sf, int_date, upper_radius, tf_start, tf_end) -
          count_events_in_radius(hh_sf, int_date, lower_radius, tf_start, tf_end)
      }
      
      hh_results[[paste0(tf, "_", dist)]] <- n_events
    }
  }
  
  # Store the results for the household
  results[[i]] <- hh_results
  
  # Add the household ID to completed_ids
  completed_ids <- c(completed_ids, cluster_id)
  
  # Save progress every N iterations (e.g., every 100 households)
  if (i %% 100 == 0) {
    saveRDS(list(results = results, completed_ids = completed_ids), progress_file)
  }
}

# Shut down parallel workers after the job is done
plan(sequential)

# Final save after the loop completes
saveRDS(list(results = results, completed_ids = completed_ids), progress_file)

# Convert results to data.table
results_dt <- rbindlist(lapply(results, as.data.table), fill = TRUE)

write.csv(results_dt, paste0(wd, data_path, "ACLED_cluster_all",".csv"), row.names = FALSE, na = "")
print("ACLED all treatment variables saved to ACLED_cluster_all.csv file")

rm(saved_data)
# ===========================================================================
#   3.2) Create table for Boku Haram events
# ===========================================================================


# Filter ACLED events for "Boko Haram" in actor1 or assoc_actor_1
boko_in_actor1 <- grepl("Boko Haram", ACLED_sf$actor1, ignore.case = TRUE)
boko_in_assoc_actor1 <- grepl("Boko Haram", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_boko_haram <- ACLED_sf[boko_in_actor1 | boko_in_assoc_actor1, ]


# Use 'multicore' for Linux/macOS or 'multisession' for Windows
plan(multisession)

# Initialize results and progress tracking
results_boko_haram <- list()
progress_file <- "progress_results_boko.rds"
completed_ids <- c()

# Check if a progress file exists, and load it if it does
if (file.exists(progress_file)) {
  saved_data <- readRDS(progress_file)
  results_boko_haram <- saved_data$results_boko_haram
  completed_ids <- saved_data$completed_ids
}

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  cluster_id <- MICS_data[i, id_coordinates_num][1]  # Store cluster_id
  
  # Skip already completed households
  if (cluster_id %in% completed_ids) next
  
  hh_results <- list()
  hh_results$cluster_id <- cluster_id
  
  print(paste0("Running for household number: ",cluster_id))
  
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
  
  # Add the household ID to completed_ids
  completed_ids <- c(completed_ids, cluster_id)
  
  # Save progress every N iterations (e.g., every 100 households)
  if (i %% 100 == 0) {
    saveRDS(list(results_boko_haram = results_boko_haram, completed_ids = completed_ids), progress_file)
  }
}

# Shut down parallel workers after the job is done
plan(sequential)

# Final save after the loop completes
saveRDS(list(results_boko_haram = results_boko_haram, completed_ids = completed_ids), progress_file)

# Convert to data.table
results_boko_haram_dt <- rbindlist(lapply(results_boko_haram, as.data.table), fill = TRUE)

write.csv(results_boko_haram_dt, paste0(wd, data_path, "ACLED_cluster_boko_haram",".csv"), row.names = FALSE, na = "")
print("ACLED Boko Haram treatment variables saved to ACLED_cluster_boko_haram.csv file")

rm(saved_data)
# ===========================================================================
#   3.3) Create table for Fulani events
# ===========================================================================


# Filter ACLED events for "Fulani" in actor1 or assoc_actor_1
fulani_in_actor1 <- grepl("Fulani", ACLED_sf$actor1, ignore.case = TRUE)
fulani_in_assoc_actor1 <- grepl("Fulani", ACLED_sf$assoc_actor_1, ignore.case = TRUE)

# Combine the conditions
ACLED_fulani <- ACLED_sf[fulani_in_actor1 | fulani_in_assoc_actor1, ]


# Use 'multicore' for Linux/macOS or 'multisession' for Windows
plan(multisession)

# Initialize results and progress tracking
results_fulani <- list()
progress_file <- "progress_results_fulani.rds"
completed_ids <- c()

# Check if a progress file exists, and load it if it does
if (file.exists(progress_file)) {
  saved_data <- readRDS(progress_file)
  results_fulani <- saved_data$results_fulani
  completed_ids <- saved_data$completed_ids
}

for (i in 1:nrow(MICS_sf)) {
  hh_sf <- MICS_sf[i, ]
  int_date <- MICS_data[i, date]
  cluster_id <- MICS_data[i, id_coordinates_num][1]  # Store cluster_id
  
  # Skip already completed households
  if (cluster_id %in% completed_ids) next
  
  hh_results <- list()
  hh_results$cluster_id <- cluster_id
  
  print(paste0("Running for household number: ",cluster_id))
  
  
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
  
  # Add the household ID to completed_ids
  completed_ids <- c(completed_ids, cluster_id)
  
  # Save progress every N iterations (e.g., every 100 households)
  if (i %% 100 == 0) {
    saveRDS(list(results_fulani = results_fulani, completed_ids = completed_ids), progress_file)
  }
}

# Shut down parallel workers after the job is done
plan(sequential)

# Final save after the loop completes
saveRDS(list(results_fulani = results_fulani, completed_ids = completed_ids), progress_file)

# Convert to data.table
results_fulani_dt <- rbindlist(lapply(results_fulani, as.data.table), fill = TRUE)

write.csv(results_fulani_dt, paste0(wd, data_path, "ACLED_cluster_fulani",".csv"), row.names = FALSE, na = "")
print("ACLED Fulani treatment variables saved to ACLED_cluster_fulani.csv file")
