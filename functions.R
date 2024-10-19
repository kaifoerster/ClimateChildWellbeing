# Function to count the number of events within a specified radius
count_events_in_radius <- function(hh_sf, int_date, radius, time_frame_start, time_frame_end) {
  # Filter events by time frame
  filtered_ACLED <- ACLED_sf[ACLED_sf$date >= int_date - time_frame_end & 
                               ACLED_sf$date <= int_date - time_frame_start, ]
  
  # Calculate distances (in meters) between the household and all filtered events
  distances <- st_distance(hh_sf, filtered_ACLED)
  
  # Convert radius to meters (e.g., 10km = 10000 meters)
  radius_meters <- radius * 1000
  
  # Create a vector of counts of events within the radius for each household
  n_events <- apply(distances, 1, function(x) sum(x <= radius_meters))
  
  return(n_events)
}


count_events_in_radius_boko <- function(hh_sf, int_date, radius, time_frame_start, time_frame_end) {
  # Filter events by time frame
  filtered_ACLED <- ACLED_boko_haram[ACLED_boko_haram$date >= int_date - time_frame_end & 
                                       ACLED_boko_haram$date <= int_date - time_frame_start, ]
  
  # Calculate distances (in meters) between the household and all events
  distances <- st_distance(hh_sf, filtered_ACLED)
  
  # Convert radius to meters (e.g., 10km = 10000 meters)
  radius_meters <- radius * 1000
  
  # Count the number of events within the radius
  n_events <- apply(distances, 1, function(x) sum(x <= radius_meters))
  
  return(n_events)
}

count_events_in_radius_fulani <- function(hh_sf, int_date, radius, time_frame_start, time_frame_end) {
  # Filter events by time frame
  filtered_ACLED <- ACLED_fulani[ACLED_fulani$date >= int_date - time_frame_end & 
                                   ACLED_fulani$date <= int_date - time_frame_start, ]
  
  # Calculate distances (in meters) between the household and all events
  distances <- st_distance(hh_sf, filtered_ACLED)
  
  # Convert radius to meters (e.g., 10km = 10000 meters)
  radius_meters <- radius * 1000
  
  # Count the number of events within the radius
  n_events <- apply(distances, 1, function(x) sum(x <= radius_meters))
  
  return(n_events)
}
