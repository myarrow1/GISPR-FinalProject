
#===========================================================
#Import gtfs from Metro and ST, determine identity of routes operating between 9pm and 6AM
#===========================================================
library(tidyverse)
library(tidytransit) # R package tailored for GTFS processing
library(sf)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(gifski)

# 1. Define Feed URLs
urls <- list(
  KCM = "https://metro.kingcounty.gov/GTFS/google_transit.zip",
  ST  = "https://www.soundtransit.org/GTFS-rail/40_gtfs.zip"
)

gtfs_KCM <- tidytransit::read_gtfs(urls$KCM)
gtfs_ST <- tidytransit::read_gtfs(urls$ST)

head(gtfs_KCM$calendar)
head(gtfs_KCM$calendar_dates)

gtfs_KCM$calendar <- gtfs_KCM$calendar %>%
  mutate(
    start_date = ymd(as.character(start_date)),
    end_date   = ymd(as.character(end_date))
  )

gtfs_KCM$calendar_dates <- gtfs_KCM$calendar_dates %>%
  mutate(
    date = ymd(as.character(date))
  )

class(gtfs_KCM$calendar$start_date)

#=====================================================
#Create a new tidygtfs that has only necessary tables

kcm <- list(
  agency = gtfs_KCM$agency,
  stops = gtfs_KCM$stops,
  routes = gtfs_KCM$routes,
  trips = gtfs_KCM$trips,
  stop_times = gtfs_KCM$stop_times,
  calendar = gtfs_KCM$calendar,
  calendar_dates = gtfs_KCM$calendar_dates,
  shapes = gtfs_KCM$shapes
)

kcm <- tidytransit::as_tidygtfs(kcm)

validation_result <- attr(kcm, "validation_result")
head(validation_result)
#kcm <- tidytransit:::set_dates_services(kcm)
kcm <- tidytransit::set_servicepattern(kcm)
head(kcm$.$servicepatterns)
print(kcm$.$dates_servicepatterns, n = 90)
summary(kcm)


length(unique(kcm$trips$service_id)) # 13
length(unique(kcm$.$servicepatterns$servicepattern_id)) #38

#========================================================================================================
#service pattern plot
#adapted from: https://r-transit.github.io/tidytransit/articles/servicepatterns.html


# 1. Join dates with their service pattern definitions
date_servicepattern_table <- kcm$.$dates_servicepatterns %>% 
  left_join(kcm$.$servicepatterns, by = "servicepattern_id")

# 2. If you also need the raw calendar attributes (like start_date, end_date, 
# and weekly service flags) from kcm$calendar, join via service_id:
full_schedule_table <- date_servicepattern_table %>% 
  left_join(kcm$calendar, by = "service_id")

# 3. Build the table with the necessary weekday and Date formatting
plot_data <- full_schedule_table %>% 
  mutate(
    date = ymd(date), # Ensure 'date' is a Date object
    weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 7) # Create the weekday column
  )

# 4. Run a ggplot
ggplot(plot_data) + 
  theme_bw() +  
  geom_point(aes(x = date, y = servicepattern_id, color = weekday), size = 1) +  
  scale_x_date(breaks = scales::date_breaks("1 month")) + 
  theme(legend.position = "bottom") +
  labs(
    x = "Date", 
    y = "Service Pattern ID", 
    color = "Day of Week"
  )


#=====================================================================================================
#automatically generate readable names for service patterns
suggest_servicepattern_name <- function(dates, calendar = NULL) {
  # 1. Build a local helper tibble from the dates vector
  df <- tibble(date = as.Date(dates)) %>% 
    mutate(
      # Extract full weekday names ("Sunday", "Monday", etc.)
      weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 7),
      weekday = as.character(weekday),
      # Fallback for holiday if not present in input
      holiday = if (!is.null(calendar) && "holiday" %in% names(calendar)) calendar$holiday else NA_character_
    )
  
  # All normal dates without holidays  
  calendar_normal <- df %>% filter(is.na(holiday))
  
  # Create a frequency table for all calendar dates without holidays
  weekday_freq = sort(table(calendar_normal$weekday), decreasing = TRUE)
  n_weekdays = length(weekday_freq)
  
  # All holidays that are not covered by normal weekdays anyways
  calendar_holidays <- df %>% filter(!is.na(holiday)) %>% filter(!(weekday %in% names(weekday_freq)))
  
  if(n_weekdays == 7) {
    pattern_name = "Every day"
  }
  # Single day service
  else if(n_weekdays == 1) {
    wd = names(weekday_freq)[1]
    pattern_name = c("Sunday"  = "Sundays",         
                     "Monday"    = "Mondays",         
                     "Tuesday"   = "Tuesdays",         
                     "Wednesday" = "Wednesdays",        
                     "Thursday"  = "Thursdays",          
                     "Friday"    = "Fridays",          
                     "Saturday"  = "Saturdays")[wd]
  } 
  # Weekday Service
  else if(n_weekdays == 5 &&       
          length(intersect(names(weekday_freq), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))) == 5) {
    pattern_name = "Weekdays"
  } 
  # Weekend
  else if(n_weekdays == 2 &&       
          length(intersect(names(weekday_freq), c("Saturday", "Sunday"))) == 2) {
    pattern_name = "Weekends"
  } 
  # Multiple weekdays that appear regularly
  else if(n_weekdays >= 2 && (max(weekday_freq) - min(weekday_freq)) <= 1) {
    wd = names(weekday_freq)
    ordered_wd = wd[order(match(wd, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))]
    pattern_name = paste(ordered_wd, collapse = ", ")
  } 
  # default
  else {
    pattern_name = paste(weekday_freq, names(weekday_freq), sep = "x ", collapse = ", ")
  }
  
  # add holidays if any
  if(nrow(calendar_holidays) > 0) {
    pattern_name <- paste0(pattern_name, " and ", paste(calendar_holidays$holiday, collapse = ", "))
  }
  
  pattern_name <- paste0(pattern_name, " (", min(dates), " - ", max(dates), ")") 
  
  return(pattern_name)
}

#==================================================================
#generate a human-readable name for every unique service pattern in King County Metro

service_pattern_names <- full_schedule_table %>%
  group_by(servicepattern_id) %>%
  summarise(
    pattern_name = suggest_servicepattern_name(date),
    total_operating_days = n(),
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  )


#================================================================
#Plot calendar for each service pattern

# 1. Prepare and clean the dates data frame from your tidytransit object
dates <- kcm$.$dates_servicepatterns %>% 
  mutate(
    date = ymd(date),
    wday = wday(date, label = TRUE, abbr = TRUE, week_start = 7),
    # Combine year and week number to prevent overlapping across years
    week_nr = year(date) * 100 + week(date)
  )

# 2. Extract the first date of each week for the y-axis labels
week_labels <- dates %>% 
  group_by(week_nr) %>% 
  summarise(week_first_date = min(date), .groups = "drop")
dates <- week_labels %>% 
  right_join(dates, by = "week_nr")

# 3. Generate the calendar heatmap plot
ggplot(dates) + 
  theme_bw() +  
  geom_tile(aes(x = wday, y = week_nr), color = "#747474") +  
  scale_x_discrete(drop = FALSE) +  
  scale_y_continuous(
    trans = "reverse", 
    labels = week_labels$week_first_date, 
    breaks = week_labels$week_nr
  ) +  
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.minor = element_blank()
  ) +  
  labs(x = NULL, y = "Week Starting Date") +  
  facet_wrap(~servicepattern_id, nrow = 1)

#====================================================================================
#Plot number of trips per day as calendar

# 1. Map trips to service patterns via service_id
trips_servicepattern <- left_join(
  select(kcm$trips, trip_id, service_id), 
  kcm$.$servicepatterns, 
  by = "service_id"
)

# 2. Join with service pattern dates and count active trips per date
trip_dates <- left_join(
  kcm$.$dates_servicepatterns, 
  trips_servicepattern, 
  by = "servicepattern_id", 
  relationship = "many-to-many"
)

trip_dates_count <- trip_dates %>%   
  mutate(date = ymd(date)) %>% # Ensure date is a Date object
  group_by(date) %>%   
  summarise(count = dplyr::n(), .groups = "drop") 

# 3. Compute calendar grid positions for monthly faceting
trip_dates_count <- trip_dates_count %>%   
  mutate(
    weekday = wday(date, label = TRUE, abbr = TRUE, week_start = 7),         
    day_of_month = day(date),         
    first_day_of_month = wday(date - day_of_month, week_start = 7),         
    week_of_month = ceiling((day_of_month - as.numeric(weekday) - first_day_of_month) / 7),         
    month = month(date, label = TRUE, abbr = FALSE)
  )

# 4. Generate the calendar heatmap
ggplot(trip_dates_count, aes(x = weekday, y = -week_of_month)) +   
  theme_bw() +  
  facet_wrap(~month, ncol = 3) +  
  geom_tile(aes(fill = count, colour = "grey50")) +  
  geom_text(aes(label = day_of_month), size = 3, colour = "grey20") +  
  scale_fill_gradient(low = "cornsilk1", high = "DarkOrange", na.value = "white") +  
  scale_color_manual(guide = "none", values = "grey50") +  
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +  
  labs(x = NULL, y = NULL, fill = "# Trips") +  
  coord_fixed()


#====================================================================

library(dplyr)
library(lubridate)

# 1. Map trips to dates through service patterns to see daily service volume
trip_dates <- kcm$trips %>%
  select(trip_id, service_id) %>%
  left_join(kcm$.$servicepatterns, by = "service_id", relationship = "many-to-many") %>%
  left_join(kcm$.$dates_servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  mutate(
    date = ymd(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 7)
  )

# 2. Find the first full operating date for Weekdays, Saturday, and Sunday
# We define a "full" day as one of the earliest dates that achieves close to max daily trip volume for that day type
daily_trip_summary <- trip_dates %>%
  group_by(date, weekday) %>%
  summarise(total_trips = n_distinct(trip_id), .groups = "drop")

# Determine max trips per day type to filter out minor holiday schedules
max_trips_by_type <- daily_trip_summary %>%
  group_by(weekday) %>%
  summarise(max_trips = max(total_trips), .groups = "drop")

# Select the first date that reaches at least 95% of peak service for that day type
selected_dates_df <- daily_trip_summary %>%
  left_join(max_trips_by_type, by = "weekday") %>%
  filter(total_trips >= 0.95 * max_trips) %>%
  group_by(weekday) %>%
  summarise(first_full_date = min(date), .groups = "drop") %>%
  filter(weekday %in% c("Tuesday", "Saturday", "Sunday")) # Or pick Monday for weekday

# 3. Print the selected dates nicely
print("First full operating dates selected:")
print(selected_dates_df)

# Store them as named variables for downstream filtering
target_dates <- setNames(as.character(selected_dates_df$first_full_date), selected_dates_df$weekday)


#=============================================================================
#For each day of the week, create a chart of number of trip starts by route by hour. 

# 1. Filter dates, map to trips, and join kcm$routes to get route_short_name
active_trips_on_target_dates <- kcm$.$dates_servicepatterns %>%
  mutate(date_char = as.character(ymd(date))) %>%
  filter(date_char %in% target_dates) %>%
  mutate(
    day_type = case_when(
      date_char == target_dates["Tuesday"] ~ "Weekday (Tuesday)",
      date_char == target_dates["Saturday"] ~ "Saturday",
      date_char == target_dates["Sunday"] ~ "Sunday"
    )
  ) %>%
  left_join(kcm$.$servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  left_join(kcm$trips %>% select(trip_id, route_id, service_id), by = "service_id", relationship = "many-to-many") %>%
  left_join(kcm$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  select(trip_id, route_short_name, day_type)

# 2. Join stop_times ONLY for the filtered subset of trips
trip_start_hours <- kcm$stop_times %>%
  filter(stop_sequence == 1) %>%
  inner_join(active_trips_on_target_dates, by = "trip_id") %>%
  mutate(
    # Handle GTFS times that go beyond 24:00:00 (late-night service)
    hour = as.numeric(substr(departure_time, 1, 2)) %% 24
  )

# 3. Aggregate trip counts by day type, route_short_name, and hour
route_hourly_trips <- trip_start_hours %>%
  group_by(day_type, route_short_name, hour) %>%
  summarise(trip_count = n(), .groups = "drop")

# 4. Filter for the top 15 routes by total volume using their short names
top_routes <- route_hourly_trips %>%
  group_by(route_short_name) %>%
  summarise(total = sum(trip_count)) %>%
  slice_max(total, n = 15) %>%
  pull(route_short_name)

# 5. Plot the faceted hourly distribution
ggplot(route_hourly_trips %>% filter(route_short_name %in% top_routes), 
       aes(x = hour, y = trip_count, color = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_grid(route_short_name ~ day_type, scales = "free_y") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 23, by = 4), labels = paste0(seq(0, 23, by = 4), ":00")) +
  labs(
    title = "Trip Starts by Route and Hour (First Full Operating Schedule)",
    subtitle = "Grouped by Route Short Name",
    x = "Hour of Day",
    y = "Number of Trip Starts",
    color = "Day Type"
  ) +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0, size = 8),
    panel.grid.minor = element_blank()
  )



#===============================================================


# 1. Select a representative weekday date from our earlier analysis (e.g., Tuesday)
target_date <- "2026-10-13" # Replace with your dynamic target date if desired

# 2. Extract stop times and filter for the 9:00 PM - 10:00 PM window (21:00 to 22:00)
# Convert GTFS time string to total seconds past midnight for easy filtering
window_stops <- kcm$stop_times %>%
  mutate(
    h = as.numeric(substr(arrival_time, 1, 2)),
    m = as.numeric(substr(arrival_time, 4, 5)),
    sec_from_midnight = h * 3600 + m * 60,
    # Create 5-minute bins (e.g., "21:00", "21:05", etc.)
    bin_minute = floor(m / 5) * 5,
    time_window = sprintf("21:%02d", bin_minute)
  ) %>%
  filter(h == 21, sec_from_midnight >= 75600, sec_from_midnight < 79200) %>% # 21:00 to 22:00 in seconds
  inner_join(kcm$trips %>% select(trip_id, route_id, service_id), by = "trip_id") %>%
  inner_join(kcm$.$servicepatterns, by = "service_id") %>%
  inner_join(kcm$.$dates_servicepatterns, by = "servicepattern_id") %>%
  filter(as.character(ymd(date)) == target_date)

# 3. Join with stops to get coordinates and calculate bearings
stop_coords <- kcm$stops %>% select(stop_id, stop_lat, stop_lon)

trip_spatial_data <- window_stops %>%
  left_join(stop_coords, by = "stop_id") %>%
  arrange(trip_id, stop_sequence) %>%
  group_by(trip_id) %>%
  mutate(
    # Get prior and subsequent stop coordinates for bearing calculation
    prev_lon = lag(stop_lon, default = first(stop_lon)),
    prev_lat = lag(stop_lat, default = first(stop_lat)),
    next_lon = lead(stop_lon, default = last(stop_lon)),
    next_lat = lead(stop_lat, default = last(stop_lat)),
    # Calculate bearing angle (in degrees) between prior and subsequent stop
    bearing = atan2(next_lon - prev_lon, next_lat - prev_lat) * (180 / pi)
  ) %>%
  ungroup()

# 4. For each 5-min window, find the LAST stop a trip passed by in that window
last_stops_per_window <- trip_spatial_data %>%
  group_by(time_window, trip_id) %>%
  slice_max(stop_sequence, n = 1, with_ties = FALSE) %>%
  ungroup()

# 5. Extract and convert route shapes for active routes using shapes_as_sf()
active_route_ids <- unique(window_stops$route_id)

active_shape_ids <- kcm$trips %>% 
  filter(route_id %in% active_route_ids) %>% 
  pull(shape_id) %>% 
  unique()

# Filter the shapes table and convert directly to an sf LINESTRING object
route_shapes_sf <- kcm$shapes %>% 
  filter(shape_id %in% active_shape_ids) %>% 
  tidytransit::shapes_as_sf()
#===============================================================

# Convert last stop locations to an sf point object
last_stops_sf <- st_as_sf(
  last_stops_per_window, 
  coords = c("stop_lon", "stop_lat"), 
  crs = 4326
)

# Build the ggplot animation
anim <- ggplot() +
  # Background: Route shapes for all active routes
  geom_sf(data = route_shapes_sf, color = "grey80", linewidth = 0.5, alpha = 0.6) +
  # Foreground: Trip stop locations represented as directional arrows (using angle = bearing)
  geom_point(
    data = last_stops_per_window, 
    aes(x = stop_lon, y = stop_lat, color = time_window), 
    size = 2.5, 
    alpha = 0.8
  ) +
  theme_bw() +
  labs(
    title = "King County Metro Active Trips (9:00 PM - 10:00 PM)",
    subtitle = "Time Window: {closest_state}",
    x = "Longitude", 
    y = "Latitude",
    color = "5-Min Window"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  ) +
  # Animate across the 5-minute time windows
  transition_states(time_window, transition_length = 2, state_length = 1) +
  ease_aes('linear')

# Render and save the animation as a GIF
animate(anim, nframes = 60, fps = 10, width = 700, height = 600, renderer = gifski_renderer("kc_metro_9pm_animation.gif"))





























set_servicepattern_safe <- function(gtfs) {
  
  # Generate dates_services only if missing
  if (is.null(gtfs$.$dates_services)) {
    gtfs <- tidytransit:::set_dates_services(gtfs)
  }
  
  # Calculate service patterns
  gtfs <- tidytransit::set_servicepattern(gtfs)
  
  return(gtfs)
}

gtfs_KCM <- set_servicepattern_safe(gtfs_KCM)
gtfs_ST <- set_servicepattern(gtfs_ST)

kcm<-gtfs_KCM

summary(gtfs_KCM)
head(gtfs_KCM$stops)
names(kcm)


validation_result <- attr(kcm, "validation_result")
head(validation_result)

head(kcm$.$dates_services)



service_pattern_summary <- function(gtfs) {
  
  # Convert GTFS to spatial format
  gtfs <- gtfs_as_sf(gtfs)
  
  # Calculate shape lengths
  gtfs$shapes$length <- sf::st_length(gtfs$shapes)
  
  # Extract shape lengths
  shape_lengths <- gtfs$shapes %>%
    as.data.frame() %>%
    select(shape_id, length)
  
  # Calculate service pattern statistics
  summary <- gtfs$trips %>%
    left_join(gtfs$.$servicepatterns, by = "service_id") %>%
    left_join(shape_lengths, by = "shape_id") %>%
    left_join(gtfs$stop_times, by = "trip_id") %>%
    group_by(servicepattern_id) %>%
    summarise(
      trips = n(),
      routes = n_distinct(route_id),
      total_distance_per_day_km =
        sum(as.numeric(length), na.rm = TRUE) / 1e3,
      route_avg_distance_km =
        (sum(as.numeric(length), na.rm = TRUE) / 1e3) /
        (trips * routes),
      stops = n_distinct(stop_id) / 2,
      .groups = "drop"
    )
  
  # Calculate days in service
  days_summary <- gtfs$.$dates_servicepatterns %>%
    group_by(servicepattern_id) %>%
    summarise(
      days_in_service = n(),
      .groups = "drop"
    )
  
  # Combine summaries
  service_pattern_summary <- days_summary %>%
    left_join(summary, by = "servicepattern_id")
  
  return(service_pattern_summary)
}

KCM_Summary<-service_pattern_summary(kcm)
ST_Summary<-service_pattern_summary(gtfs_ST)


class(gtfs_KCM)
names(kcm)
is.null(gtfs_KCM$trips)
is.null(gtfs_KCM$stop_times)
is.null(gtfs_KCM$calendar)
is.null(gtfs_KCM$calendar_dates)
is.null(gtfs_KCM$.$dates_services) #
gtfs_KCM$.$dates_services
gtfs_KCM$calendar
gtfs_KCM$calendar_dates
ls("package:tidytransit")
getAnywhere(feed_has_non_empty_table)

str(gtfs_KCM$., max.level = 1)
names(gtfs_KCM$.)
packageVersion("tidytransit")


# Generate the missing dates_services table
gtfs_KCM <- tidytransit:::set_dates_services(gtfs_KCM)

# Verify that it exists
head(gtfs_KCM$.$dates_services)

# Calculate service patterns
gtfs_KCM <- tidytransit::set_servicepattern(gtfs_KCM)

gtfs_KCM <- set_servicepattern(gtfs_KCM) 
gtfs_ST <- set_servicepattern(gtfs_ST)

# Define Target Date
target_date_num <- 20261112
target_date <- as.Date(as.character(target_date_num), format = "%Y%m%d")
target_dow  <- tolower(format(target_date, "%A")) # e.g., "thursday"

cat(paste0("Target Date: ", target_date, " (", tools::toTitleCase(target_dow), ")\n"))

# Helper function to extract night service routes from a GTFS URL
get_night_routes <- function(url, agency_label) {

    # Download GTFS data
  gtfs <- read_gtfs(url)

  # Extract hour (first 2 digits) from string HH:MM:SS format
  # GTFS uses >24h format for trips past midnight (e.g., 25:15:00 = 1:15 AM next day)    
  # Night range: 21:00-24:00 (9PM-12AM), 24:00-30:00 (12AM-6AM next day), or 00:00-06:00
  night_stop_times <- gtfs$stop_times %>%
    mutate(
      hour = as.integer(substr(departure_time, 1, 2))
    ) %>%
    filter((hour >= 21 & hour < 24) | (hour >= 24 & hour < 30) | (hour >= 0 & hour < 6)) %>%
    select(trip_id) %>%
    distinct()
  
  # Join with trips to get route_ids, then join with routes to get names
  night_routes <- gtfs$trips %>%
    filter(trip_id %in% night_stop_times$trip_id) %>%
    select(route_id) %>%
    distinct() %>%
    inner_join(gtfs$routes, by = "route_id") %>%
    mutate(agency = agency_label) %>%
    select(agency, route_id, route_short_name, route_long_name) %>%
    arrange(suppressWarnings(as.numeric(route_short_name)), route_short_name)
  
  return(night_routes)
}

# 2. Run for both agencies
kcm_night <- get_night_routes(urls$KCM, "King County Metro")
st_night  <- get_night_routes(urls$ST, "Sound Transit")

# 3. Combine results
all_night_routes <- bind_rows(kcm_night, st_night)

# Display Results
print(all_night_routes)

#===========================================================
#Import gtfs from Metro and ST, determine identity of routes operating between 9pm and 6AM
#===========================================================
# Robust helper function handling hms objects, numeric seconds, and character strings
gtfs_time_to_seconds <- function(time_col) {
  # If tidytransit parsed it as an hms object or numeric, it's already seconds since midnight
  if (inherits(time_col, "hms") || is.numeric(time_col)) {
    return(as.numeric(time_col))
  }
  
  # Otherwise, treat as character/factor and parse
  time_str <- as.character(time_col)
  parts <- strsplit(time_str, ":")
  map_dbl(parts, function(x) {
    if (length(x) != 3 || any(is.na(x))) return(NA)
    as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3])
  })
}

# Robust custom function to convert GTFS shapes table directly to sf LINESTRINGs
shapes_to_sf <- function(gtfs) {
  if (is.null(gtfs$shapes) || nrow(gtfs$shapes) == 0) return(NULL)
  
  gtfs$shapes %>%
    filter(!is.na(shape_id) & shape_id != "" & !is.na(shape_pt_lon) & !is.na(shape_pt_lat)) %>%
    arrange(shape_id, shape_pt_sequence) %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
    group_by(shape_id) %>%
    summarize(do_union = FALSE, .groups = "drop") %>%
    st_cast("LINESTRING")
}

# Time window bounds in seconds past midnight
WINDOW_START <- 21 * 3600        # 21:00:00 (75600s)
WINDOW_END   <- 21 * 3600 + 300  # 21:05:00 (75900s)

get_active_service_ids <- function(gtfs, target_date, target_dow) {
  base_services <- character(0)
  if (!is.null(gtfs$calendar)) {
    dow_col <- target_dow
    base_services <- gtfs$calendar %>%
      filter(
        start_date <= target_date & 
          end_date >= target_date & 
          .data[[dow_col]] == 1
      ) %>%
      pull(service_id)
  }
  
  if (!is.null(gtfs$calendar_dates)) {
    exceptions <- gtfs$calendar_dates %>%
      filter(date == target_date)
    
    added_services <- exceptions %>% filter(exception_type == 1) %>% pull(service_id)
    removed_services <- exceptions %>% filter(exception_type == 2) %>% pull(service_id)
    
    base_services <- union(base_services, added_services)
    base_services <- setdiff(base_services, removed_services)
  }
  
  return(base_services)
}

process_agency_spatial <- function(url, agency_label, target_date, target_dow) {
  cat(paste0("\n--- Processing ", agency_label, " ---\n"))
  
  gtfs <- tryCatch({
    read_gtfs(url)
  }, error = function(e) {
    message("Failed to load GTFS feed: ", e$message)
    return(NULL)
  })
  
  if (is.null(gtfs)) return(NULL)
  
  active_services <- get_active_service_ids(gtfs, target_date, target_dow)
  
  if (length(active_services) == 0) {
    message("No active services found for ", agency_label, " on ", target_date)
    return(NULL)
  }
  
  date_trips <- gtfs$trips %>%
    filter(service_id %in% active_services)
  
  # Convert GTFS stop_times safely
  stop_times_sec <- gtfs$stop_times %>%
    filter(trip_id %in% date_trips$trip_id) %>%
    mutate(
      arr_sec = gtfs_time_to_seconds(arrival_time),
      dep_sec = gtfs_time_to_seconds(departure_time)
    )
  
  trip_summary <- stop_times_sec %>%
    group_by(trip_id) %>%
    summarise(
      trip_start = min(dep_sec, na.rm = TRUE),
      trip_end   = max(arr_sec, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(trip_start <= WINDOW_END & trip_end >= WINDOW_START)
  
  if (nrow(trip_summary) == 0) {
    message("No active trips found in the 9:00-9:05 PM window for ", agency_label, " on ", target_date)
    return(NULL)
  }
  
  active_trips <- date_trips %>%
    filter(trip_id %in% trip_summary$trip_id) %>%
    inner_join(gtfs$routes, by = "route_id") %>%
    mutate(agency = agency_label)
  
  # A. SPATIAL OBJECT 1: Active Route Line Geometries (using robust custom converter)
  gtfs_sf <- shapes_to_sf(gtfs)
  
  active_shapes_sf <- active_trips %>%
    select(agency, route_id, route_short_name, route_long_name, shape_id) %>%
    distinct() %>%
    filter(!is.na(shape_id)) %>%
    inner_join(gtfs_sf, by = "shape_id") %>%
    st_as_sf()
  
  # B. SPATIAL OBJECT 2: Latest Stop Location for Every Active Trip by 9:05 PM
  latest_trip_stops <- stop_times_sec %>%
    filter(trip_id %in% active_trips$trip_id) %>%
    filter(arr_sec <= WINDOW_END) %>%
    group_by(trip_id) %>%
    slice_max(order_by = stop_sequence, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    inner_join(
      active_trips %>% select(agency, trip_id, route_id, route_short_name, route_long_name),
      by = "trip_id"
    ) %>%
    inner_join(gtfs$stops, by = "stop_id") %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    mutate(
      arr_time_formatted = sprintf(
        "%02d:%02d:%02d",
        as.integer(arr_sec) %/% 3600,
        (as.integer(arr_sec) %% 3600) %/% 60,
        as.integer(arr_sec) %% 60
      )
    )
  
  return(list(
    routes_sf = active_shapes_sf,
    stops_sf  = latest_trip_stops
  ))
}

# 2. Extract Spatial Data for Both Agencies for the Target Date
kcm_spatial <- process_agency_spatial(urls$KCM, "King County Metro", target_date, target_dow)
st_spatial  <- process_agency_spatial(urls$ST,  "Sound Transit",     target_date, target_dow)

# Combine sf objects across agencies safely
active_routes_sf <- bind_rows(
  if (!is.null(kcm_spatial)) kcm_spatial$routes_sf,
  if (!is.null(st_spatial))  st_spatial$routes_sf
)

latest_stops_sf <- bind_rows(
  if (!is.null(kcm_spatial)) kcm_spatial$stops_sf,
  if (!is.null(st_spatial))  st_spatial$stops_sf
)

cat("\n=== Spatial Data Summary for Date:", as.character(target_date), "===\n")
cat("Active Route Shapes:", nrow(active_routes_sf), "\n")
cat("Latest Trip Stops Reached by 9:05 PM:", nrow(latest_stops_sf), "\n")

# 3. Interactive Leaflet Map using OpenStreetMap Base Tiles
if (nrow(active_routes_sf) > 0) {
  leaflet() %>%
    # Use standard OpenStreetMap tiles (free, no API key required)
    addTiles() %>%
    # Route Line Geometries
    addPolylines(
      data = active_routes_sf,
      color = ~if_else(agency == "King County Metro", "#00835C", "#002F6C"),
      weight = 3,
      opacity = 0.7,
      popup = ~paste0("<b>Agency:</b> ", agency, "<br><b>Route:</b> ", route_short_name, " - ", route_long_name)
    ) %>%
    # Latest Bus Stop Locations
    addCircleMarkers(
      data = latest_stops_sf,
      radius = 5,
      fillColor = ~if_else(agency == "King County Metro", "#E31837", "#FFC72C"),
      color = "#ffffff",
      weight = 1,
      fillOpacity = 0.9,
      popup = ~paste0(
        "<b>Agency:</b> ", agency, "<br>",
        "<b>Route:</b> ", route_short_name, "<br>",
        "<b>Trip ID:</b> ", trip_id, "<br>",
        "<b>Latest Stop:</b> ", stop_name, "<br>",
        "<b>Arrival Time:</b> ", arr_time_formatted
      )
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#00835C", "#002F6C", "#E31837", "#FFC72C"),
      labels = c("KCM Active Route", "ST Active Route", "KCM Latest Stop", "ST Latest Stop"),
      title = paste0("Transit Activity: ", target_date, " (21:00-21:05)")
    )
} else {
  message("No spatial features found to plot for the selected date and time window.")
}

