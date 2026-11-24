####NOTE: Some of the code below adapted from adapted from: https://r-transit.github.io/tidytransit/articles/servicepatterns.html
####I also have used AI to help write or debug code. I am trying to build this up logically, so I can continue to modify it.
#===========================================================
#Libraries used below
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
#===========================================================
#Import gtfs from Metro and ST, determine identity of routes operating between 9pm and 6AM
#===========================================================

# Define Feed URLs
urls <- list(
  KCM = "https://metro.kingcounty.gov/GTFS/google_transit.zip",
  ST  = "https://www.soundtransit.org/GTFS-rail/40_gtfs.zip"
)

gtfs_KCM <- tidytransit::read_gtfs(urls$KCM)
gtfs_ST <- tidytransit::read_gtfs(urls$ST)

#Note the data fields in the Metro gtfs were not formatted correctly, so the following code allows me to reformate the date fields
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

#Validate the new Metro gtfs
validation_result <- attr(kcm, "validation_result")
head(validation_result)
#kcm <- tidytransit:::set_dates_services(kcm)
kcm <- tidytransit::set_servicepattern(kcm)
head(kcm$.$servicepatterns)
print(kcm$.$dates_servicepatterns, n = 90)
summary(kcm)

#Make sure the service_id and servicepattern_id counts are as expected
length(unique(kcm$trips$service_id)) # 13
length(unique(kcm$.$servicepatterns$servicepattern_id)) #38

#====================================================================
#Merge two GTFS feeds  
#prefix all ID columns in each feed before combining the tables so that overlapping IDs (like route_id = "1" or trip_id = "100") from different agencies do not collide.

prefix_gtfs <- function(gtfs_obj, prefix) {
  id_cols <- c("agency_id", "route_id", "service_id", "trip_id", 
               "stop_id", "shape_id", "fare_id", "zone_id", "block_id")
  
  # Loop through standard GTFS tables
  for (tbl_name in names(gtfs_obj)) {
    if (is.data.frame(gtfs_obj[[tbl_name]])) {
      cols_to_prefix <- intersect(names(gtfs_obj[[tbl_name]]), id_cols)
      
      gtfs_obj[[tbl_name]] <- gtfs_obj[[tbl_name]] %>%
        mutate(across(all_of(cols_to_prefix), ~ ifelse(is.na(.), NA, paste0(prefix, .))))
    }
  }
  return(gtfs_obj)
}

#Do the merge:
# Prefix IDs so they never collide
kcm_pref <- prefix_gtfs(kcm, "1")
st_pref  <- prefix_gtfs(gtfs_ST, "2")

# Combine shared tables
shared_tables <- intersect(names(kcm_pref), names(st_pref))


merged_gtfs <- list()
for (tbl in shared_tables) {
  if (is.data.frame(kcm_pref[[tbl]]) && is.data.frame(st_pref[[tbl]])) {
    merged_gtfs[[tbl]] <- bind_rows(kcm_pref[[tbl]], st_pref[[tbl]])
  }
}

# Restore tidytransit class and rebuild service patterns
merged_gtfs <- as_tidygtfs(merged_gtfs)
merged_gtfs <- set_servicepattern(merged_gtfs)

#validate merged gtfs
validation_result <- attr(merged_gtfs, "validation_result")
head(validation_result)
head(merged_gtfs$agency)
#========================================================================================================
#service pattern plot
#adapted from: https://r-transit.github.io/tidytransit/articles/servicepatterns.html


# 1. Join dates with their service pattern definitions
date_servicepattern_table <- merged_gtfs$.$dates_servicepatterns %>% 
  left_join(merged_gtfs$.$servicepatterns, by = "servicepattern_id")

# 2. If you also need the raw calendar attributes (like start_date, end_date, 
# and weekly service flags) from kcm$calendar, join via service_id:
full_schedule_table <- date_servicepattern_table %>% 
  left_join(merged_gtfs$calendar, by = "service_id")

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



#====================================================================================
#Plot number of trips per day as calendar
#adapted from: https://r-transit.github.io/tidytransit/articles/servicepatterns.html

# 1. Map trips to service patterns via service_id
trips_servicepattern <- left_join(
  select(merged_gtfs$trips, trip_id, service_id), 
  merged_gtfs$.$servicepatterns, 
  by = "service_id"
)

# 2. Join with service pattern dates and count active trips per date
trip_dates <- left_join(
  merged_gtfs$.$dates_servicepatterns, 
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
# With so many service patterns, it can be hard to find calendar days that have all service operating
#This section of code maps trips to dates through service patterns to see daily service volume

trip_dates <- merged_gtfs$trips %>%
  select(trip_id, service_id) %>%
  left_join(merged_gtfs$.$servicepatterns, by = "service_id", relationship = "many-to-many") %>%
  left_join(merged_gtfs$.$dates_servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  mutate(
    date = ymd(date),
    weekday = wday(date, label = TRUE, abbr = FALSE, week_start = 7)
  )

# Find the first full operating date for Weekdays, Saturday, and Sunday
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

# Print the selected dates nicely
print("First full operating dates selected:")
print(selected_dates_df)

# Store them as named variables for downstream filtering
target_dates <- setNames(as.character(selected_dates_df$first_full_date), selected_dates_df$weekday)


#=============================================================================
#For each day of the week, create a chart of number of trip starts by route by hour. 
#adapted from: https://r-transit.github.io/tidytransit/articles/servicepatterns.html

# Filter dates, map to trips, and join routes to get route_short_name
active_trips_on_target_dates <- merged_gtfs$.$dates_servicepatterns %>%
  mutate(date_char = as.character(ymd(date))) %>%
  filter(date_char %in% target_dates) %>%
  mutate(
    day_type = case_when(
      date_char == target_dates["Tuesday"] ~ "Weekday",
      date_char == target_dates["Saturday"] ~ "Saturday",
      date_char == target_dates["Sunday"] ~ "Sunday"
    )
  ) %>%
  left_join(merged_gtfs$.$servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  left_join(merged_gtfs$trips %>% select(trip_id, route_id, service_id), by = "service_id", relationship = "many-to-many") %>%
  left_join(merged_gtfs$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  select(trip_id, route_short_name, day_type)

# Join stop_times ONLY for the filtered subset of trips
trip_start_hours <- merged_gtfs$stop_times %>%
  filter(stop_sequence == 1) %>%
  inner_join(active_trips_on_target_dates, by = "trip_id") %>%
  mutate(
    # Extract leading digits before first colon to handle GTFS 24+ hour service (e.g., 25:30:00 -> 1)
    hour = as.numeric(sub(":.*", "", departure_time)) %% 24
  )

# Aggregate trip counts by day type, route_short_name, and hour
route_hourly_trips <- trip_start_hours %>%
  group_by(day_type, route_short_name, hour) %>%
  summarise(trip_count = n(), .groups = "drop")

# Extract top 10 routes and order factors by volume
top_routes_summary <- route_hourly_trips %>%
  filter(!is.na(route_short_name)) %>%
  group_by(route_short_name) %>%
  summarise(total = sum(trip_count, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 10)

# Complete missing day/route/hour combinations with 0 for the top routes with NAs
day_order <- c("Weekday", "Saturday", "Sunday")

plot_data <- route_hourly_trips %>%
  filter(route_short_name %in% top_routes_summary$route_short_name) %>%
  complete(
    day_type = day_order,
    route_short_name = top_routes_summary$route_short_name,
    hour = 0:23,
    fill = list(trip_count = 0)
  ) %>%
  mutate(
    day_type = factor(day_type, levels = day_order),
    route_short_name = factor(route_short_name, levels = top_routes_summary$route_short_name)
  )

# Plot the faceted hourly distribution
ggplot(plot_data, aes(x = hour, y = trip_count,color = day_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_point(
    data = ~ filter(.x, trip_count == 0),
    shape = 15,
    color = "black",
    size = 2
  ) +
  facet_grid(route_short_name ~ day_type) +  
  coord_cartesian(ylim = c(0, 20), clip = "off") +     # Locks y-axis from 0 to 20
  scale_x_continuous(breaks = seq(0, 23, by = 4), labels = paste0(seq(0, 23, by = 4), ":00")) +
  theme_bw() +
  labs(
    title = "Number of Trips by Route Day of Week, and Hour",
    subtitle = paste(
      "Grouped by Route/Line for the highest frequency routes at Sound Transit and King County Metro",
      "Black squares represent zero trips starts in that hour.",
      sep = "\n"
    ),
    x = "Hour of Day",
    y = "Number of Trips",
    color = "Day of Week"
  ) +
  theme(
    legend.position = "bottom",
    strip.text.y = element_text(angle = 0, size = 8, face = "bold"),
    strip.background = element_rect(fill = "gray95"),
    panel.grid.minor = element_blank()
  )


#==================================================================
#==================================================================
#Work in Progress: finds all transit trips running on weekdays and extracts stop arrivals occurring between 21:00 and 22:00, grouping each arrival into a 5-minute time bin.
#================================

# Get active trip_ids for target_date first
active_trips <- merged_gtfs$.$dates_servicepatterns %>%
  filter(as.character(date) == target_dates[2]) %>%
  distinct(servicepattern_id) %>%
  inner_join(merged_gtfs$.$servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  distinct(service_id) %>%
  inner_join(
    merged_gtfs$trips %>% select(trip_id, route_id, service_id),
    by = "service_id",
    relationship = "many-to-many"
  ) %>%
  left_join(
    merged_gtfs$routes %>% select(route_id, route_short_name),
    by = "route_id"
  )

# Filter stop_times for active trips in the 21:00-22:00 window into 5-minute bins
window_stops <- merged_gtfs$stop_times %>%
  inner_join(active_trips, by = "trip_id") %>%
  mutate(
    h = as.numeric(sub("^([0-9]+):.*", "\\1", arrival_time)),
    m = as.numeric(sub("^[0-9]+:([0-9]+):.*", "\\1", arrival_time)),
    s = as.numeric(sub("^[0-9]+:[0-9]+:([0-9]+).*", "\\1", arrival_time)),
    sec_from_midnight = h * 3600 + m * 60 + s
  ) %>%
  filter(sec_from_midnight >= 75600, sec_from_midnight < 79200) %>%
  mutate(
    bin_minute = floor(m / 5) * 5,
    time_window = sprintf("%02d:%02d", h %% 24, bin_minute)
  )


#==============================
#Create transfer points: identify where active transit routes physically cross each other and builds a summary table of those intersection points with coordinates and route lists.

# Helper to safely assign CRS only when missing, otherwise transform to target EPSG
ensure_crs <- function(x, epsg = 4326) {
  if (is.na(st_crs(x))) {
    st_set_crs(x, epsg)
  } else {
    st_transform(x, epsg)
  }
}

# Extract active route IDs as a vector and pull matching shape IDs
active_route_ids <- window_stops %>%
  filter(!is.na(route_id)) %>%
  distinct(route_id) %>%
  pull(route_id)

active_shape_ids <- merged_gtfs$trips %>%
  filter(route_id %in% active_route_ids, !is.na(shape_id)) %>%
  distinct(shape_id) %>%
  pull(shape_id)

# Filter shapes with a defensive row check
filtered_shapes <- merged_gtfs$shapes %>%
  filter(shape_id %in% active_shape_ids)

if (nrow(filtered_shapes) == 0) {
  stop("Error: No matching shapes found in shapes table for these active routes.")
}

# Convert filtered shapes to sf
route_shapes_sf <- shapes_as_sf(filtered_shapes) %>%
  ensure_crs(4326)

# Helper function to extract route-level geometries from a GTFS object
get_route_shapes_sf <- function(gtfs_obj, agency_label, route_filter = NULL) {
  shapes_sf <- shapes_as_sf(gtfs_obj$shapes) %>%
    ensure_crs(4326)
  
  trips_routes <- gtfs_obj$trips %>%
    select(route_id, shape_id) %>%
    distinct() %>%
    inner_join(
      gtfs_obj$routes %>% select(route_id, route_short_name),
      by = "route_id",
      relationship = "many-to-many"
    )
  
  if (!is.null(route_filter)) {
    trips_routes <- trips_routes %>% filter(route_id %in% route_filter)
  }
  
  trips_routes %>%
    distinct(route_id, route_short_name, shape_id) %>%
    inner_join(shapes_sf, by = "shape_id", relationship = "many-to-many") %>%
    st_as_sf() %>%
    group_by(route_id, route_short_name) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(agency = agency_label)
}

# Build combined routes layer and compute crossing points
all_routes <- get_route_shapes_sf(
  merged_gtfs,
  agency_label = "Combined GTFS",
  route_filter = active_route_ids
) %>%
  ensure_crs(4326)

# Compute geometric intersections across route geometries
intersection_geom <- st_intersection(all_routes)

# Extract POINT / MULTIPOINT crossings where lines intersect
intersection_points <- st_collection_extract(intersection_geom, "POINT") %>%
  ensure_crs(4326) %>%
  distinct() %>%
  mutate(intersection_id = row_number())

# Map each crossing point to intersecting routes
intersect_matrix <- st_intersects(intersection_points, all_routes)

intersection_summary <- intersection_points %>%
  mutate(
    intersecting_routes = map(intersect_matrix, ~ unique(all_routes$route_short_name[.x])),
    agency_mix          = map(intersect_matrix, ~ unique(all_routes$agency[.x])),
    route_count         = lengths(intersecting_routes)
  ) %>%
  filter(route_count >= 2) %>%
  st_cast("POINT") %>% # Splits any MULTIPOINT features into individual POINT rows
  mutate(
    intersection_id = row_number(),
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

print(intersection_summary)

#Note - Need to work on grouping intersection points withing a buffer distance

#============================================================
#Work in progress: code to make an interactive leaflet web map displaying transit routes and route-intersection points over an OpenStreetMap basemap.

# Create clean popup labels for the intersection points
intersection_map_data <- intersection_summary %>%
  mutate(
    routes_label = sapply(intersecting_routes, paste, collapse = ", "),
    agency_label = sapply(agency_mix, paste, collapse = " / "),
    popup_text = paste0(
      "<b>Intersection #", intersection_id, "</b><br>",
      "<b>Routes (", route_count, "):</b> ", routes_label, "<br>",
      "<b>Agencies:</b> ", agency_label
    )
  )

# Color palette by agency
agency_pal <- colorFactor(
  palette = c("KCM" = "#007A33", "ST" = "#005596", "OTHER" = "#666666"),
  domain = all_routes$agency
)

# Render interactive map with OpenStreetMap basemap
leaflet() %>%
  addTiles() %>% # Standard OpenStreetMap basemap
  # Layer 1: Transit routes/lines
  addPolylines(
    data = all_routes,
    color = ~agency_pal(agency),
    weight = 2.5,
    opacity = 0.65,
    label = ~paste0(agency, " Route ", route_short_name)
  ) %>%
  # Layer 2: Intersection points
  addCircleMarkers(
    data = intersection_map_data,
    radius = ~pmin(3 + route_count, 10),
    color = "#D95F02",
    fillColor = "#D95F02",
    fillOpacity = 0.85,
    stroke = TRUE,
    weight = 1,
    popup = ~popup_text
  ) %>%
  addLegend(
    position = "bottomright",
    pal = agency_pal,
    values = all_routes$agency,
    title = "Transit Agency"
  )



