
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

#====================================================================
#Merge two GTFS feeds in R 
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

#do the merge:
# Prefix IDs so they never collide
kcm_pref <- prefix_gtfs(kcm, "1")
st_pref  <- prefix_gtfs(gtfs_ST, "2")

# Combine shared tables
shared_tables <- intersect(names(kcm_pref), names(st_pref))
#shared_tables <- shared_tables[shared_tables != "."] # Exclude internal tidytransit list

merged_gtfs <- list()
for (tbl in shared_tables) {
  if (is.data.frame(kcm_pref[[tbl]]) && is.data.frame(st_pref[[tbl]])) {
    merged_gtfs[[tbl]] <- bind_rows(kcm_pref[[tbl]], st_pref[[tbl]])
  }
}

# 4. Restore tidytransit class and rebuild service patterns
merged_gtfs <- as_tidygtfs(merged_gtfs)
merged_gtfs <- set_servicepattern(merged_gtfs)



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
# 1. Map trips to dates through service patterns to see daily service volume

trip_dates <- merged_gtfs$trips %>%
  select(trip_id, service_id) %>%
  left_join(merged_gtfs$.$servicepatterns, by = "service_id", relationship = "many-to-many") %>%
  left_join(merged_gtfs$.$dates_servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
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

# 2. Join stop_times ONLY for the filtered subset of trips
trip_start_hours <- merged_gtfs$stop_times %>%
  filter(stop_sequence == 1) %>%
  inner_join(active_trips_on_target_dates, by = "trip_id") %>%
  mutate(
    # Extract leading digits before first colon to handle GTFS 24+ hour service (e.g., 25:30:00 -> 1)
    hour = as.numeric(sub(":.*", "", departure_time)) %% 24
  )

# 3. Aggregate trip counts by day type, route_short_name, and hour
route_hourly_trips <- trip_start_hours %>%
  group_by(day_type, route_short_name, hour) %>%
  summarise(trip_count = n(), .groups = "drop")

# 4. Extract top 10 routes and order factors by volume
top_routes_summary <- route_hourly_trips %>%
  filter(!is.na(route_short_name)) %>%
  group_by(route_short_name) %>%
  summarise(total = sum(trip_count, na.rm = TRUE), .groups = "drop") %>%
  slice_max(total, n = 10)

# 5. Complete missing day/route/hour combinations with 0 for the top routes
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

# 6. Plot the faceted hourly distribution
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
window_stops <- merged_gtfs$stop_times %>%
  mutate(
    h = as.numeric(substr(arrival_time, 1, 2)),
    m = as.numeric(substr(arrival_time, 4, 5)),
    sec_from_midnight = h * 3600 + m * 60,
    bin_minute = floor(m / 5) * 5,
    time_window = sprintf("21:%02d", bin_minute)
  ) %>%
  filter(h == 21, sec_from_midnight >= 75600, sec_from_midnight < 79200) %>%
  inner_join(merged_gtfs$trips %>% select(trip_id, route_id, service_id), by = "trip_id") %>%
  left_join(merged_gtfs$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  inner_join(merged_gtfs$.$servicepatterns, by = "service_id") %>%
  inner_join(merged_gtfs$.$dates_servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  filter(as.character(ymd(date)) == target_date)





#================================
# Does target_date match any date in dates_servicepatterns?
target_str <- as.character(target_dates)[2]
merged_gtfs$.$dates_servicepatterns %>%
  mutate(date_str = format(as.Date(date), "%Y-%m-%d")) %>%
  count(date_str == target_str)


# 1. Get active trip_ids for target_date first
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

# 2. Filter stop_times for active trips in the 21:00-22:00 window into 5-minute bins
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
#Create transfer windows

# Helper to safely assign CRS only when missing, otherwise transform to target EPSG
ensure_crs <- function(x, epsg = 4326) {
  if (is.na(st_crs(x))) {
    st_set_crs(x, epsg)
  } else {
    st_transform(x, epsg)
  }
}

# 1. Extract active route IDs as a vector and pull matching shape IDs
active_route_ids <- window_stops %>%
  filter(!is.na(route_id)) %>%
  distinct(route_id) %>%
  pull(route_id)

active_shape_ids <- merged_gtfs$trips %>%
  filter(route_id %in% active_route_ids, !is.na(shape_id)) %>%
  distinct(shape_id) %>%
  pull(shape_id)

# 2. Filter shapes with a defensive row check
filtered_shapes <- merged_gtfs$shapes %>%
  filter(shape_id %in% active_shape_ids)

if (nrow(filtered_shapes) == 0) {
  stop("Error: No matching shapes found in shapes table for these active routes.")
}

# 3. Convert filtered shapes to sf
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

# 4. Build combined routes layer and compute crossing points
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

# 5. Map each crossing point to intersecting routes
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

#============================================================
#Create transfer windows

# 1. Safely extract active route IDs and shape IDs
active_routes_ids <- window_stops %>%
  distinct(route_id, route_short_name)

active_shape_ids <- merged_gtfs$trips %>% 
  filter(route_id %in% active_routes_ids$route_id) %>% 
  pull(shape_id) %>% 
  unique()

# 2. Defensive row check on shapes
if (sum(merged_gtfs$shapes$shape_id %in% active_shape_ids) == 0) {
  stop("Error: No matching shapes found in merged_gtfs$shapes for these active routes.")
}

# 3. Helper function to extract route-level LINESTRING geometries directly via sf
get_route_shapes_sf <- function(gtfs_obj) {
  shapes_sf <- gtfs_obj$shapes %>%
    as_tibble() %>%
    filter(shape_id %in% active_shape_ids) %>%
    arrange(shape_id, shape_pt_sequence) %>%
    st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
    group_by(shape_id) %>%
    summarise(do_union = FALSE, .groups = "drop") %>%
    st_cast("LINESTRING")
  
  gtfs_obj$trips %>%
    select(trip_id, route_id, shape_id) %>%
    filter(route_id %in% active_routes_ids$route_id) %>%
    inner_join(gtfs_obj$routes %>% select(route_id, agency_id, route_short_name), by = "route_id", relationship = "many-to-many") %>%
    left_join(gtfs_obj$agency %>% select(agency_id, agency_name), by = "agency_id") %>%
    mutate(
      agency = case_when(
        grepl("King County", agency_name, ignore.case = TRUE) ~ "KCM",
        grepl("Sound Transit", agency_name, ignore.case = TRUE) ~ "ST",
        TRUE ~ coalesce(agency_name, agency_id, "OTHER")
      )
    ) %>%
    select(agency, route_id, route_short_name, shape_id) %>%
    distinct() %>%
    inner_join(shapes_sf, by = "shape_id") %>%
    st_as_sf() %>%
    group_by(agency, route_id, route_short_name) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
}


# 4. Extract all active routes from merged_gtfs
all_routes <- get_route_shapes_sf(merged_gtfs)

# 5. Reproject to UTM Zone 10N (EPSG:32610, units = meters) before intersecting
all_routes_proj <- all_routes %>%
  st_set_crs(4326) %>%
  st_transform(32610)

# Compute pairwise intersections in planar meters
intersection_geom <- st_intersection(all_routes_proj) %>%
  filter(n.overlaps >= 2)

# Split any MULTIPOINTs into individual POINTs and deduplicate
intersection_points_proj <- st_collection_extract(intersection_geom, "POINT") %>%
  st_cast("POINT") %>%
  st_geometry() %>%
  unique() %>%
  st_sfc(crs = 32610) %>%
  st_sf(geometry = .) %>%
  mutate(intersection_id = row_number())

# 6. Find which routes intersect each point (in projected meters)
intersect_matrix <- st_intersects(intersection_points_proj, all_routes_proj)

# 7. Transform back to WGS84 (CRS 4326) and build the final summary
intersection_summary <- intersection_points_proj %>%
  st_transform(4326) %>%
  mutate(
    intersecting_routes = purrr::map(intersect_matrix, ~ all_routes_proj$route_short_name[.x]),
    agency_mix          = purrr::map(intersect_matrix, ~ unique(all_routes_proj$agency[.x])),
    route_count         = lengths(intersecting_routes)
  ) %>%
  filter(route_count >= 2) %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

# Preview the resulting spatial object
print(intersection_summary)


#============================================================


# 1. Create clean popup labels for the intersection points
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

# 2. Color palette by agency
agency_pal <- colorFactor(
  palette = c("KCM" = "#007A33", "ST" = "#005596", "OTHER" = "#666666"),
  domain = all_routes$agency
)

# 3. Render interactive map with OpenStreetMap basemap
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

























