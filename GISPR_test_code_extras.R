#===============================================================

# 1. Clean and prepare data with explicit character conversion
plot_data <- last_stops_per_window %>%
  ungroup() %>%
  mutate(
    time_window = as.character(time_window),
    trip_id = as.character(trip_id)
  )

# 2. Build the ggplot animation with group = trip_id
anim <- ggplot() +
  # Background: Route shapes for all active routes (static)
  geom_sf(data = route_shapes_sf, color = "grey80", linewidth = 0.5, alpha = 0.6) +
  # Foreground: Trip stop locations tracked across states by trip_id
  geom_point(
    data = plot_data, 
    aes(x = stop_lon, y = stop_lat, color = time_window, group = trip_id), 
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

# 3. Render and save the animation as a GIF
animate(anim, nframes = 60, fps = 10, width = 700, height = 600, renderer = gifski_renderer("kc_metro_9pm_animation.gif"))


#==============================================================


# 1. Define target date (representative weekday)
target_date <- target_dates[3]

# 2. Extract stop times for the 9:00 PM - 10:00 PM window (21:00:00 to 22:00:00)
window_stops <- kcm$stop_times %>%
  mutate(
    h = as.numeric(substr(arrival_time, 1, 2)),
    m = as.numeric(substr(arrival_time, 4, 5)),
    sec_from_midnight = h * 3600 + m * 60,
    bin_minute = floor(m / 5) * 5,
    time_window = sprintf("21:%02d", bin_minute)
  ) %>%
  filter(sec_from_midnight >= 75600 & sec_from_midnight < 79200) %>%
  inner_join(kcm$trips %>% select(trip_id, route_id, service_id), by = "trip_id", relationship = "many-to-many") %>%
  left_join(kcm$routes %>% select(route_id, route_short_name), by = "route_id") %>%
  inner_join(kcm$.$servicepatterns, by = "service_id", relationship = "many-to-many") %>%
  inner_join(kcm$.$dates_servicepatterns, by = "servicepattern_id", relationship = "many-to-many") %>%
  mutate(date_char = as.character(ymd(date))) %>%
  filter(date_char == target_date)

# 3. Join with stop coordinates
stop_coords <- kcm$stops %>% select(stop_id, stop_lat, stop_lon)

trip_spatial_data <- window_stops %>%
  left_join(stop_coords, by = "stop_id") %>%
  arrange(trip_id, stop_sequence)

# 4. For each 5-min window, find the LAST stop a trip passed by, 
# ensuring clean character types and no lingering group attributes
last_stops_per_window <- trip_spatial_data %>%
  group_by(time_window, trip_id) %>%
  slice_max(stop_sequence, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    time_window = as.character(time_window),
    trip_id = as.character(trip_id)
  )

# 5. Extract route shapes for active routes using shapes_as_sf()
active_route_ids <- unique(window_stops$route_id)
active_shape_ids <- kcm$trips %>% 
  filter(route_id %in% active_route_ids) %>% 
  pull(shape_id) %>% 
  unique()

route_shapes_sf <- kcm$shapes %>% 
  filter(shape_id %in% active_shape_ids) %>% 
  shapes_as_sf()

# 6. Build the gganimate object
anim <- ggplot() +
  # Background: Route shapes for all active routes
  geom_sf(data = route_shapes_sf, color = "grey80", linewidth = 0.5, alpha = 0.6) +
  # Foreground: Trip stop locations tracked seamlessly by group = trip_id
  geom_point(
    data = last_stops_per_window, 
    aes(x = stop_lon, y = stop_lat, color = time_window, group = trip_id), 
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

# 7. Render and save the animation as a GIF
animate(
  anim, 
  nframes = length(unique(last_stops_per_window$time_window)) * 10, 
  fps = 10, 
  width = 700, 
  height = 600, 
  renderer = gifski_renderer("kc_metro_9pm_animation.gif")
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

