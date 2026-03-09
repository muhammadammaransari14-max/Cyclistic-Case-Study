# Load essential libraries for data manipulation and date-time parsing
library(tidyverse)
library(lubridate)

# Define the path to the directory containing the 2024 and 2025 CSV files
# Note: Ensure the working directory is set correctly before running
file_paths <- list.files(path = "raw_data/cyclistic_2024_2025", 
                         pattern = "\\.csv$", 
                         full.names = TRUE)

# Import and combine all 24 monthly CSV files into one unified data frame
all_trips <- file_paths %>%
  map_df(~read_csv(.))

# Verify the structure and column names of the combined dataset
glimpse(all_trips)


# Clean the dataset by removing duplicates and NA values
cleaned_trips <- all_trips %>%
  # Remove any exact duplicate rows
  distinct() %>% 
  # Drop rows missing crucial station identification 
  # (often occurs with electric bikes left outside docks)
  drop_na(start_station_name, end_station_name, start_station_id, end_station_id)

# Create new columns to facilitate detailed analysis
processed_trips <- cleaned_trips %>%
  mutate(
    # Ensure standard POSIXct date-time formatting
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    
    # Calculate the exact duration of each ride in minutes
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    
    # Extract the day of the week (e.g., Monday, Tuesday)
    day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
    
    # Extract the month for seasonality tracking (e.g., January, February)
    month = month(started_at, label = TRUE, abbr = FALSE)
  )
# Remove the few rows where the date-time failed to parse
processed_trips <- processed_trips %>%
  drop_na(started_at, ended_at)
# Filter out erroneous ride lengths to finalize the dataset
final_trips <- processed_trips %>%
  filter(
    # Remove negative or zero-minute rides (false starts or maintenance)
    ride_length > 0,
    # Remove rides lasting longer than 24 hours (1440 minutes) 
    # to exclude unreturned or stolen bikes
    ride_length <= 1440 
  )

# Review the summary statistics of the finalized dataset
summary(final_trips$ride_length)


# Calculate average ride length by member type
ride_length_summary <- final_trips %>%
  group_by(member_casual) %>%
  summarise(
    average_duration_mins = mean(ride_length),
    median_duration_mins = median(ride_length),
    max_duration_mins = max(ride_length),
    total_rides = n()
  ) %>%
  arrange(member_casual)

# View the summary table
print(ride_length_summary)


# Calculate total rides by day of the week and user type
weekday_summary <- final_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(member_casual, day_of_week)



# Calculate total rides by month and user type
monthly_summary <- final_trips %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  arrange(month)



# Extract hour and calculate rides by hour of the day
hourly_summary <- final_trips %>%
  mutate(hour_of_day = hour(started_at)) %>%
  group_by(member_casual, hour_of_day) %>%
  summarise(number_of_rides = n(), .groups = 'drop')

# Calculate bike type preference
bike_type_summary <- final_trips %>%
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n(), .groups = 'drop')



#share



# Visualize average ride length
ggplot(data = ride_length_summary, aes(x = member_casual, y = average_duration_mins, fill = member_casual)) +
  geom_col(width = 0.5) +
  labs(title = "Average Ride Length by User Type",
       x = "User Type",
       y = "Average Duration (Minutes)") +
  theme_minimal() +
  scale_fill_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4"))



# Visualize ride frequency by weekday
ggplot(data = weekday_summary, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Ride Frequency by Day of the Week",
       x = "Day of the Week",
       y = "Total Number of Rides") +
  theme_minimal() +
  scale_fill_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize ride frequency by month
ggplot(data = monthly_summary, aes(x = month, y = number_of_rides, group = member_casual, color = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Monthly Ride Frequency (Seasonality)",
       x = "Month",
       y = "Total Number of Rides") +
  theme_minimal() +
  scale_color_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualize peak usage hours
ggplot(data = hourly_summary, aes(x = hour_of_day, y = number_of_rides, color = member_casual)) +
  geom_line(size = 1.2) +
  labs(title = "Peak Usage Hours by User Type",
       x = "Hour of the Day (0-23)",
       y = "Total Number of Rides") +
  theme_minimal() +
  scale_color_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4")) +
  scale_x_continuous(breaks = seq(0, 23, by = 2))


# Visualize bike type preference
ggplot(data = bike_type_summary, aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Usage by Bike Type",
       x = "Bike Type",
       y = "Total Number of Rides") +
  theme_minimal() +
  scale_fill_manual(values = c("casual" = "#F8766D", "member" = "#00BFC4"))
