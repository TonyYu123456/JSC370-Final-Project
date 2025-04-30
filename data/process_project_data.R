library(tidyverse)
library(dplyr)
library(knitr)
library(jsonlite)
library(lubridate)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(httr)
library(ggcorrplot)
library(gridExtra)
library(patchwork)
library(RColorBrewer)
library(mgcv)       
library(caret)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)     
library(xgboost)   
library(tibble)
library(parallel)
library(microbenchmark)
library(doParallel)

# Code for API call:

# fire_data <- read.csv("Fire Incidents Data.csv")
# 
# fire_data_clean <- fire_data %>%
#   select(Latitude, Longitude, TFS_Alarm_Time, Civilian_Casualties, Estimated_Dollar_Loss) %>%
#   filter(!is.na(Latitude), !is.na(Longitude), !is.na(TFS_Alarm_Time)) %>%
#   mutate(Incident_Date = as.Date(ymd_hms(TFS_Alarm_Time))) %>%
#   arrange(TFS_Alarm_Time)

# get_weather_data <- function(lat, lon, start_date, end_date) {
#   base_url <- "https://archive-api.open-meteo.com/v1/archive"
#   
#   query_params <- list(
#     latitude = lat,
#     longitude = lon,
#     start_date = start_date,
#     end_date = end_date,
#     hourly = "temperature_2m,wind_speed_10m,precipitation",
#     timezone = "America/New_York",
#     wind_speed_unit = "ms"
#   )
  
#   response <- GET(url = base_url, query = query_params)
#   
#   if (response$status_code == 200) {
#     weather_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
#     
#     if (!is.null(weather_json$hourly)) {
#       return(tibble(
#         time = weather_json$hourly$time,
#         temperature = weather_json$hourly$temperature_2m,
#         wind_speed = weather_json$hourly$wind_speed_10m,
#         precipitation = weather_json$hourly$precipitation
#       ))
#     } else {
#       return(tibble(
#         time = NA, temperature = NA,
#         wind_speed = NA, precipitation = NA
#       ))
#     }
#   } else {
#     print(paste("Failed request for lat:", lat, "lon:", lon,
#                 "dates:", start_date, "to", end_date))
#     return(tibble(
#       time = NA, temperature = NA,
#       wind_speed = NA, precipitation = NA
#     ))
#   }
# }
# 
# Process a sample of 5000 incidents due to the limitation of API calls each time
# sample_fire_data <- fire_data_clean %>% slice(1:5000)
# 
# weather_data_list <- list()
# 
# for (i in 1:nrow(sample_fire_data)) {
#   row <- sample_fire_data[i, ]
#   
#   start_date <- as.character(row$Incident_Date)
#   end_date <- as.character(row$Incident_Date)
#   
#   weather <- get_weather_data(lat = row$Latitude, lon = row$Longitude,
#                               start_date = start_date, end_date = end_date)
#   
#   alarm_hour <- hour(ymd_hms(row$TFS_Alarm_Time))
#   
#   weather_filtered <- weather %>% 
#     mutate(weather_hour = hour(ymd_hm(time))) %>% 
#     filter(weather_hour == alarm_hour)
#   
#   if(nrow(weather_filtered) == 0) {
#     weather_filtered <- tibble(
#       time = NA, temperature = NA,
#       wind_speed = NA, precipitation = NA
#     )
#   } else {
#     weather_filtered <- weather_filtered %>% slice(1) %>% select(-weather_hour)
#   }
#   
#   combined_row <- bind_cols(row, weather_filtered)
#   weather_data_list[[i]] <- combined_row
#   
#   print(paste("Completed row", i))
# }
# 
# final_fire_weather_data <- bind_rows(weather_data_list)
# 
# Process a sample of 5000 incidents due to the limitation of API calls each time
# sample_fire_data2 <- fire_data_clean %>% slice(5001:10000)
# weather_data_list2 <- list()
# 
# for (i in 1:nrow(sample_fire_data2)) {
#   row <- sample_fire_data2[i, ]
#   
#   start_date <- as.character(row$Incident_Date)
#   end_date <- as.character(row$Incident_Date)
#   
#   weather <- get_weather_data(lat = row$Latitude, lon = row$Longitude,
#                               start_date = start_date, end_date = end_date)
#   
#   alarm_hour <- hour(ymd_hms(row$TFS_Alarm_Time))
#   
#   weather_filtered <- weather %>% 
#     mutate(weather_hour = hour(ymd_hm(time))) %>% 
#     filter(weather_hour == alarm_hour)
#   
#   if(nrow(weather_filtered) == 0) {
#     weather_filtered <- tibble(
#       time = NA, temperature = NA,
#       wind_speed = NA, precipitation = NA
#     )
#   } else {
#     weather_filtered <- weather_filtered %>% slice(1) %>% select(-weather_hour)
#   }
#   
#   combined_row <- bind_cols(row, weather_filtered)
#   weather_data_list2[[i]] <- combined_row
#   
#   print(paste("Completed row", 5000+i))
# }
# 
# final_fire_weather_data2 <- bind_rows(weather_data_list2)
# merged_data <- bind_rows(final_fire_weather_data, final_fire_weather_data2)
# write_csv(merged_data, "Fire Weather Merged Data.csv")

merged_data <- read.csv("Fire Weather Merged Data.csv")
toronto_map <- st_read("Neighbourhoods.geojson")

merged_data_modified <- merged_data %>%
  mutate(
    TFS_Alarm_Time = ymd_hms(TFS_Alarm_Time),
    Incident_Date = as.Date(Incident_Date),
    Weather_Time = ymd_hm(time),
    Temperature = temperature,
    Wind_Speed = wind_speed,
    Precipitation = precipitation
  ) %>%
  select(-time, -temperature, -wind_speed, -precipitation)

merged_data_no_NA <- merged_data_modified %>% drop_na()

merged_data_transformed <- merged_data_no_NA %>%
  mutate(Wind_Speed_Log = log(Wind_Speed + 1),
         Precipitation_Binary = ifelse(Precipitation > 0, "Rain", "No Rain"),
         Estimated_Dollar_Loss_Log = log(Estimated_Dollar_Loss + 1),
         Civilian_Casualties_Binary = 
           ifelse(Civilian_Casualties > 0, "Casualties", "No Casualties")
  )

remove_outliers_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25)
  Q3 <- quantile(data[[column]], 0.75)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  data %>% filter((.data[[column]] >= lower_bound) & (.data[[column]] <= upper_bound))
}

merged_data_no_outliers <- merged_data_transformed %>%
  remove_outliers_iqr("Temperature") %>%
  remove_outliers_iqr("Wind_Speed_Log")

cleaned_data <- merged_data_no_outliers %>%
  mutate(
    DayNight = if_else(hour(TFS_Alarm_Time) >= 6 & hour(TFS_Alarm_Time) < 18, "Day", "Night"),
    Season = case_when(
      month(Incident_Date) %in% c(12, 1, 2) ~ "Winter",
      month(Incident_Date) %in% c(3, 4, 5) ~ "Spring",
      month(Incident_Date) %in% c(6, 7, 8) ~ "Summer",
      month(Incident_Date) %in% c(9, 10, 11) ~ "Fall"
    ),
    Month = month(Incident_Date, label = TRUE, abbr = TRUE)
  ) %>%
  select(Latitude, Longitude, TFS_Alarm_Time, Incident_Date, Month, DayNight, Season,
         Temperature, Wind_Speed_Log, Precipitation_Binary, 
         Estimated_Dollar_Loss_Log, Civilian_Casualties_Binary)

set.seed(123)
n <- nrow(cleaned_data)
train_indices <- sample(seq_len(n), size = floor(0.7 * n))
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

train_data <- train_data %>%
  mutate(
    Precipitation_Binary = factor(Precipitation_Binary),
    DayNight             = factor(DayNight),
    Month                = factor(Month, levels = month.abb),
    Season               = factor(Season, levels = c("Winter","Spring","Summer","Fall"))
  )

test_data <- test_data %>%
  mutate(
    Precipitation_Binary = factor(Precipitation_Binary, levels = levels(train_data$Precipitation_Binary)),
    DayNight             = factor(DayNight, levels = levels(train_data$DayNight)),
    Month                = factor(Month, levels = levels(train_data$Month)),
    Season               = factor(Season, levels = levels(train_data$Season))
  )



