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



