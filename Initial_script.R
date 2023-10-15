# 0.0 Clean memory and restart session
rm(list = ls())
.rs.restartR()

# Libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(janitor)
library(vroom)


# 1. Importing data
nyc_collisions <- read_csv(file = "nyc_car_collisions.csv") %>% janitor::clean_names() 

# 2. Pivoting the data into a Long format

# 2.1 Pivoting the street variables
nyc_c_streets <- 
  nyc_collisions %>% 
  mutate(crash_date = mdy(crash_date), 
         crash_time = hms(crash_time), 
         cross_street_name = ifelse(!is.na(cross_street_name), 
                                    paste0(on_street_name, "-", cross_street_name),
                                    cross_street_name),
         on_street_name = ifelse(!is.na(cross_street_name), 
                                    NA,
                                 on_street_name)) %>% 
  pivot_longer(cols = contains("street"),
               names_to = "street_category", 
               values_to = "street_name") %>% 
  filter(!is.na(street_name))


# 2.2 Pivoting the severity variables
nyc_c_victims <- 
  nyc_c_streets %>% 
  select(-c(number_of_persons_injured, number_of_persons_killed)) %>% 
  pivot_longer(cols = starts_with("number_of"),
             names_to = "severity_category", 
             values_to = "severity_number") %>% 
  separate_wider_regex(cols = severity_category, c(victim_type = ".*", "_", severity = ".*")) %>% 
  mutate(victim_type = str_remove(victim_type, "number_of_"))


# 2.3 Pivoting the contributing factor variables
nyc_reason_contribution <- 
  nyc_c_victims %>% 
  mutate(contributing_factor_vehicle_1 = ifelse(is.na(contributing_factor_vehicle_1) & 
                                                  is.na(contributing_factor_vehicle_2) & 
                                                  is.na(contributing_factor_vehicle_3) & 
                                                  is.na(contributing_factor_vehicle_4) & 
                                                  is.na(contributing_factor_vehicle_5),
                                                "No information for collision", 
                                                contributing_factor_vehicle_1)) %>%
  pivot_longer(cols = starts_with("contributing_factor"),
               names_to = "contributing_factor_vehicle_num", 
               values_to = "contributing_factor_vehicle_reason") %>%
  filter(!is.na(contributing_factor_vehicle_reason)) %>% 
  mutate(contributing_factor_vehicle_num = str_remove(contributing_factor_vehicle_num, "contributing_factor_vehicle_")) 


# 2.4 Pivoting the vehicle type variables
nyc_vehicle_type <- 
  nyc_reason_contribution %>% 
  mutate(vehicle_type_code_1 = ifelse(is.na(vehicle_type_code_1) &
                                        is.na(vehicle_type_code_2) &
                                        is.na(vehicle_type_code_3) &
                                        is.na(vehicle_type_code_4) &
                                        is.na(vehicle_type_code_5),
                                      "No information about vehicle", 
                                      vehicle_type_code_1)) %>% 
  pivot_longer(cols = starts_with("vehicle_type"),
               names_to = "vehicle_category_code", 
               values_to = "vehicle_category") %>% 
  filter(!is.na(vehicle_category))






