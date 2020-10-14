library(readr)
library(tidyverse)
library(dplyr)
gss <- read_csv("ps2/gss.csv")

gss_cleaned <- gss %>%
  select(feelings_life,income_family,own_rent,average_hours_worked) %>%
  drop_na(feelings_life) %>%
  mutate(own_rent = coalesce(own_rent, "Don't know"),
         average_hours_worked = coalesce(average_hours_worked, "Don't know"),
         income_1 = ifelse(income_family == "Less than $25,000", 1, 0),
         income_2 = ifelse(income_family == "$25,000 to $49,999", 1, 0),
         income_3 = ifelse(income_family == "$50,000 to $74,999", 1, 0),
         income_4 = ifelse(income_family == "$75,000 to $99,999", 1, 0),
         income_5 = ifelse(income_family == "$100,000 to $ 124,999", 1, 0),
         income_6 = ifelse(income_family == "$125,000 and more", 1, 0),
         own = ifelse(own_rent == "Owned by you or a member of this household, even if it i...", 1, 0),
         rent = ifelse(own_rent == "Rented, even if no cash rent is paid", 1, 0),
         idk = ifelse(own_rent == "Don't know", 1, 0),
         work_hours_1 = ifelse(average_hours_worked == "0 hour", 1, 0),
         work_hours_2 = ifelse(average_hours_worked == "0.1 to 29.9 hours", 1, 0),
         work_hours_3 = ifelse(average_hours_worked == "30.0 to 40.0 hours", 1, 0),
         work_hours_4 = ifelse(average_hours_worked == "40.1 to 50.0 hours", 1, 0),
         work_hours_5 = ifelse(average_hours_worked == "50.1 hours and more", 1, 0),
         work_hours_6 = ifelse(average_hours_worked == "Don't know", 1, 0)) 
  


  
