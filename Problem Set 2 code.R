library(readr)
library(tidyverse)
library(dplyr)
library(survey)
gss <- read_csv("gss.csv")

gss_cleaned <- gss %>%
  select(feelings_life,income_family,own_rent,average_hours_worked) %>%
  drop_na(feelings_life) %>%
  mutate(own_rent = coalesce(own_rent, "Don't know"),
         average_hours_worked = coalesce(average_hours_worked, "Don't know"),
         own_rent = ifelse(own_rent == "Owned by you or a member of this household, even if it i...", "Own", 
                           ifelse(own_rent == "Rented, even if no cash rent is paid", "Rent", "Don't know")))

N = 30538825
n = 20331
fpc.srs = rep(N, n)
ucla.design <- svydesign(id=~1, data=gss_cleaned, fpc=fpc.srs)
mylogit <- svyglm(feelings_life ~ as.factor(income_family) + as.factor(own_rent) + as.factor(average_hours_worked), 
                  ucla.design)

summary(mylogit)

ggplot(gss_cleaned, aes(x=factor(feelings_life), fill=own_rent)) + 
  geom_bar() +
  labs(title= "Count of feelings of life categorized by whether they own or rent",
         x = "Feelings of life", y = "Count", fill = "Housing Status")
  
ggplot(gss_cleaned, aes(x=factor(feelings_life), fill=income_family)) + 
  geom_bar() +
  labs(title= "Count of feelings of life categorized income of the household",
       x = "Feelings of life", y = "Count", fill = "Income of Entire Household")

ggplot(gss_cleaned, aes(x=factor(feelings_life), fill=average_hours_worked)) + 
  geom_bar() +
  labs(title= "Count of feelings of life categorized by average working hours",
       x = "Feelings of life", y = "Count", fill = "Average Working Hours")





