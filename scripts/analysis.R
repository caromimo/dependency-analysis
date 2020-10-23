# setup
rm(list = ls())

# libraries
library(tidyverse)
library(lubridate)
library(semver)

# import the datasets
data <- read_csv("~/projects/dependency-analysis/data/processed/clean_dataset.csv")

# plot the number of updates per date
data %>%
  group_by(date) %>%
  tally() %>%
  ggplot(aes(date, n)) +
  geom_col() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  #scale_x_date(date_labels = "%Y") +
  labs(
    x = "Date", 
    y = "Number of updates"
  )

data %>% 
  count(package_name, sort = TRUE) %>%
  mutate(package_name = fct_reorder(package_name, n)) %>%
  #filter(n >= 10) %>%
  ggplot(aes(n, package_name)) +
  geom_col() +
  labs(
    title = "Number of versions between 2010 and 2020", 
    x = "Number of versions", 
    y = "Package name"
  ) + 
  theme_minimal()

# time to next major update
data %>%
  filter(date >= "2015-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) %>% 
  ggplot(
    aes(x=days_until_next_major_version)
    ) +
    geom_histogram()

data %>%
  filter(date >= "2015-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) %>%
  group_by(date) %>%
  tally() %>%
  ggplot(aes(date, n)) +
  geom_col() +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  #scale_x_date(date_labels = "%Y") +
  labs(
    x = "Date", 
    y = "Number of updates"
  )    