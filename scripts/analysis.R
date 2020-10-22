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

