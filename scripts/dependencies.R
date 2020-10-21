# setup
rm(list = ls())

# libraries
library(tidyverse)
library(lubridate)

# import the datasets
pkgs <- read_csv("~/projects/dependency-analysis/data/raw/pkgs.csv", col_names = c("package_name", "version", "time_stamp"))

# remove some left over headers and extract date
data <- pkgs %>%
  filter(version != "created") %>%
  filter(version != "modified") %>%
  mutate(
    date = as.Date(time_stamp, "%Y/%m/%d"),
    version = str_replace(version, "(\\d{1,2}.\\d{1,2}.\\d{1,2})(\\w+)", "\\1-\\2")
  )

# extracting the versions numbers with semver
data <- add_column(
  data,
  pull(
    data, version) %>%
    semver::parse_version() %>%
    as.data.frame() 
    ) %>%
  arrange(major, minor, patch)

data %>% write_csv("~/projects/dependency-analysis/data/processed/clean_dataset.csv")