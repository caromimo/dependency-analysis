# setup
rm(list = ls())

# libraries
library(tidyverse)
library(ggthemes)
library(extrafont)

# import the datasets
data <- read_csv("~/projects/dependency-analysis/data/raw/pkgs.csv", col_names = c("package_name", "version", "time_stamp"))

# remove some left over headers
data <- data %>%
  filter(version != "created") %>%
  filter(version != "modified")

