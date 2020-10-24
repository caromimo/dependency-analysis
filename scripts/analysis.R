# setup
rm(list = ls())

# libraries
library(tidyverse)
library(lubridate)
library(semver)

# import the datasets
data <- read_csv("~/projects/dependency-analysis/data/processed/clean_dataset.csv")

# plot all versions per date
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

# plot the number of versions per package (2010-2020)
data %>% 
  count(package_name, sort = TRUE) %>%
  mutate(package_name = fct_reorder(package_name, n)) %>%
  ggplot(aes(n, package_name)) +
  geom_col() +
  labs(
    title = "Number of versions between 2010 and 2020", 
    x = "Number of versions", 
    y = "Package name"
  ) + 
  theme_minimal()

# plot histogram of time (in days) to next major version since 2015
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

# filter major versions since 2015
major_versions_since_2015 <- data %>%
  filter(date >= "2015-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

# plot major versions per date since 2015
major_versions_since_2015 %>%
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

# https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
library(scales)

# filter major versions since 2018
major_versions_since_2018 <- data %>%
  filter(date >= "2018-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
    "date"=unique(major_versions_since_2018$date),
    "position"=rep(positions, length.out=length(unique(major_versions_since_2018$date))),
    "direction"=rep(directions, length.out=length(unique(major_versions_since_2018$date)))
)

df <- merge(x=major_versions_since_2018, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, package_name)), ]

head(df)

month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
    intersect(
        ceiling_date(year_date_range, unit="year"),
        floor_date(year_date_range, unit="year")
    ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)


#### PLOT ####

timeline_plot<-ggplot(df,aes(x=date,y=0, label=package_name))
#timeline_plot<-timeline_plot+labs(col="package_name")
#timeline_plot<-timeline_plot+scale_color_manual(labels=package_name, drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "bottom"
                )

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black')
# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=package_name),size=2.5)
print(timeline_plot)