# setup
rm(list = ls())

# libraries
library(tidyverse)
library(lubridate)
library(semver)
library(scales)

# import the datasets
data <- read_csv("~/projects/dependency-analysis/data/processed/clean_dataset.csv")

# plot all versions per date since December 2010
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
    x = "Number of versions (majors, minors and patches)", 
    y = "Package name"
  ) + 
  theme_minimal()

# computer time (days) between major versions
major_versions <- data %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

# histogram of all major versions  
major_versions %>% 
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

# plot histogram of days to next major version since 2015
major_versions_since_2015 %>% 
  ggplot(
    aes(x=days_until_next_major_version)
    ) +
    geom_histogram()

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

# filter major versions in 2017
major_versions_in_2017 <- data %>%
  filter(date >= "2017-01-01") %>%
  filter(date < "2018-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  mutate(major_version = paste0(package_name, " ", major, ".", minor, ".", patch)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

###########################
#### timelines ############
###########################

#positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
positions <- c(0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0)
directions <- c(1, -1)
month_buffer <- 1
text_offset <- 0.05

# 2017 timeline

line_pos <- data.frame(
    "date"=unique(major_versions_in_2017$date),
    "position"=rep(positions, length.out=length(unique(major_versions_in_2017$date))),
    "direction"=rep(directions, length.out=length(unique(major_versions_in_2017$date)))
)

df <- merge(x=major_versions_in_2017, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, major_version)), ]

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

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position

#### PLOT ####
timeline_2017 <- ggplot(df,aes(x=date,y=0, label=major_version)) + 
  theme_classic() +
  geom_hline(yintercept=0, color = "black", size=0.3) +
  geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2) +
  geom_point(aes(y=0), size=3) +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom"
        ) +
  geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90) +
  geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black') +
  geom_text(data=df, aes(y=position,label=major_version),size=2.5)





timeline_plot<-timeline_plot+theme_classic()
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                color = "black", size=0.3)
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)
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
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black')
# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=package_name),size=2.5)
timeline_plot













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


### 2018 ###
### 2018 ###
### 2018 ###
### 2018 ###
### 2018 ###
# filter major versions in 2018
major_versions_in_2018 <- data %>%
  filter(date >= "2018-01-01") %>%
  filter(date < "2019-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

#positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
positions <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
directions <- c(1, -1)

line_pos <- data.frame(
    "date"=unique(major_versions_in_2018$date),
    "position"=rep(positions, length.out=length(unique(major_versions_in_2018$date))),
    "direction"=rep(directions, length.out=length(unique(major_versions_in_2018$date)))
)

df <- merge(x=major_versions_in_2018, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, package_name)), ]

head(df)

#month_buffer <- 1

month_date_range <- seq(min(df$date)), max(df$date)), by='month')
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
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

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

timeline_plot





2018_timeline <- df %>% 
  ggplot(aes(x=date,y=0, label=package_name)) +
  theme_classic() +
  geom_hline(
    yintercept=0, 
    color = "black",
    size=0.3) +
  geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2) +
  geom_point(aes(y=0), size=2) + 
  theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "bottom"
                ) +
  geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90) +
  geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black') +
  geom_text(aes(y=text_position,label=package_name),size=2.5)

2018_timeline










#
#
#
#

# CLEANING CODE#

# filter major versions in 2019
major_versions_in_2019 <- data %>%
  filter(date >= "2019-01-01") %>%
  filter(date < "2020-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) 

#positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
positions <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
directions <- c(1, -1)

line_pos <- data.frame(
    "date"=unique(major_versions_in_2019$date),
    "position"=rep(positions, length.out=length(unique(major_versions_in_2019$date))),
    "direction"=rep(directions, length.out=length(unique(major_versions_in_2019$date)))
)

df <- merge(x=major_versions_in_2019, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, package_name)), ]

month_buffer <- 1

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
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=2)

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

major_versions_in_2019 %>%
  ggplot(aes(x=days_until_next_major_version)) +
    geom_histogram() +
    theme_minimal() + 
    geom_histogram(binwidth = 1)

hist(
  major_versions_since_2015$days_until_next_major_version, 
  main = "Distribution of days to next major version since 2015", 
  xlab = "Days until next major version", 
  xlim = c(0,100),
  breaks = seq(0,100, 10),
  xaxp = c(0,100, 10),
  ylab = "Number of major versions", 
  ylim = c(0, 200)
  )

hist(
  major_versions_since_2015$days_until_next_major_version, 
  main = "Distribution of days to next major version since 2015", 
  xlab = "Days until next major version", 
  xlim = c(0, 100),
  breaks = seq(0, 100, 5),
  xaxp = c(0, 100, 10),
  ylab = "Number of major versions", 
  ylim = c(0, 150)
  )






# plotting the time to next major version per year (since 2011 - this excludes one data point on December 29th, 2010)
time_to_next_major_version_2011_to_2019 <- data %>%
  filter(date >= "2011-01-01") %>%
  filter(date < "2020-01-01") %>%
  filter(minor == "0") %>%
  filter(patch == "0") %>% 
  filter(is.na(prerelease)) %>% 
  mutate(year = as.factor(year(date))) %>%
  arrange(time_stamp) %>%
  mutate(
    days_until_next_major_version = as.numeric(lead(date) - date)
  ) %>%
  drop_na(days_until_next_major_version)


library(dplyr)

time_to_next_major_version_2011_to_2019 %>%
  group_by(year) %>%
  summarise(
    mean = mean(days_until_next_major_version, na.rm = TRUE),
    sd = sd(days_until_next_major_version, na.rm = TRUE)
  )




test <- group_by(time_to_next_major_version_2011_to_2019, year) %>%
  summarise(
     mean = mean(days_until_next_major_version, na.rm = TRUE),
    sd = sd(days_until_next_major_version, na.rm = TRUE)
  )

library(hrbrthemes)
library(viridis)
time_to_next_major_version_2011_to_2019 %>%
  group_by(year) %>%
  filter(date >= "2015-01-01") %>%
  ggplot(aes(x=year, y=days_until_next_major_version)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10')
  +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Time (days) to next majoy version per year") +
  xlab("Year") +
  ylab("Number of days") +
  scale_y_continuous(trans='log10')

time_to_next_major_version_2011_to_2019 %>%
  group_by(year) %>%
  tally()

time_to_next_major_version_2011_to_2019 %>%
  ggplot(aes(x=year, y=days_until_next_major_version)) + #, colour=supp)) + 
    #geom_errorbar(aes(ymin=len-se, ymax=len+se), width=.1) +
    geom_point() 


library(plyr)
per_year <- time_to_next_major_version_2011_to_2019 %>%
  ddply(c("year", summarise,
               n    = length(time_to_next_major_version_2011_to_2019),
               mean = mean(time_to_next_major_version_2011_to_2019),
               sd   = sd(time_to_next_major_version_2011_to_2019),
               se   = sd / sqrt(n)
))
per_year

cdata <- ddply(time_to_next_major_version_2011_to_2019, c("year"), summarise,
               N    = length(days_until_next_major_version),
               mean = mean(days_until_next_major_version),
               sd   = sd(days_until_next_major_version),
               se   = sd / sqrt(N)
)
cdata

# Standard error of the mean
ggplot(cdata, aes(x=year, y=mean)) + 
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point()


plot_per_year <- time_to_next_major_version_2011_to_2019 %>% 
  group_by(year) %>% 
  summarise(
    mean_days = mean(days_until_next_major_version), 
    std = sd(days_until_next_major_version)
    )
plot_per_year


data2 <- data_summary(time_to_next_major_version_2011_to_2019, varname="year)

library(dplyr)

df <- time_to_next_major_version_2011_to_2019 %>%
  group_by(year) %>%
  summarise(
    sd = sd(days_until_next_major_version, na.rm = TRUE),
    mean = mean(days_until_next_major_version, na.rm = TRUE)
  )
df