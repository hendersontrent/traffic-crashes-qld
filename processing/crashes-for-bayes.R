#------------------------------------------
# This script sets out to preprocess some
# data for use in Bayesian modelling
#
# Data source: https://www.data.qld.gov.au/dataset/crash-data-from-queensland-roads/resource/e88943c0-5968-4972-a15f-38e120d72ec0
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 4 September 2020
#------------------------------------------

d <- read_csv("data/locations.csv") %>%
  clean_names() %>%
  filter(crash_year > 2010)

#------------------------- PRE PROCESSING --------------------------

# Daily

d1 <- d %>%
  mutate(crash_month_short = substr(crash_month, start = 1, stop = 3)) %>%
  mutate(crash_date = paste0("01-",crash_month_short,"-",crash_year)) %>%
  mutate(crash_date = as.Date(crash_date, format = "%d-%b-%Y")) %>%
  mutate(crash_day_of_week = factor(crash_day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                  "Thursday", "Friday", "Saturday"))) %>%
  group_by(crash_date, crash_day_of_week, crash_severity) %>%
  summarise(counter = n()) %>%
  ungroup()

# Monthly

d2 <- d %>%
  mutate(crash_month_short = substr(crash_month, start = 1, stop = 3)) %>%
  mutate(crash_date = paste0("01-",crash_month_short,"-",crash_year)) %>%
  mutate(crash_date = as.Date(crash_date, format = "%d-%b-%Y")) %>%
  mutate(year = gsub("-.*", "\\1", crash_date)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(crash_month_int = case_when(
          crash_month_short == "Jan" ~ 1,
          crash_month_short == "Feb" ~ 2,
          crash_month_short == "Mar" ~ 3,
          crash_month_short == "Apr" ~ 4,
          crash_month_short == "May" ~ 5,
          crash_month_short == "Jun" ~ 6,
          crash_month_short == "Jul" ~ 7,
          crash_month_short == "Aug" ~ 8,
          crash_month_short == "Sep" ~ 9,
          crash_month_short == "Oct" ~ 10,
          crash_month_short == "Nov" ~ 11,
          crash_month_short == "Dec" ~ 12)) %>%
  group_by(crash_date, crash_month_int, year, crash_severity) %>%
  summarise(counter = n()) %>%
  ungroup()

# Remove massive file from environment

rm(d)
