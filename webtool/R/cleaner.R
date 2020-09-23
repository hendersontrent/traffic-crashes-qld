#------------------------------------------
# This script sets out to preprocess data
# for use in the webtool
#------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 23 September 2020
#-------------------------------------------

# Aggregate data to monthly level for time series

ts_cleaner <- function(data) {
  
  # High level
  
  tmp1 <- data %>%
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
    crash_month_short == "Dec" ~ 12))
  
  # Individual aggregations
  
  hospital <- tmp1 %>%
    filter(crash_severity == "Hospitalisation") %>%
    group_by(crash_date, crash_month_int, year, crash_severity) %>%
    summarise(value = sum(count_casualty_hospitalised)) %>%
    ungroup() %>%
    mutate(date_num = as.numeric(crash_date)/1000)
  
  fatal <- tmp1 %>%
    filter(crash_severity == "Fatal") %>%
    group_by(crash_date, crash_month_int, year, crash_severity) %>%
    summarise(value = sum(count_casualty_fatality)) %>%
    ungroup() %>%
    mutate(date_num = as.numeric(crash_date)/1000)
  
  min_inj <- tmp1 %>%
    filter(crash_severity == "Minor injury") %>%
    group_by(crash_date, crash_month_int, year, crash_severity) %>%
    summarise(value = sum(count_casualty_minor_injury)) %>%
    ungroup() %>%
    mutate(date_num = as.numeric(crash_date)/1000)
  
  medical <- tmp1 %>%
    filter(crash_severity == "Medical treatment") %>%
    group_by(crash_date, crash_month_int, year, crash_severity) %>%
    summarise(value = sum(count_casualty_medically_treated)) %>%
    ungroup() %>%
    mutate(date_num = as.numeric(crash_date)/1000)
  
  # Bind all together and return it
  
  tmp2 <- bind_rows(hospital, fatal, min_inj, medical)
  
  return(tmp2)
}

# Aggregate data for statistical modelling

model_cleaner <- function(data) {
  
  # High level
  
  tmp1 <- data %>%
    mutate(crash_month_short = substr(crash_month, start = 1, stop = 3)) %>%
    mutate(crash_date = paste0("01-",crash_month_short,"-",crash_year)) %>%
    mutate(crash_date = as.Date(crash_date, format = "%d-%b-%Y")) %>%
    mutate(year = gsub("-.*", "\\1", crash_date)) %>%
    mutate(year = as.numeric(year))
  
  # Individual aggregations
  
  hospital <- tmp1 %>%
    filter(crash_severity == "Hospitalisation") %>%
    group_by(year, crash_severity, loc_post_code) %>%
    summarise(value = sum(count_casualty_hospitalised)) %>%
    ungroup()
  
  fatal <- tmp1 %>%
    filter(crash_severity == "Fatal") %>%
    group_by(year, crash_severity, loc_post_code) %>%
    summarise(value = sum(count_casualty_fatality)) %>%
    ungroup()
  
  min_inj <- tmp1 %>%
    filter(crash_severity == "Minor injury") %>%
    group_by(year, crash_severity, loc_post_code) %>%
    summarise(value = sum(count_casualty_minor_injury)) %>%
    ungroup()
  
  medical <- tmp1 %>%
    filter(crash_severity == "Medical treatment") %>%
    group_by(year, crash_severity, loc_post_code) %>%
    summarise(value = sum(count_casualty_medically_treated)) %>%
    ungroup()
  
  # Bind all together and return it
  
  tmp2 <- bind_rows(hospital, fatal, min_inj, medical)
  
  return(tmp2)
}
