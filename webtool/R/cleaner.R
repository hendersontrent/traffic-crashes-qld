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
  mutate(crash_date = paste0("01-", crash_month_short,"-", crash_year)) %>%
  mutate(crash_date = as.Date(crash_date, format = "%d-%b-%Y"))
  
  # Convert date information to numerics for more accurate modelling
  
  tmp2 <- transform(tmp1, ndate = as.numeric(crash_date),
                    nyear = as.numeric(format(crash_date, '%Y')),
                    nmonth = as.numeric(format(crash_date, '%m')))
  
  # Individual aggregations
  
  hospital <- tmp2 %>%
    filter(crash_severity == "Hospitalisation") %>%
    group_by(ndate, nyear, nmonth, crash_severity, crash_date) %>%
    summarise(value = sum(count_casualty_hospitalised)) %>%
    ungroup()
  
  fatal <- tmp2 %>%
    filter(crash_severity == "Fatal") %>%
    group_by(ndate, nyear, nmonth, crash_severity, crash_date) %>%
    summarise(value = sum(count_casualty_fatality)) %>%
    ungroup()
  
  min_inj <- tmp2 %>%
    filter(crash_severity == "Minor injury") %>%
    group_by(ndate, nyear, nmonth, crash_severity, crash_date) %>%
    summarise(value = sum(count_casualty_minor_injury)) %>%
    ungroup()
  
  medical <- tmp2 %>%
    filter(crash_severity == "Medical treatment") %>%
    group_by(ndate, nyear, nmonth, crash_severity, crash_date) %>%
    summarise(value = sum(count_casualty_medically_treated)) %>%
    ungroup()
  
  # Bind all together and return it
  
  tmp3 <- bind_rows(hospital, fatal, min_inj, medical)
  
  return(tmp3)
}

# Aggregate data for statistical modelling

model_cleaner <- function(data, data2) {
  
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
  
  tmp2 <- bind_rows(hospital, fatal, min_inj, medical) %>%
    left_join(data2, by = c("loc_post_code" = "postcode"))
  
  return(tmp2)
}

#--------------------- MAPPING PRODUCTION --------------------------

map_cleaner <- function(data) {
  
  # High level
  
  tmp1 <- data
  
  # Individual aggregations
  
  hospital <- tmp1 %>%
    filter(crash_severity == "Hospitalisation") %>%
    group_by(crash_year, crash_severity, loc_post_code,
             loc_abs_statistical_area_2, loc_abs_statistical_area_3,
             loc_abs_statistical_area_4) %>%
    summarise(value = sum(count_casualty_hospitalised)) %>%
    ungroup()
  
  fatal <- tmp1 %>%
    filter(crash_severity == "Fatal") %>%
    group_by(crash_year, crash_severity, loc_post_code,
             loc_abs_statistical_area_2, loc_abs_statistical_area_3,
             loc_abs_statistical_area_4) %>%
    summarise(value = sum(count_casualty_fatality)) %>%
    ungroup()
  
  min_inj <- tmp1 %>%
    filter(crash_severity == "Minor injury") %>%
    group_by(crash_year, crash_severity, loc_post_code,
             loc_abs_statistical_area_2, loc_abs_statistical_area_3,
             loc_abs_statistical_area_4) %>%
    summarise(value = sum(count_casualty_minor_injury)) %>%
    ungroup()
  
  medical <- tmp1 %>%
    filter(crash_severity == "Medical treatment") %>%
    group_by(crash_year, crash_severity, loc_post_code,
             loc_abs_statistical_area_2, loc_abs_statistical_area_3,
             loc_abs_statistical_area_4) %>%
    summarise(value = sum(count_casualty_medically_treated)) %>%
    ungroup()
  
  # Bind all together and return it
  
  tmp2 <- bind_rows(hospital, fatal, min_inj, medical)
  
  return(tmp2)
}

#-----------------
# ITERATIONS
#-----------------

postcode_cleaner <- function(data){
  map_data <- d3 %>%
    filter(loc_post_code != "Unknown") %>%
    mutate(loc_post_code = as.character(loc_post_code)) %>%
    inner_join(data, by = c("loc_post_code" = "POA_CODE16"))
  
  st_as_sf(map_data, map_data$geometry)
}
