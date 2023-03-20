# Title:        Prepare sleep measures
# Author:       Ewan Carr
# Started:      2022-02-07

library(here)
library(tidyverse)
library(haven)
library(janitor)
library(lubridate)
library(naniar)
library(fs)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
source(here("functions.R"))

# Load 'sleep features' -------------------------------------------------------

# These are the daily sleep features from WP8 / Dan.

p <- here("data", "raw", "radarV1_MDD", "csv_files")
sleep <- read_csv(paste0(p, "/dailyFeatures_fitbit_sleep.csv"))
te <- read_csv(paste0(p, "/timeIntervals.csv"))
sleep <- left_join(sleep, te,
                   by = c("time_interval" = "timeInterval_ID")) |>
  clean_names()
sleep$merge_date <- sleep$datetime_start

# Format "time" variable as datetime
sleep$sleep_day <- as_date(sleep$datetime_start + hours(12))
sleep$hr <- as.numeric(substr(sleep$time, 1, 2))
sleep$min <- as.numeric(substr(sleep$time, 3, 4))
sleep$time_formatted <- (sleep$sleep_day + 
                         hours(sleep$hr + (sleep$min / 60)))

# Identify sleep events that finish in the next day
sleep$next_day <- if_else(sleep$sleep_offset < sleep$sleep_onset, 1, 0)

# Format "sleep_onset" and "sleep_offset" as datetime
extract_dt <- function(time, origin) {
  return(
    origin +
    hours(floor(time)) +  
    minutes(as.integer(time %% 1 * 60))
  )  
}

# Start sleeping
sleep$start_sleep <- extract_dt(sleep$sleep_onset, sleep$sleep_day)

# Stop sleeping
sleep$stop_sleep <- extract_dt(sleep$sleep_offset, 
                               sleep$sleep_day + days(sleep$next_day))

# Identify site 
sleep <- sleep |>
  mutate(site = case_when(project_id == "RADAR-MDD-CIBER-S1" ~ "CIBER",
                          project_id == "RADAR-MDD-CIBER-s1" ~ "CIBER",
                          project_id == "RADAR-MDD-VUmc-s1" ~ "VUmc",
                          project_id == "RADAR-MDD-KCL-s1" ~ "KCL",
                          TRUE ~ NA_character_))

# Export lookup, used when processing FitBit step data
days <- sleep |>
  select(user_id = participant_name,
         site,
         sleep_day) |>
  distinct()

save(days, file = here("data", "clean", "days_lookup.Rdata"))


###############################################################################
####                                                                      #####
####                          Sleep onset latency                         #####
####                                                                      #####
###############################################################################

# Load FitBit steps data ------------------------------------------------------

# Load pre-processed step data
load(here("data", "clean", "step_data.Rdata"), verbose = TRUE)

# NOTE: It's important to get the time zones right here. Specifically, we need:
#      
#      ┌──────── CIBER ────────┐  ┌─────── VUmc ────────────┐
#      │ Steps: UTC            │  │ Steps: UTC              │
#      │ Sleep: Europe/Madrid  │  │ Sleep: Europe/Amsterdam │
#      └───────────────────────┘  └─────────────────────────┘
#                                                            
#                    ┌───────── KCL ─────────┐               
#                    │ Steps: Europe/London  │               
#                    │ Sleep: Europe/London  │               
#                    └───────────────────────┘

# Fix time zones in steps data
steps <- steps |>
  left_join(distinct(sleep, user_id, site)) |>
  mutate(value_time_tz = case_when(
    site == "CIBER" ~ force_tz(value_time, tz = "UTC"),
    site == "VUmc" ~ force_tz(value_time, tz = "UTC"),
    site == "KCL" ~ force_tz(value_time, tz = "Europe/London")
    ))

# Fix time zones in 'sleep features' data
sleep <- sleep |> 
  mutate(start_sleep_tz = case_when(
    site == "CIBER" ~ force_tz(start_sleep, tz = "Europe/Madrid"), 
    site == "VUmc" ~ force_tz(start_sleep, tz = "Europe/Amsterdam"),  # Amsterdam
    site == "KCL" ~ force_tz(start_sleep, tz = "Europe/London")
    ))

sleep_onset <- select(sleep,
                      user_id, site, sleep_day, start_sleep, start_sleep_tz)

# Merge step data with sleep onset
ss <- inner_join(steps, sleep_onset, by = c("site", "user_id", "sleep_day")) 

# Identify last step before sleep onset ---------------------------------------

ss <- ss |>
  lazy_dt() |>
  filter(value_time_tz < start_sleep_tz) |>
  group_by(user_id, sleep_day) |>
  arrange(user_id, sleep_day, desc(value_time_tz)) |>
  slice_head(n = 1) |>
  select(user_id, site, value_time_tz, steps, sleep_day, start_sleep_tz) |>
  as_tibble()

# Derive sleep onset latency --------------------------------------------------

sol <- ss
sol$sol <- interval(sol$value_time_tz, sol$start_sleep_tz) / hours(1)

# Winsorize extreme values
sol$sol <- winsor(sol$sol, c(0, 4))

# Merge with other sleep measures ---------------------------------------------

sleep <- sleep |>
  left_join(select(sol, user_id, sleep_day, sol),
             by = c("user_id", "merge_date" = "sleep_day"))

# Save ------------------------------------------------------------------------

saveRDS(sleep, file = here("data", "clean", "sleep.rds"))
