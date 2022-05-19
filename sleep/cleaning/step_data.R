# Title:        Import FitBit step data, needed to calculate Sleep Onset Latency
# Author:       Ewan Carr
# Started:      2022-04-04

library(tidyverse)
library(here)
library(lubridate)
library(fs)

# We're using the FitBit minute-by-minute step data to derive sleep onset
# latency (SoL). SoL is defined here as the time between the last recorded step
# and the 'sleep onset' from the sleep features dataset. 

# This is based on the raw FitBit step data. It's huge (>120 GB). As such, the
# source data are not stored in this directory, but instead, are kept in
# another local folder (i.e. not synced between devices). To replicate this
# study it'll therefore be need to first download the raw data from the
# Synology server. Specifically, we're using data from:
#
#   RADAR-CNS/RADAR-P/MDD/connect_fitbit_intraday_steps   
# 
# On my campus PC, these are stored locally as:
#
#   ~/Documents/data/RADAR/step_data
#
# Since the data > RAM, we need to process each participant sequentially.

load(here("data", "clean", "days_lookup.Rdata"), verbose = TRUE)

process_single_participant <- function(i, files, local, days) {
  if (i %in% files) {
    # Get their sleep days/site from lookup table
    lookup <- days[days$user_id == i, ]
    if (is.na(lookup$site[[1]])) {
          return("Site is missing.")
    }
    # Load step data for this participant
    temp <- read_csv(str_glue("{local}/{i}.csv"))
    tz_i <- case_when(lookup$site[[1]] == "KCL" ~ "Europe/London",
                      lookup$site[[1]] == "CIBER" ~ "Europe/Madrid",
                      lookup$site[[1]] == "VUmc" ~ "Europe/Amsterdam")
    temp$value_time <- as_datetime(temp$value.time)
    temp$value_time <- force_tz(temp$value_time, tz = tz_i)
    temp$step_day <- as_date(temp$value_time)
    temp <- select(temp,
                   user_id = key.userId,
                   value_time,
                   step_day,
                   steps = value.steps)
    temp <- temp[temp$steps != 0, ]
    result <- left_join(lookup, temp,
                        by = c("user_id", "sleep_day" = "step_day"))
    rm(list = c("temp", "lookup"))
    gc()
    return(result)
  } else {
    return("Participant not found.")
  }
}

participants <- unique(days$user_id)
local <- "~/Documents/data/RADAR/step_data"
files <- dir_ls(local, glob = "*.csv") %>% path_file() %>% path_ext_remove()

by_participant <- map(participants,
                      ~ process_single_participant(.x, files, local, days))

participants_with_data <- keep(by_participant, is.list)
steps <- data.table::rbindlist(participants_with_data) %>% as_tibble()

save(steps, file = here("data", "clean", "step_data.Rdata"))
