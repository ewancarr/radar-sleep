# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(lubridate)
library(naniar)
library(janitor)

load(here("data", "clean", "merged.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####               Derive summary measures for 2-week period              #####
####                                                                      #####
###############################################################################

# Select participants with at least 7 days of data, per 2-week period ---------

merged <- merged %>%
  mutate(day = (interval(win_start, merge_date) / days(1)) + 1)

atleast7 <- merged %>%
  group_by(user_id, t) %>%
  summarise(n_obs = n_complete(total_sleep_time)) %>%
  filter(n_obs >= 7) 

merged <- left_join(atleast7, merged, by = c("user_id", "t")) %>%
  clean_names()

# Calculate social jet lag ----------------------------------------------------

merged <- merged %>%
  mutate(day_label = wday(merge_date, label = TRUE),
         weekend = if_else(day_label %in% c("Fri", "Sat"),
                           "wkend", "wkday"),
         across(c(total_sleep_time, time_in_bed), ~ .x / 3600),
         clock_midpoint = if_else(is.na(time_sleep_midpoint_hours),
                                  NA_real_,
                                  as.numeric(time_sleep_midpoint_hours) +
                                    (time_sleep_midpoint_minutes / 60))) 

decimal_to_clock <- function(t) {
  hrs = as.integer(t)
  mins = round((t - hrs) * 60)
  return(hm(str_glue("{hrs}:{mins}")))
}

diff_clocks <- function(t1, t2) {
  # Function to calculate hours difference between to decimal-formatted clocks
  # i.e. HH.MM where MM is 0-1 proportion of an hour.
  if(is.na(t1) | is.na(t2)) {
    return(NA) }
  else {
    start <- decimal_to_clock(t1)
    end <- decimal_to_clock(t2)
    return((end - start) / minutes(1))
  }
}


sjl <- sel %>%
  group_by(user_id, t, weekend) %>%
  summarise(mid = mean(clock_midpoint, na.rm = TRUE)) %>%
  pivot_wider(names_from = "weekend",
              values_from = "mid") %>%
  mutate(diff_min = diff_clocks(wkend, wkday))

merged <- merged %>%
  left_join(sjl, by = c("user_id", "t"))

# Summarise each sleep measure for two week period ----------------------------

av2 <- merged %>%
  group_by(user_id, t) %>%
  summarise(across(c(total_sleep_time,
                     time_in_bed,
                     sleep_efficiency,
                     sleep_onset_latency_minutes,
                     sleep_fragmentation_index_hours,
                     insomnia,
                     hypersomnia,
                     clock_midpoint,
                     sleep_onset,
                     sleep_offset,
                     sleep_fragmentation_index_hours),
                   ~ mean(.x, na.rm = TRUE),
                   .names = "{.col}_mean"),
            across(c(awakenings),
                   ~ median(.x, na.rm = TRUE),
                   .names = "{.col}_med"),
            across(c(sleep_onset_latency_minutes,
                     clock_midpoint),
                   ~ var(.x, na.rm = TRUE),
                   .names = "{.col}_var"),
            across(c(wkend, wkday, diff_min, age, gender,
                     rel, rel_alt, ids_total),
                   ~ first(na.omit(.x))))

# Save ------------------------------------------------------------------------
clean <- merged
save(clean, av2, file = here("data", "clean", "clean.Rdata"))
