# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(lubridate)
library(naniar)
library(janitor)

load(here("data", "clean", "opts.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####           Function to derive sleep measures for each window          #####
####                                                                      #####
###############################################################################

summarise_sleep <- function(sleep_data, min_days) {
  # Select only observations with minimum required days of data
  selected <- sleep_data %>%
    group_by(user_id, t) %>%
    drop_na(total_sleep_time_hours) %>%
    count(name = "n_days") %>%
    filter(n_days >= min_days) %>%
    left_join(sleep_data, by = c("user_id", "t"))
  # Rename
  selected <- rename(selected,
                     tst = total_sleep_time,
                     tib = time_in_bed,
                     slpeff = sleep_efficiency,
                     sol = sleep_onset_latency_minutes,
                     sfi = sleep_fragmentation_index_hours,
                     son = sleep_onset,
                     soff = sleep_offset,
                     insom = insomnia,
                     hysom = hypersomnia,
                     awak = awakenings)
  # Summarise measures
  selected %>%
    group_by(user_id, t) %>%
    mutate(across(c(tst, tib), ~ .x / 3600)) %>%
    summarise(across(c(tst, tib, slpeff, sol, sfi, son, soff, awak),
                     median,
                     na.rm = TRUE,
                     .names = "{.col}_med"),
              across(c(insom, hysom), 
                     mean, na.rm = TRUE,
                     .names = "{.col}_prop"),
              across(c(sol, tst, tib, slpeff),
                     var, na.rm = TRUE,
                     .names = "{.col}_var")) %>%
    return()
}

sleep_m3 <- summarise_sleep(opts$last_month, 14)

# The above function derives sleep measures in the last month of each 
# 3-monthly window:
# 
#     T1                           T2
#     │                            │
#     └────────────────┬───────────┤
#                      │ Sleep in  │
#                      │  month 3  │
#                      └───────────┘
# 
# We're going to measure changes in sleep, from month 3 in the current episode
# to month three in the next episode:
#   
#                 T1                           T2
#                 │                            │
#     ┌───────────┼────────────────┬───────────┤
#     │ Sleep in  │                │ Sleep in  │
#     │  month 3  │                │  month 3  │
#     └───────────┘                └───────────┘
#       Previous                      Current
#        window                        window
#   

# Merge with survey data ------------------------------------------------------

merged <- survey %>%
  select(user_id, t, age, male, edyrs, wsas, gad, ids_total, relb, det) %>%
  group_by(user_id) %>%
  mutate(across(c(male, edyrs), ~ first(na.omit(.x))),
         age_at_enrolment = min(age, na.rm = TRUE),
         age_this_window = age_at_enrolment + (t / 12)) %>%
  left_join(sleep_m3, by = c("user_id", "t"))

# Calculate change ------------------------------------------------------------

# For sleep variables, this is the changes from M3 [previous window] to M3
# [current window], i.e. sleep 12-15 weeks ago vs. sleep 0-4 weeks ago. 

# For outcomes (i.e. ids_total) this is change between the current assessment
# vs. the previous assessmetn (12 weeks ago).

merged <- merged %>%
  mutate(orig = TRUE, .before = 3) %>%
  complete(user_id, t = seq(0, 24, 3)) %>%
  arrange(user_id, t) %>%
  mutate(across(c(tst_med:sol_var, ids_total), ~ .x - lag(.x), .names = "cm3_{.col}"),
         across(starts_with("cm3_"), ~ abs(.x), .names = "{.col}_abs")) %>%
  filter(orig) %>%
  select(-orig)

# Save ------------------------------------------------------------------------

save(merged, file = here("data", "clean", "merged.Rdata"))
