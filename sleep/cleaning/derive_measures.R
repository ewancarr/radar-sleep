# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(dtplyr)
library(lubridate)
library(naniar)
library(janitor)

load(here("data", "clean", "opts.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                          Tidy sleep measures                         #####
####                                                                      #####
###############################################################################

derive_midpoint <- function(start, stop) {
  half <- round(0.5 * (interval(start, stop) / minutes(1)))
  midpoint <- start + minutes(half)
  return(hour(midpoint) + (minute(midpoint) / 60))
}

derive_sleep_measures <- function(d) {
  d %>%
    mutate(total_hours = total_sleep_time / 3600,
           time_in_bed = time_in_bed / 3600,
           sleep_fragmentation_index = awakenings / total_hours,
           sleep_onset_latency_minutes = interval(in_bed, start_sleep) / minutes(1),
           sleep_onset_latency_hours = sleep_onset_latency_minutes / 60,
           sleep_midpoint = derive_midpoint(start_sleep, stop_sleep))
}

opts$last_month <- derive_sleep_measures(opts$last_month)

###############################################################################
####                                                                      #####
####               Derive sleep measures for chosen window                #####
####                                                                      #####
###############################################################################

sleep_data <- opts$last_month
min_days <- 7

# Select only observations with minimum required days of data -----------------

sel <- sleep_data %>%
  group_by(user_id, t) %>%
  drop_na(total_hours) %>%
  summarise(n_days = length(unique(merge_date))) %>%
  filter(n_days >= min_days) %>%
  left_join(sleep_data, by = c("user_id", "t"))

# Rename some variables -------------------------------------------------------

sel <- rename(sel,
              tst = total_hours,
              tib = time_in_bed,
              slpeff = sleep_efficiency,
              sol = sleep_onset_latency_hours,
              sfi = sleep_fragmentation_index,
              son = sleep_onset,
              soff = sleep_offset,
              smid = sleep_midpoint,
              insom = insomnia,
              hysom = hypersomnia,
              awak = awakenings)

# Combine multiple events per day ---------------------------------------------

perday <- sel %>%
  drop_na(user_id, t, merge_date, participant_name) %>%
  group_by(user_id, t, merge_date) %>%
  lazy_dt() %>%
  summarise(across(c(start_sleep, stop_sleep, smid), ~ first(na.omit(.x))),
            across(c(tst, tib, awak), sum, na.rm = TRUE),
            across(c(slpeff, sol, son, soff, sfi), mean, na.rm = TRUE),
            across(c(insom, hysom), max, na.rm = TRUE)) %>%
  as_tibble()

# Derive "Social Jet Lag" -----------------------------------------------------

# Absolute value of the difference in the midpoint of sleep times between
# weekdays and weekends.

sjl <- sel %>%
  mutate(day_label = wday(merge_date, label = TRUE),
         weekend = if_else(day_label %in% c("Fri", "Sat"),
                           "wkend", "wkday")) %>%
  group_by(user_id, t, weekend) %>%
  summarise(mid_sjl = mean(smid, na.rm = TRUE)) %>%
  select(user_id, t, weekend, mid_sjl) %>%
  pivot_wider(names_from = "weekend",
              values_from = "mid_sjl") %>%
  mutate(sjl = wkend - wkday) %>%
  select(user_id, t, sjl)

sel <- left_join(sel, sjl, by = c("user_id", "t"))

# Summarise sleep measures for this time period -------------------------------

sleep_vars <- sel %>%
  group_by(user_id, t) %>%
  mutate(awake_at_2am = hour(start_sleep) < 2 & hour(stop_sleep) > 2,
         abs_son = son - median(son, na.rm = TRUE),
         abs_soff = soff - median(soff, na.rm = TRUE)) %>%
  summarise(sjl = first(na.omit(sjl)),
            across(c(tst, tib, slpeff, sol, sfi, son, soff, awak),
                   median,
                   na.rm = TRUE,
                   .names = "{.col}_med"),
            across(c(insom, hysom),
                   mean, na.rm = TRUE,
                   .names = "{.col}_prop"),
            across(c(sol, tst, tib, slpeff, smid, abs_son, abs_soff),
                   var, na.rm = TRUE,
                   .names = "{.col}_var"),
            sri = mean(awake_at_2am, na.rm = TRUE)) %>%
  arrange(user_id, t)

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


###############################################################################
####                                                                      #####
####                               Outcomes                               #####
####                                                                      #####
###############################################################################

# Relapse definitions ---------------------------------------------------------

# We're considering two: 
# 
# 1. The original definition as per Matcham et al. (2019):
# 
#   The presence of MDD during follow-up will be defined as meeting criteria
#   for MDD according to the Wold Health Organisation’s Composite International
#   Diagnostic Interview - Short Form (CIDI-SF; [44]). [...] Additionally, a
#   score of > 25 on the the Inventory of Depressive Symptomatology –
#   Self-Reported (IDS-SR; [45]) will be required to establish that the
#   severity of the depressive episode is at least moderate.
#
# 2. A modified definition, that requires participants in remission to also
#    have an IDS score of <= 25. 
# 
# ╔══════════════╦═══════════════════════════════╦══════════════════════════════════════╗
# ║              ║ Original definition           ║ Modified definition                  ║
# ╠══════════════╬═══════════════════════════════╬══════════════════════════════════════╣
# ║ y = 1        ║ Meeting criteria for CIDI-SF  ║ The same.                            ║
# ║              ║ AND scoring > 25 on IDS SR    ║                                      ║
# ╠══════════════╬═══════════════════════════════╬══════════════════════════════════════╣
# ║ y = 0        ║ Not meeting criteria for      ║ Not meeting criteria for relapse     ║
# ║              ║ relapse (i.e. everyone else). ║ AND scoring ≤ 25 on IDS-SR           ║
# ╠══════════════╬═══════════════════════════════╬══════════════════════════════════════╣
# ║ y = Excluded ║ None.                         ║ Not meeting criteria for relapse AND ║
# ║              ║                               ║  scoring > 25 on IDS-SR              ║
# ╚══════════════╩═══════════════════════════════╩══════════════════════════════════════╝

# Derive alternative outcome measures -----------------------------------------

survey <- survey %>%
  group_by(user_id, t) %>%
  mutate(relb = case_when(rel == 0 & ids_total <= 25 ~ 0,
                          rel == 1 & ids_total > 25 ~ 1,
                          TRUE ~ NA_real_))

# Merge sleep measures with survey data ---------------------------------------

merged <- survey %>%
  select(user_id, t, age, male, edyrs, wsas, gad,
         ids_total, rel, relb, det) %>%
  group_by(user_id) %>%
  mutate(across(c(male, edyrs), ~ dplyr::first(na.omit(.x))),
         age_at_enrolment = min(age, na.rm = TRUE),
         age_this_window = age_at_enrolment + (t / 12)) %>%
  left_join(sleep_vars, by = c("user_id", "t"))

# Calculate change ------------------------------------------------------------

# For sleep variables, this is the changes from M3 [previous window] to M3
# [current window], i.e. sleep 12-15 weeks ago vs. sleep 0-4 weeks ago. 

# For outcomes (i.e. ids_total) this is change between the current assessment
# vs. the previous assessmetn (12 weeks ago).

merged$orig <- TRUE
merged <- expand(merged, t = seq(3, 24, 3)) %>%
  full_join(merged, by = c("user_id", "t")) %>%
  arrange(user_id, t) %>%
  mutate(across(c(sjl:sri, ids_total), ~ .x - lag(.x), .names = "cm3_{.col}"),
         across(starts_with("cm3_"), ~ abs(.x), .names = "{.col}_abs")) %>%
  filter(orig) %>%
  select(-orig)

# Save ------------------------------------------------------------------------

save(merged, file = here("data", "clean", "merged.Rdata"))
