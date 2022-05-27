# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(dtplyr)
library(lubridate)
library(naniar)
library(janitor)
library(data.table)
verbose <- FALSE

load(here("data", "clean", "opts.Rdata"), verbose = TRUE)

derive_midpoint <- function(start, stop) {
  half <- round(0.5 * (interval(start, stop) / minutes(1)))
  midpoint <- start + minutes(half)
  return(hour(midpoint) + (minute(midpoint) / 60))
}

weekend <- c("Fri", "Sat")

# Select window ---------------------------------------------------------------

sleep_data <- opts$last_month

# Select only observations with minimum required days of data -----------------

# NOTE: this is set to a minimum number of WEEKDAYS.

min_days <- 7

sel <- sleep_data |>
  group_by(user_id, t) |>
  drop_na(total_sleep_time) |>
  summarise(n_weekdays = sum(!(lubridate::wday(unique(merge_date), label = TRUE) %in% weekend)),
            n_days = length(unique(merge_date))) |>
  filter(n_weekdays >= min_days) |>
  left_join(sleep_data, by = c("user_id", "t"))
  
# Select/rename required variables --------------------------------------------

sel <- select(sel,
              user_id,
              t,
              n_days,
              merge_date,
              next_day,
              tst = total_sleep_time,
              tib = time_in_bed,
              slpeff = sleep_efficiency,
              sol,
              start_sleep,
              stop_sleep,
              son = sleep_onset,
              soff = sleep_offset,
              insom = insomnia,
              hysom = hypersomnia,
              awak = awakenings)

# Select a single sleep event per day -----------------------------------------

# We've discussed various ways to solve this. For now, I'm selecting the single
# longest sleep event, with the idea being that this captures the main sleep
# event, and secondary sleep events are naps, etc.

# Not ideal, but arguably better than combining sleep events, esp. when it 
# isn't straightforward (e.g. the mean of two sleep onsets).

sel <- na.omit(sel, cols = c("user_id", "merge_date", "tst"))
setDT(sel)[,
           c("n_events", "longest") := .(.N, max(tst, na.rm = TRUE)),
           by = .(user_id, merge_date)]
sel <- sel[tst == longest, head(.SD, 1), by = .(user_id, merge_date)]

# Check: this should contain 0 rows
sel[, by = .(user_id, merge_date), count := .N][count > 1, ]


# Derive some measures --------------------------------------------------------

sel$tst <- sel$tst / 3600              # Total sleep time (minutes --> hours)
sel$tib <- sel$tib / 3600              # Time in bed (minutes --> hours)
sel$sfi <- sel$awak / sel$tst          # Sleep Fragmentation Index (SFI)
sel$smid <- derive_midpoint(sel$start_sleep, sel$stop_sleep)

# Relative sleep onset/offset -------------------------------------------------

# Difference between start of sleep and [that person's] median start of
# sleep [within this time period]. Variance value of days within time
# period.

clock_diff <- function(i) {
  # Function to calculate difference in hours between two 24 hour clocks.
  from <- min(i)
  to <- max(i)
  d_fw <- (from + 24) - to
  d_bw <- to - from
  return(min(c(d_fw, d_bw)))
}


clock_diff <- function(i) {
  # This isn't pretty, but...
  # ---------------------------------------------------------------------------
  # I needed away to calculate the difference between two 24 hour clocks. i.e.
  # 23 vs. 02 = +3
  # 05 vs. 21 = -8
  # The approach taken here is to find the smallest interval between 'a' and
  # and 'b' assuming that these clock times are:
  # i.    On the same day
  # ii.   'a' is day before 'b'
  # iii.  'b' is day before 'a'
  # The function then returns the smallest absolute interval
  opts <- list(i = c("2021-01-01", "2021-01-01"),
               ii = c("2021-01-01", "2021-01-02"),
               iii = c("2021-01-02", "2021-01-01"))
  res <- map_dbl(opts, function(o) {
                   start_time <- as.POSIXct(i[[1]] * 3600, origin = o[1])
                   end_time <- as.POSIXct(i[[2]] * 3600, origin = o[2])
                   return(as.numeric(difftime(start_time,
                                              end_time,
                                              units = "hours"))) })
  return(as.numeric(res[which(abs(res) == min(abs(res)))])[1])
}

# Check that the above function is behaving correctly
if (verbose) {
  cross_df(list(a = 1:24, b = 24:1)) |>
    rowwise() |>
    mutate(diff = clock_diff(c(a, b))) |>
    ggplot(aes(x = a,
               y = b,
               label = diff,
               color = diff)) +
      geom_text() +
      theme_minimal() +
      scale_x_continuous(breaks = 1:24) +
      scale_y_continuous(breaks = 1:24)  +
      labs(x = "T1", y = "T2")
}

sel <- sel |>
  group_by(user_id, t) |>
  mutate(# Calculate median onset/offset per monthly period
         son_med = median(son, na.rm = TRUE),
         soff_med = median(soff, na.rm = TRUE))
sel$son_rel <- apply(sel[, c("son", "son_med")], 1, clock_diff)
sel$soff_rel <- apply(sel[, c("soff", "soff_med")], 1, clock_diff)

# Social Jet Lag --------------------------------------------------------------

# Absolute value of the difference in the midpoint of sleep times between
# weekdays and weekends.

sjl <- sel |>
  mutate(day_label = lubridate::wday(merge_date, label = TRUE),
         weekend = if_else(day_label %in% c("Fri", "Sat"), "wkend", "wkday")) |>
  group_by(user_id, t, weekend) |>
  summarise(mid_sjl = mean(smid, na.rm = TRUE)) |>
  select(user_id, t, weekend, mid_sjl) |>
  pivot_wider(names_from = "weekend",
              values_from = "mid_sjl") |>
  mutate(sjl = wkend - wkday) |>
  select(user_id, t, sjl)

sel <- left_join(sel, sjl, by = c("user_id", "t"))

# Summarise sleep measures for this time period -------------------------------

sleep_vars <- sel |>
  # DECIDE: only summarise weekday sleep, or use all days? --------------------
  filter(!(lubridate::wday(merge_date, label = TRUE) %in% weekend)) |>
  group_by(user_id, t) |>
  mutate(awake_at_2am = hour(start_sleep) < 2 & hour(stop_sleep) > 2) |>
  summarise(sjl = first(na.omit(sjl)),
            hysom_ever = max(hysom, na.rm = TRUE),
            across(c(tst, tib, slpeff, sol, sfi, awak, smid),
                   median,
                   na.rm = TRUE,
                   .names = "{.col}_med"),
            across(c(sol, tst, tib, slpeff, son_rel, soff_rel),
                   var, na.rm = TRUE,
                   .names = "{.col}_var"),
            sri = mean(awake_at_2am, na.rm = TRUE),
            smid_var = var(smid - smid_med),  
            across(c(son_med, soff_med), first)) |>
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

survey <- survey |>
  group_by(user_id, t) |>
  mutate(relb = case_when(rel == 0 & ids_total <= 25 ~ 0,
                          rel == 1 & ids_total > 25 ~ 1,
                          TRUE ~ NA_real_))

# Merge sleep measures with survey data ---------------------------------------

merged <- survey |>
  select(user_id, t, age, male, edyrs, wsas, gad,
         ids_total, rel, relb, det,
         medication_category,
         melancholic_depression,
         atypical_depression) |>
  group_by(user_id) |>
  mutate(across(c(male, edyrs), ~ dplyr::first(na.omit(.x))),
         age = min(age, na.rm = TRUE),
         age_this_window = age + (t / 12)) |>
  left_join(sleep_vars, by = c("user_id", "t"))

# Calculate change (in sleep measures, IDS total score) -----------------------

# For sleep variables, this is the changes from M3 [previous window] to M3
# [current window], i.e. sleep 12-15 weeks ago vs. sleep 0-4 weeks ago. 

# For outcomes (i.e. ids_total) this is change between the current assessment
# vs. the previous assessment (12 weeks ago).

merged$orig <- TRUE
merged <- tidyr::expand(merged, t = seq(3, 24, 3)) |>
  full_join(merged, by = c("user_id", "t")) |>
  arrange(user_id, t) |>
  mutate(# For some variable, calculate raw change (i.e. current minus lagged)
         across(c(sjl, tst_med, tib_med, slpeff_med,
                  sol_med, sfi_med, awak_med,
                  hysom_ever, sol_var, tst_var,
                  ids_total), 
                ~ .x - dplyr::lag(.x), .names = "cm3_{.col}"),
         lag_ids_total = dplyr::lag(ids_total, 1),
         across(c(ids_total, son_med, soff_med), dplyr::lag, 1,
                .names = "lag_{.col}"),
         across(starts_with("cm3_"), ~ abs(.x), .names = "{.col}_abs"))
merged$cm3_son_med <- apply(merged[, c("lag_son_med", "son_med")], 1, clock_diff)
merged$cm3_soff_med <- apply(merged[, c("lag_soff_med", "soff_med")], 1, clock_diff)
merged <- filter(merged, orig & !is.na(user_id))

###############################################################################
####                                                                      #####
####                          Depression subtypes                         #####
####                                                                      #####
###############################################################################

merged <- merged |>
  mutate(subtype = case_when(melancholic_depression & atypical_depression ~ "Both",
                             melancholic_depression ~ "Melancholic",
                             atypical_depression ~ "Atypical",
                             TRUE ~ "Neither"),
         subtype = factor(subtype, levels = c("Neither",
                                              "Atypical",
                                              "Melancholic",
                                              "Both")))

###############################################################################
####                                                                      #####
####                                 Misc.                                #####
####                                                                      #####
###############################################################################

# Create numeric ID -----------------------------------------------------------

merged$pid <- as.numeric(as.factor(merged$user_id))

###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

save(merged, file = here("data", "clean", "merged.Rdata"))
