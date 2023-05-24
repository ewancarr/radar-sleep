# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(conflicted)
library(tidyverse)
library(here)
library(dtplyr)
library(naniar)
library(janitor)
conflicts_prefer(dplyr::filter,
                 lubridate::hour,
                 lubridate::minute,
                 lubridate::wday,
                 dplyr::first,
                 dplyr::lag)
verbose <- FALSE
source(here("functions.R"))

# Load prepared data
sleep <- readRDS(here("a-data", "clean", "sleep.rds"))
survey <- readRDS(here("a-data", "clean", "survey.rds"))

# Merge survey data (every 3 months) with sleep data (daily) ------------------

# For a given window (e.g. 4 weeks), create a data frame for each individual
# containing the dates within that window. We then merge these dates with the 
# corresponding sleep data.

expand_individual <- function(d, ...) {
    pmap_dfr(list(win_start = d$win_start,
                  win_end = d$win_end,
                  t = d$t,
                  user_id = d$user_id),
             function(win_start,
                      win_end,
                      ids_date,
                      t,
                      user_id) {
               data.frame(user_id = user_id,
                          t = t,
                          merge_date = seq(win_start,
                                           win_end,
                                           by = "1 day"))
             })
}

lookup_table <- survey |>
  select(user_id, t, ids_date) |>
  mutate(win_start = ids_date + weeks(-4),
         win_end = ids_date + weeks(0)) |>
  drop_na(win_start, win_end) |>
  group_by(user_id) |>
  group_split() |>
  map_dfr(expand_individual) |>
  as_tibble()

sel <- left_join(lookup_table,
                 sleep,
                 by = c("user_id", "merge_date"),
                 multiple = "all")

# Select a single sleep event per day -----------------------------------------

# We've discussed various ways to solve this. For now, I'm selecting the single
# longest sleep event, with the idea being that this captures the main sleep
# event, and secondary sleep events are naps, etc.

# Not ideal, but arguably better than combining sleep events, esp. when it 
# isn't straightforward (e.g. the mean of two sleep onsets).

sel <- sel |>
  drop_na(user_id, merge_date, total_sleep_time) |>
  group_by(user_id, merge_date) |>
  mutate(longest_sleep_event = max(total_sleep_time, na.rm = TRUE)) |>
  filter(total_sleep_time == longest_sleep_event) |>
  distinct(user_id, t, merge_date, .keep_all = TRUE)

stopifnot(nrow(sel) == nrow(distinct(sel, user_id, t, merge_date)))

# Remove sleep events that occur before the baseline assessment ---------------

date_of_baseline <- survey |>
  filter(t == 0) |>
  distinct(user_id, baseline = ids_date)

sel <- full_join(sel, date_of_baseline, join_by("user_id"))
sel$invalid <- sel$merge_date < sel$baseline
sel <- filter(sel, !invalid)

# Select/rename required sleep variables --------------------------------------

sel <- sel |>
  select(user_id,
         t,
         weekend,
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

# Count the number of days/month where sleep information is provided ----------

sel <- sel |>
  group_by(user_id, t) |>
  mutate(n_weekdays = sum(!(wday(unique(merge_date),
                                 label = TRUE) %in% weekend)),
         n_days = length(unique(merge_date))) |>
  ungroup()

# Remove time periods with fewer than 8 days of data ---------------------------

sel <- filter(sel, n_days >= 8)

###############################################################################
####                                                                      #####
####                       Derive daily sleep measures                    #####
####                                                                      #####
###############################################################################

# Total sleep time, time in bed -----------------------------------------------

sel$tst <- sel$tst / 3600      # Total sleep time (converting minutes to hours)
sel$tib <- sel$tib / 3600      # Time in bed (converting minutes to hours)

# Sleep fragmentation index ---------------------------------------------------

sel$sfi <- sel$awak / sel$tst

# Sleep midpoint --------------------------------------------------------------

sel$smid <- derive_midpoint(sel$start_sleep, sel$stop_sleep)

# Trim extreme values
sel$smid <- winsor(sel$smid, c(0, 12))

# Sleep onset/offset ----------------------------------------------------------

sel$son_scaled <- ifelse(sel$son < 12,
                         sel$son + 12,
                         sel$son - 12)

###############################################################################
####                                                                      #####
####                            Social jet lag                            #####
####                                                                      #####
###############################################################################

# This represents the mean difference between the midpoint of sleep at weekends
# vs. weekdays in each period. 

# Note that since we're working with daily data here, the 'social jet lag for
# this period' is repeated across all days in a given period.

sjl <- sel |> 
  arrange(user_id, t) |>
  summarise(mid_sjl = mean(smid, na.rm = TRUE),
         .by = c(user_id, t, weekend)) |>
  select(user_id, t, weekend, mid_sjl) |>
  pivot_wider(names_from = weekend,
              values_from = mid_sjl) |>
  mutate(sjl = `TRUE` - `FALSE`,
         sjl = winsor(sjl, c(-5, 5))) |>
  select(user_id, t, sjl) |>
  drop_na()

sel <- left_join(sel, sjl, by = c("user_id", "t"))

###############################################################################
####                                                                      #####
####             Summarise sleep measures for this time period            #####
####                                                                      #####
###############################################################################

# The above code derives sleep measures in the last month of each 3-monthly
# window:
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

# This function takes the daily sleep measures for each 3-monthly period and
# derives the required summary variables -- e.g., median or variance for the
# period.

# We're doing this twice: for (1) weekdays only; and (2) for weekdays and
# weekends.

first_nm <- \(x) first(na.omit(x))
median_nm <- \(x) median(x, na.rm = TRUE)
var_nm <- \(x) var(x, na.rm = TRUE)

# ======================== START OF FUNCTION ==================================
summarise_sleep <- function(d, prefix) {
  # Step 1: Derive medians for variables needed below
  # =================================================
  # (i.e., need 'son_rel' before we can calculate 'son_rel_var')

  d <- d |>
    group_by(user_id, t) |>
    mutate(across(c(smid, son, soff), median_nm, .names = "{.col}_med"))

  # Step 2: calculate summaries for this time period
  # ================================================

  d <- d |> 
    group_by(user_id, t) |>
    mutate(smid_med = median_nm(smid),
           smid_rel = smid - smid_med) |>
    summarise(# First non-missing value
              across(c(sjl, n_days, n_weekdays, smid_rel, smid_med,
                       son_med, soff_med),
                     first_nm),
              # Median
              across(c(tst, tib, son, son_scaled, 
                       soff, slpeff, sol, sfi, awak),
                     median_nm,
                     .names = "{.col}_med"),
              # Variance
              across(c(tst, tib, slpeff, son, soff, smid, sol),
                     var_nm,
                     .names = "{.col}_var"),
                     .groups = "keep")

  # Step 3: Calculate lagged values
  # ===============================

  lagged <- c("sjl", "tst_med", "tst_var", "tib_med", "slpeff_med", "sol_med",
              "sol_var", "smid_med", "smid_var", "son_med", "soff_med",
              "son_var", "soff_var", "sfi_med", "awak_med")

  d$orig <- TRUE
  d <- d |> 
    group_by(user_id) |>
    expand(t = seq(3, 24, 3)) |>
    full_join(d, by = c("user_id", "t")) |>
    arrange(user_id, t) |>
    mutate(# Calculate change, this 3-month period versus last 3-month period
           across(all_of(lagged),
                  \(x) { x - lag(x) },
                  .names = "cm3_{.col}"),
           across(c(son_med, soff_med), lag, .names = "lag_{.col}")) |>
    filter(orig)

  # Step 4: Derive measures based on clock time
  # ===========================================

  d$cm3_son_med <- apply(d[, c("lag_son_med", "son_med")], 1, clock_diff)
  d$cm3_soff_med <- apply(d[, c("lag_soff_med", "soff_med")], 1, clock_diff)

  # Step 5: Trim some extreme values
  # ================================

  d$cm3_soff_med <- winsor(d$cm3_soff_med, c(-4, 4))
  d$sol_var <- ifelse(d$sol_var == 0, 0.001, d$sol_var)

  # Step 5: Sort and rename
  # =======================

  add_prefix <- \(v, prefix) {
    if_else(v %in% c("user_id", "t", "merge_date", "next_day",
                     "n_weekdays", "n_days", "weekend"),
            v,
            str_glue("{prefix}{v}"))
  }

  d <- arrange(d, user_id, t)
  names(d) <- add_prefix(names(d), prefix)

  return(ungroup(d))
}
# ======================== END OF FUNCTION ====================================

# Use this function twice, for "weekdays only" and "weekdays and weekends":

# 1. Weekdays only

sleep_wd <- sel |>
  filter(!weekend) |>
  summarise_sleep(prefix = "wd_")

# 2. Weekdays and weekends

sleep_we <- sel |>
  summarise_sleep(prefix = "we_")

# Combine

sleep <- bind_cols(sleep_wd,
                   select(sleep_we, -c(user_id, t, n_days, n_weekdays)))


###############################################################################
####                                                                      #####
####                          Prepare survey data                         #####
####                                                                      #####
###############################################################################

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  ifelse(length(x) > 0, x[1], NA)
}

# 1. Fill missing values of age, gender, years of education
# 2. Recode site

survey <- survey |>
  arrange(user_id, t) |>
  group_by(user_id) |>
  mutate(across(c(age, male, edyrs, partner), first_nonmissing),
         site_dam = site == "AMSTERDAM",
         site_spain = site == "CIBER")

###############################################################################
####                                                                      #####
####                            Merge and save                            #####
####                                                                      #####
###############################################################################

# Merge sleep measures with survey data

merged <- survey |>
  select(user_id, 
         t,
         age,
         male,
         edyrs,
         partner,
         audit,
         ids_date,
         ids_total,
         lag_ids_total,
         ids_nosleep,
         lag_ids_nosleep,
         rel,
         rel_mod,
         det,
         starts_with("med_"),
         melancholic_depression,
         atyp,
         sunshine,
         site,
         site_dam,
         site_spain) |>
  group_by(user_id) |>
  arrange(user_id, t) |>
  full_join(sleep, by = c("user_id", "t"))

# Create numeric ID

merged$pid <- as.numeric(as.factor(merged$user_id))

###############################################################################
####                                                                      #####
####                                 Save                                 #####
####                                                                      #####
###############################################################################

saveRDS(merged, file = here("a-data", "clean", "merged.rds"))
