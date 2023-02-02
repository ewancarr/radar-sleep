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
library(future.apply)
plan(multicore)
verbose <- FALSE
weekend <- c("Fri", "Sat")
source(here("sleep", "functions.R"))

# Load prepared data
sleep <- readRDS(here("data", "clean", "sleep.rds"))
survey <- readRDS(here("data", "clean", "survey.rds"))

# Merge 3-monthly survey data with sleep data ---------------------------------

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

sleep_4w <- left_join(lookup_table,
                      sleep,
                      by = c("user_id", "merge_date"),
                      multiple = "all")

# Select/rename required variables --------------------------------------------

sel <- sleep_4w |>
  select(user_id,
         t,
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

sel <- sel |>
  drop_na(user_id, merge_date, tst) |>
  group_by(user_id, merge_date) |>
  mutate(longest_sleep_event = max(tst, na.rm = TRUE)) |>
  filter(tst == longest_sleep_event) |>
  distinct(user_id, t, merge_date, .keep_all = TRUE)

stopifnot(nrow(sel) == nrow(distinct(sel, user_id, t, merge_date)))

# Count the number of days/month where sleep information is provided ----------

sel <- sel |>
  group_by(user_id, t) |>
  mutate(n_weekdays = sum(!(lubridate::wday(unique(merge_date),
                                            label = TRUE) %in% weekend)),
         n_days = length(unique(merge_date))) |>
  ungroup()

# Remove time periods with fewer than 5 days of data ---------------------------

sel <- filter(sel, n_weekdays >= 5)

# Convert to data.table -------------------------------------------------------

sel <- setDT(sel)

# Total sleep time / time in bed ----------------------------------------------

sel$tst <- sel$tst / 3600              # Total sleep time (minutes --> hours)
sel$tib <- sel$tib / 3600              # Time in bed (minutes --> hours)

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

# Relative sleep onset/offset -------------------------------------------------

# Difference between start of sleep and [that person's] median start of
# sleep [within this time period]. Variance value of days within time
# period.

tdiff <- function(i) {
  start_time <- as.POSIXct(i[[1]] * 3600, origin = i[1])
  end_time <- as.POSIXct(i[[2]] * 3600, origin = i[2])
  return(as.numeric(difftime(start_time,
                             end_time,
                             units = "hours"))) 
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
  opts <- list(c("2021-01-01", "2021-01-01"),   # i.
               c("2021-01-01", "2021-01-02"),   # ii.
               c("2021-01-02", "2021-01-01"))   # iii.
  res <- vector(length = 3)
  for (o in seq_along(opts)) {
    end_time <- as.POSIXct(i[1] * 3600, origin = opts[[o]][1])
    start_time <- as.POSIXct(i[2] * 3600, origin = opts[[o]][2])
    res[o] <- as.numeric(difftime(start_time, end_time, units = "hours"))
  }
  return(as.numeric(res[which(abs(res) == min(abs(res)))])[1])
}

# Calculate median onset/offset per monthly period
sel[, by = .(user_id, t),
    c("son_med", "soff_med") := .(median(son, na.rm = TRUE),
                                  median(soff, na.rm = TRUE))]

# Calculate relative onset/offset
sel$son_rel <- future_apply(sel[, c("son", "son_med")], 1, clock_diff)
sel$soff_rel <- future_apply(sel[, c("soff", "soff_med")], 1, clock_diff)

# Social Jet Lag --------------------------------------------------------------

# Absolute value of the difference in the midpoint of sleep times between
# weekdays and weekends.

sjl <- sel
sjl$day_label <- lubridate::wday(sjl$merge_date, label = TRUE)
sjl$weekend <- sjl$day_label %in% weekend
sjl <- sjl[order(user_id, t)][, .(mid_sjl = mean(smid, na.rm = TRUE)),
                              by = .(user_id, t, weekend)]
sjl <- sjl[, .(user_id, t, weekend, mid_sjl)]
sjl <- dcast(sjl, ... ~ weekend, value.var = "mid_sjl")
sjl[, sjl := `TRUE` - `FALSE`]

# Winsorise extreme values
sjl$sjl <- winsor(sjl$sjl, c(-5, 5))

# Merge with other sleep measures
sjl <- na.omit(sjl, cols = c("user_id", "t", "sjl"))
sel <- merge(sel, sjl, all.x = TRUE, by = c("user_id", "t"))

# Awake at 2am ----------------------------------------------------------------

# (Not sure if we're using this).

sel$awake_2am <- (lubridate::hour(sel$start_sleep) < 2) & 
                 (lubridate::hour(sel$stop_sleep) > 2)

# Summarise sleep measures for this time period -------------------------------

# Select weekdays only
sel$day <- as.character(lubridate::wday(sel$merge_date, label = TRUE))
sel <- sel[!(day %chin% weekend)] # NOTE: we're treating Friday and Saturday 
                                  # night as the weekend. So Sunday night 
                                  # is included.

# Derive 'median sleep midpoint', needed when summarising, below.
sel[, by = .(user_id, t), smid_med := median(smid, na.rm = TRUE)]

# Summarise sleep measures for each period (t), per person (user_id).
sleep_vars <- sel[,
                  by = .(user_id, t),
                  .(# First non-missing value
                    sjl = first(na.omit(sjl)),
                    n_days = first(na.omit(n_days)),
                    n_weekdays = first(na.omit(n_weekdays)),
                    # Maximum 
                    hysom_ever = max(hysom, na.rm = TRUE),
                    # Median
                    tst_med = median(tst, na.rm = TRUE),
                    tib_med = median(tib, na.rm = TRUE),
                    son_med = median(son, na.rm = TRUE),
                    son_scaled_med = median(son_scaled, na.rm = TRUE),
                    soff_med = median(soff, na.rm = TRUE),
                    slpeff_med = median(slpeff, na.rm = TRUE),
                    sol_med = median(sol, na.rm = TRUE),
                    sfi_med = median(sfi, na.rm = TRUE),
                    awak_med = median(awak, na.rm = TRUE),
                    smid_med = median(smid, na.rm = TRUE),
                    # Variance
                    tst_var = var(tst, na.rm = TRUE),
                    tib_var = var(tib, na.rm = TRUE),
                    slpeff_var = var(slpeff, na.rm = TRUE),
                    son_rel_var = var(son_rel, na.rm = TRUE),
                    soff_rel_var = var(soff_rel, na.rm = TRUE),
                    son_var = var(son, na.rm = TRUE),
                    soff_var = var(soff, na.rm = TRUE),
                    smid_rel_var = var(smid - smid_med, na.rm = TRUE),  
                    smid_var = var(smid, na.rm = TRUE),  
                    sol_var = var(sol, na.rm = TRUE),
                    # Mean
                    sri = mean(awake_2am, na.rm = TRUE)
                    )
                  ]
sleep_vars <- sleep_vars[order(user_id, t)]

sleep_vars <- as_tibble(sleep_vars)

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
#


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
#   score of > 25 on the the Inventory of Depressive Symptomatology –
#   Self-Reported (IDS-SR; [45]) will be required to establish that the
#   severity of the depressive episode is at least moderate.
#
# 2. A modified definition, that requires participants in remission to also
#    have an IDS score of <= 25. 
# 
# ════════ ══════════════════════════════ ═════════════════════════════════════
#          Original definition             Modified definition                 
# ════════ ══════════════════════════════ ═════════════════════════════════════
#  y = 1   Meeting criteria for CIDI-SF    The same.                           
#          AND scoring > 25 on IDS SR                                          
# ════════ ══════════════════════════════ ═════════════════════════════════════
#  y = 0   Not meeting criteria for        Not meeting criteria for relapse    
#          relapse (i.e. everyone else).   AND scoring ≤ 25 on IDS-SR          
# ════════ ══════════════════════════════ ═════════════════════════════════════
#  y = NA  None.                           Not meeting criteria for relapse AND
#                                           scoring > 25 on IDS-SR             
# ════════ ══════════════════════════════ ═════════════════════════════════════

# Merge sleep measures with survey data ---------------------------------------

merged <- survey |>
  select(user_id, 
         t,
         age,
         male,
         edyrs,
         partner,
         audit,
         ids_total,
         ids_nosleep,
         rel,
         det,
         starts_with("med_"),
         melancholic_depression,
         atypical_depression,
         sunshine,
         site,
         site_dam,
         site_spain) |>
  group_by(user_id) |>
  arrange(user_id, t) |>
  full_join(sleep_vars, by = c("user_id", "t"))

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
         across(c(sjl, 
                  tst_med, tst_var,
                  tib_med, 
                  slpeff_med,
                  sol_med, sol_var,
                  smid_med, smid_var,
                  son_rel_var, soff_rel_var,
                  son_var, soff_var, 
                  sfi_med, 
                  awak_med,
                  hysom_ever,
                  ids_total), 
                ~ .x - dplyr::lag(.x), .names = "cm3_{.col}"),
         across(c(ids_total, ids_nosleep, son_med, soff_med),
                dplyr::lag, 1, .names = "lag_{.col}"))

# For others, calculate difference in clock time
merged$cm3_son_med <- future_apply(merged[, c("lag_son_med", "son_med")],
                                   1, clock_diff)
merged$cm3_soff_med <- future_apply(merged[, c("lag_soff_med", "soff_med")],
                                    1, clock_diff)

# Remove extra rows created by "expand", above.
merged <- filter(merged, orig & !is.na(user_id))

# Trim some extreme values ----------------------------------------------------

# Winsorize change in sleep offset
merged$cm3_soff_med <- winsor(merged$cm3_soff_med, c(-4, 4))

merged$sol_var <- ifelse(merged$sol_var == 0, 0.001, merged$sol_var)
merged$smid_rel_var <- ifelse(merged$smid_rel_var == 0, 
                              0.001, 
                              merged$smid_rel_var)

###############################################################################
####                                                                      #####
####                          Depression outcomes                         #####
####                                                                      #####
###############################################################################

merged <- merged |>
  group_by(user_id, t) |>
  mutate(rel = as.logical(rel),
   rel_mod = case_when( # Modified relapse definition -------------------------
   rel  & ids_total > 25   ~ 1,         # Meets relapse, current IDS > 25.
   rel  & ids_total <= 25  ~ -99,       # Meets relapse, current IDS ≤ 25.
   !rel & ids_total <= 25  ~ 0,         # No relapse, current IDS ≤ 25.
   !rel & ids_total > 25   ~ NA_real_), # No relapse, current IDS > 25
   rel_3cat = case_when( # 3-category relapse outcome -------------------------
   rel  & ids_total > 25   ~ 2,
   rel  & ids_total <= 25  ~ -99,
   !rel & ids_total <= 25  ~ 0,
   !rel & ids_total > 25   ~ 1),
 rel_5cat = case_when(# 5-category relapse outcome ----------------------------
   !rel & ids_total <= 25 & lag_ids_total <= 25 ~ 0,
   !rel & ids_total <= 25 & lag_ids_total  > 25 ~ 1,
   !rel & ids_total  > 25 & lag_ids_total <= 25 ~ 2,
   !rel & ids_total  > 25 & lag_ids_total  > 25 ~ 3,
   rel                                          ~ 4))

merged$rel_5cat <- factor(merged$rel_5cat,
                          levels = 0:4,
                          labels = c("No change, low symptoms",
                                     "Improvement",
                                     "Worsening",
                                     "No change, high symptoms",
                                     "Relapse"))
  
###############################################################################
####                                                                      #####
####                          Depression subtypes                         #####
####                                                                      #####
###############################################################################

# NOTE: if subtype is missing, use PREVIOUS non-missing value for participant

merged <- merged |>
  arrange(user_id, t) |>
  group_by(user_id, t) |>
  fill(atypical_depression, .direction = "up") |>
  rename(atyp = atypical_depression)

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

saveRDS(merged, file = here("data", "clean", "merged.rds"))
