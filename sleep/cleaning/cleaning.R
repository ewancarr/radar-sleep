# Title:        Data cleaning for RADAR sleep analyses
# Author:       Ewan Carr
# Started:      2022-02-07

library(here)
library(haven)
library(janitor)
library(lubridate)
library(naniar)
library(fs)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)

# Functions -------------------------------------------------------------------

check_overlap <- function(df_left, df_right,
                          id_left, id_right) {
  full_join(data.frame(left = unique(df_left[[id_left]]),
                       in_left = TRUE),
            data.frame(right = unique(df_right[[id_right]]),
                       in_right = TRUE),
            by = c("left" = "right")) |>
  count(in_left, in_right)
}

###############################################################################
####                                                                      #####
####                           Load survey data                           #####
####                                                                      #####
###############################################################################

which_event <- function(x) {
  if_else(x == "enrolment_arm_1",
          "0",
          str_replace(x, "_month_assessmen[t]*_arm_1", ""))
}


survey <- read_dta(here("data", "raw", "extended_data_2021_09_30.dta")) |>
    rename(user_id = subject_id,
           event = redcap_event_name,
           fut = Followuptime,
           psu = PSSUQ_TOTAL,
           eth = ETHCAT2,
           tam = TAM_TOTAL,
           ids_date = IDSdate) |>
    clean_names() |>
    mutate(
        event = which_event(event),
        t = as.numeric(event),
        pid = as.numeric(as.factor(user_id)),
        male = as.numeric(gender) == 0,
        partner = as.numeric(marital_status) == 1,
        edyrs = as.numeric(education_years),
        smoker = as.numeric(mh_smoking) == 2,
        anyben = as.numeric(benefits_type_10 == 0),
        wsas = as.numeric(wsas_total),
        gad = as.numeric(gad7_total),
        phq = as.numeric(total_phq8),
        ids = as.numeric(ids_total),
        site = as.numeric(recruitmentsite),
        survey_time = ymd_hms(na_if(pssuq_timestamp, "[not completed]")),
        debt = case_when(
            utilities_10 == 1 ~ FALSE,
            utilities_10 == 0 ~ TRUE,
            TRUE ~ NA
        ),
        prevwear = as.numeric(fitness_tracker_used),
        bame = if_else(is.na(eth), NA, as.numeric(eth) %in% 2:5)
    ) |>
    drop_na(pid, t, fut)

# Get IDS items from larger survey dataset ------------------------------------

ids_items <- read_dta(here("data", "raw", "totaldataset.dta")) |>
  select(user_id = subject_id,
         event = redcap_event_name,
         starts_with("ids_")) |>
  mutate(event = which_event(event),
         t = as.numeric(event)) |>
  rowwise() |>
  mutate(# Atypical depression ------------------------------------------------
         # 1. Mood reactivity (ids_8 = 0, 1, 2)
         mood_reactivity = ids_8 %in% 0:2,
         num_trues = sum(# 2. Leaden paralysis (ids_30 = 2, 3)
                         (ids_30 %in% 2:3)) +
                         # 3. Weight gain (ids_14 = 2, 3) OR increased appetite (ids_12 = 2, 3)
                         (ids_14 %in% 2:3 | ids_12 %in% 2:3) +
                         # 4. Hypersomnia (ids_4 = 2, 3)
                         (ids_4 %in% 2:3) +
                         # 5. Interpersonal sensitivity (ids_29 = 3)
                         (ids_29 == 3),
        atypical_depression = mood_reactivity & num_trues >= 2, 
        # Melancholic depression ----------------------------------------------
        # 1. Mood reactivity (ids_8 = 2, 3) OR pleasure (ids_21 = 2, 3)
        mood_reactivity = (ids_8 %in% 2:3 | ids_21 %in% 2:3),
        num_trues = sum(# 2. Quality of mood (ids_10 = 3)
                        (ids_10 == 3) +
                        # 3. Mood variation (ids_9 = 2, 3)
                        (ids_9 %in% 2:3) +
                        # 4. Psychomotor retardation (ids_23 = 2, 3) OR Psychomotor Agitation (ids_24 = 2, 3)
                        (ids_23 %in% 2:3 | ids_24 %in% 2:3) +
                        # 5. Appetite decrease (ids_11 = 2, 3) OR Weight decrease (ids_13 = 3)
                        (ids_11 %in% 2:3 | ids_13 == 3) +
                        # 6. Self-outlook (ids_16 = 2, 3)
                        (ids_16 %in% 2:3)),
         melancholic_depression = mood_reactivity & (num_trues >= 3))

survey <- ids_items |>
  select(user_id,
         t = event,
         atypical_depression,
         melancholic_depression) |>
  mutate(t = as.numeric(t)) |>
  right_join(survey, 
             by = c("user_id", "t"))

# Get other measures from REDCAP.dta ------------------------------------------

redcap <- read_dta(here("data", "raw", "REDCAP data.dta")) |>
  mutate(t = case_when(str_detect(redcap_event_name, "enrolment_arm_1") ~ 0,
                       TRUE ~ parse_number(redcap_event_name))) |>
  rename(user_id = subject_id)

outcomes <- redcap |>
  select(user_id,
         redcap_event_name,
         contains("relapse"),
         contains("deterioration_2SDs")) |>
  pivot_longer(!c(user_id, redcap_event_name),
               names_to = "measure",
               values_to = "value") |>
  mutate(y = case_when(str_detect(measure, "deterioration_2SDs") ~ "det",
                       str_detect(measure, "relapseordeterioration") ~ "relordet",
                       str_detect(measure, "relapseanddeterioration") ~ "relanddet",
                       str_detect(measure, "relapse") ~ "rel",
                       TRUE ~ NA_character_),
         t = case_when(str_detect(measure, "threem") ~ 3,
                       str_detect(measure, "sixm") ~ 6,
                       str_detect(measure, "ninem") ~ 9,
                       str_detect(measure, "twelvem") ~ 12,
                       str_detect(measure, "eighteenm") ~ 18,
                       str_detect(measure, "fifteenm") ~ 15,
                       str_detect(measure, "twentyonem") ~ 21,
                       str_detect(measure, "twentyfourm") ~ 24,
                       str_detect(measure, "3m$") ~ 3,
                       str_detect(measure, "6m$") ~ 6,
                       str_detect(measure, "9m$") ~ 9,
                       str_detect(measure, "12m$") ~ 12,
                       str_detect(measure, "15m$") ~ 15,
                       str_detect(measure, "18m$") ~ 18,
                       str_detect(measure, "21m$") ~ 21,
                       str_detect(measure, "24m$") ~ 24,
                       TRUE ~ NA_real_)) |>
  drop_na(measure, t, y, value)

# Important: make sure we're only using the relapse/deterioration variable 
#            [WIDE format] from the corresponding month [LONG format].

outcomes <- outcomes |>
  mutate(m_row = parse_number(redcap_event_name)) |>
  filter(t == m_row)

# Check: ensure that each participant has a single, unique value of 'relapse'
# at each time point? 

outcomes |>
  group_by(user_id, t, y) |>
  summarise(single_value = length(unique(value)) == 1) |>
  filter(!single_value)

# Reshape back to WIDE
outcomes <- outcomes |>
  filter(y %in% c("rel", "det")) |>
  select(user_id, t, y, value) |>
  pivot_wider(id_cols = c("user_id", "t"),
              names_from = "y",
              values_from = "value")

# Check overlap between survey and REDCAP data. -------------------------------

# Check participants match (n=632):
table(unique(survey$user_id) %in% unique(redcap$user_id))
# --> Yep.

# Check observations match:
a <- distinct(outcomes, user_id, t) |> mutate(from_a = "outcomes")
b <- distinct(survey, user_id, t) |> mutate(from_b = "survey")
full_join(a, b) |> 
  filter(t > 0) |>
  arrange(user_id, t) |>
  count(is.na(from_a) | is.na(from_b))
# --> All but 2, which is OK for now.
# TODO: query.

survey <- survey |>
  full_join(outcomes, by = c("user_id", "t"))

print(length(unique(survey$user_id)))

###############################################################################
####                                                                      #####
####          Load medication lookup; derive medication measures          #####
####                                                                      #####
###############################################################################

# Create lookup table (this was then sent to Matthew for checking)
med_lookup <- read_csv(here("data", "raw",
                            "medications", "medkey_complete.csv")) |>
  distinct(original, correct)

# Load Matthew's categories
cat_matthew <- read_csv(here("data", "raw", "medications",
                             "Medication Types_MH.csv"),
                        col_types = "c_c",
                        col_names = c("medication_name", "cat_matthew"))

# Get medication data from REDCAP
meds <- redcap |>
  select(user_id, t, starts_with("csri_6")) |>
  rename(med_name = csri_6_1_1) |>
  mutate(med_name = na_if(med_name, ""))

# Check: are all medications in the lookup table?
not_found <- !(meds$med_name %in% med_lookup$original)
table(not_found)

# Check: which are missing?
paste(na.omit(meds$med_name[not_found]))
  
meds <- meds |>
  left_join(med_lookup, by = c("med_name" = "original")) |>
  left_join(cat_matthew, by = c("correct" = "medication_name")) |>
  select(user_id, t, med_name, correct, medication_category = cat_matthew)

# Create simplified measures --------------------------------------------------

meds <- meds |>
  mutate(meds_mdd = medication_category %in% c("antidepressant",
                                               "antipsychotic",
                                               "anticonvulsant",
                                               "stimulating antidepressant"),
         meds_sleep = medication_category %in% c("benzodiazepine",
                                                 "hypnotic"),
         meds_other = !is.na(medication_category) & !(meds_mdd | meds_sleep))

survey <- left_join(survey,
                    select(meds, user_id, t, medication_category),
                    by = c("t", "user_id"))

###############################################################################
####                                                                      #####
####                           Load FitBit data                           #####
####                                                                      #####
###############################################################################

## Check survey respondents missing IDS date ----------------------------------

missing_dates <- survey |>
  group_by(user_id, t) |>
  summarise(n_missing = sum(is.na(ids_date))) |>
  filter(n_missing > 0) |>
  spread(t, n_missing) |>
  mutate(across(`0`:`15`, replace_na, 0))
head(missing_dates)

## Import missing dates (from Faith via email) --------------------------------

fixed_dates <- read_csv(here("data", "raw", "fixed_dates.csv")) |>
  rename(user_id = subject_id) |>
  gather(t, ids_date, -user_id) |>
  filter(ids_date != 0) |>
  mutate(ids_date = dmy(ids_date),
         t = as.numeric(t))

survey <- survey |>
  mutate(ids_date = ymd(ids_date)) |>
  full_join(fixed_dates, by = c("user_id", "t")) |>
  mutate(ids_date = coalesce(ids_date.x, ids_date.y))

table(is.na(survey$ids_date))

###############################################################################
####                                                                      #####
####                            Sleep measures                            #####
####                                                                      #####
###############################################################################

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

# -----------------------------------------------------------------------------
# --------------------------- SLEEP ONSET LATENCY -----------------------------
# -----------------------------------------------------------------------------

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

# Merge with other sleep measures ---------------------------------------------

sleep <- sleep |>
  left_join(select(sol, user_id, sleep_day, sol),
             by = c("user_id", "merge_date" = "sleep_day"))

# Merge 3-monthly survey data with sleep data ---------------------------------

# TODO: explain approach taken.

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

add_sleep <- function(survey_data,
                      sleep_data,
                      w_start = -2,  # Start of window, offset from survey date
                      w_end = 0      # End of window, offset from survey date
                      ) {
  lookup <- survey_data |>
    select(ids_date, 
           t,
           user_id) |>
    mutate(win_start = ids_date + weeks(w_start),
           win_end = ids_date + weeks(w_end)) |>
    drop_na(win_start, win_end) |>
    group_by(user_id) |>
    group_split() |>
    map_dfr(expand_individual) |>
    as_tibble()
  with_sleep <- lookup |>
    left_join(sleep_data, by = c("user_id", "merge_date"))
  return(with_sleep) 
}

# Extract sleep days for required windows
windows <- list(prev_2w = c(-2, 0),
                first_month = c(-12, -8),
                last_month = c(-4, 0))

opts <- map(windows, ~ add_sleep(survey, sleep, .x[1], .x[2]))

save(opts, survey, sleep, file = here("data", "clean", "opts.Rdata"))
