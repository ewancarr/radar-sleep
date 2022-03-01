# Title:        Data cleaning for RADAR sleep analyses
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(haven)
library(janitor)
library(lubridate)
library(naniar)
library(patchwork)

# Functions -------------------------------------------------------------------

check_overlap <- function(df_left, df_right,
                          id_left, id_right) {
  full_join(data.frame(left = unique(df_left[[id_left]]),
                       in_left = TRUE),
            data.frame(right = unique(df_right[[id_right]]),
                       in_right = TRUE),
            by = c("left" = "right")) %>%
  count(in_left, in_right)
}

###############################################################################
####                                                                      #####
####                           Load survey data                           #####
####                                                                      #####
###############################################################################

survey <- read_dta(here("data", "raw", "extended_data_2021_09_30.dta")) %>%
    rename(user_id = subject_id,
           event = redcap_event_name,
           fut = Followuptime,
           psu = PSSUQ_TOTAL,
           eth = ETHCAT2,
           tam = TAM_TOTAL,
           ids_date = IDSdate) %>%
    clean_names() %>%
    mutate(
        event = if_else(event == "enrolment_arm_1",
            "0",
            str_replace(
                event,
                "_month_assessmen[t]*_arm_1",
                ""
            )
        ),
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
    ) %>%
    drop_na(pid, t, fut)

# Get other measures from REDCAP.dta ------------------------------------------

redcap <- read_dta(here("data", "raw", "REDCAP data.dta")) %>%
  mutate(t = case_when(str_detect(redcap_event_name, "enrolment_arm_1") ~ 0,
                       TRUE ~ parse_number(redcap_event_name))) %>%
  rename(user_id = subject_id)

outcomes <- redcap %>%
  select(user_id,
         redcap_event_name,
         contains("relapse"),
         contains("deterioration_2SDs")) %>%
  pivot_longer(!c(user_id, redcap_event_name),
               names_to = "measure",
               values_to = "value") %>%
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
                       TRUE ~ NA_real_)) %>%
  drop_na(measure, t, y, value)

# Important: make sure we're only using the relapse/deterioration variable 
#            [WIDE format] from the corresponding month [LONG format].

outcomes <- outcomes %>%
  mutate(m_row = parse_number(redcap_event_name)) %>%
  filter(t == m_row)

# Check: ensure that each participant has a single, unique value of 'relapse'
# at each time point? 

outcomes %>%
  group_by(user_id, t, y) %>%
  summarise(single_value = length(unique(value)) == 1) %>%
  filter(!single_value)

# Reshape back to WIDE
outcomes <- outcomes %>%
  filter(y %in% c("rel", "det")) %>%
  select(user_id, t, y, value) %>%
  pivot_wider(id_cols = c("user_id", "t"),
              names_from = "y",
              values_from = "value")

# Check overlap between survey and REDCAP data. -------------------------------

# Check participants match (n=632):
table(unique(survey$user_id) %in% unique(redcap$user_id))
# --> Yep.

# Check observations match:
a <- distinct(outcomes, user_id, t) %>% mutate(from_a = "outcomes")
b <- distinct(survey, user_id, t) %>% mutate(from_b = "survey")
full_join(a, b) %>% 
  filter(t > 0) %>%
  arrange(user_id, t) %>%
  count(is.na(from_a) | is.na(from_b))
# --> All but 2, which is OK for now.
# TODO: query.

survey <- survey %>%
  full_join(outcomes, by = c("user_id", "t"))

print(length(unique(survey$user_id)))


###############################################################################
####                                                                      #####
####          Load medication lookup; derive medication measures          #####
####                                                                      #####
###############################################################################

# Load lookup table
med_lookup <- read_csv(here("data", "raw",
                            "medications", "medkey_complete.csv")) %>%
  distinct(original, correct)

# Load Matthew's categories
cat_matthew <- read_csv(here("data", "raw", "medications",
                             "Medication Types_MH.csv"))

# Get medication data from REDCAP
meds <- redcap %>%
  select(user_id, t, starts_with("csri_6")) %>%
  rename(med_name = csri_6_1_1) %>%
  mutate(med_name = na_if(med_name, ""))

# Check: are all medications in the lookup table?
na.omit(meds$med_name[!(meds$med_name %in% med_lookup$original)])
  
meds <- meds %>%
  left_join(med_lookup, by = c("med_name" = "original")) %>%
  left_join(cat_matthew, by = c("correct" = "Medication Name")) %>%
  select(user_id, t, med_name, correct, medication_category = cat_matthew)

###############################################################################
####                                                                      #####
####                           Load FitBit data                           #####
####                                                                      #####
###############################################################################

## Check survey respondents missing IDS date ----------------------------------

missing_dates <- survey %>%
  group_by(user_id, t) %>%
  summarise(n_missing = sum(is.na(ids_date))) %>%
  filter(n_missing > 0) %>%
  spread(t, n_missing) %>%
  mutate(across(`0`:`15`, replace_na, 0))
head(missing_dates)

## Import missing dates (from Faith via email) --------------------------------

fixed_dates <- read_csv(here("data", "raw", "fixed_dates.csv")) %>%
  rename(user_id = subject_id) %>%
  gather(t, ids_date, -user_id) %>%
  filter(ids_date != 0) %>%
  mutate(ids_date = dmy(ids_date),
         t = as.numeric(t))

survey <- survey %>%
  mutate(ids_date = ymd(ids_date)) %>%
  full_join(fixed_dates, by = c("user_id", "t")) %>%
  mutate(ids_date = coalesce(ids_date.x, ids_date.y))

table(is.na(survey$ids_date))

###############################################################################
####                                                                      #####
####                            Sleep measures                            #####
####                                                                      #####
###############################################################################

# Load raw FitBit data --------------------------------------------------------

p <- here("data", "raw", "radarV1_MDD", "csv_files")
sleep <- read_csv(paste0(p, "/dailyFeatures_fitbit_sleep.csv"))
te <- read_csv(paste0(p, "/timeIntervals.csv"))
sleep <- left_join(sleep, te,
                   by = c("time_interval" = "timeInterval_ID")) %>%
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

sleep %>%
  select(sleep_day, sleep_onset, sleep_offset, start_sleep, stop_sleep,
         next_day) %>% 
  print(n = 50)

# Derive times of (i) going to bed; (ii) going to sleep; (iii) waking up;
# (iv) getting up.

# NOTE: We're adding a fixed amount, 50 minutes, to the "time in bed" because
# without this, many people get up (i.e. out of bed) before they wake up.
sleep$in_bed <- sleep$time_formatted + minutes(45)
sleep$get_up <- sleep$in_bed + minutes(round(sleep$time_in_bed / 60))
sleep$get_up <- if_else(sleep$get_up < sleep$stop_sleep,
                        sleep$stop_sleep,
                        sleep$get_up)

# Load derived sleep measures from Dan ----------------------------------------

# NOTE: we're selecting a single observation per day, for now.

# derived <- read_csv(here("data", "raw", "sleep_measures", "2022-02-08",
#                          "output1.csv")) %>%
#   select(-`...1`) %>%
#   mutate(merge_date = ymd(time_interval_readable)) 

# Merge raw sleep data (from CSVs) with derived measures (from Dan) -----------

# Prepare raw FitBit data
# sleep <- sleep %>%
#   mutate(merge_date = as.Date(datetimeStart + hours(12)),
#          user_id = participant_name) %>%
#   group_by(user_id, merge_date) %>%
#   summarise(across(everything(), ~ first(na.omit(.x))))

# # Check counts in each dataset
# derived %>% distinct(user_id, merge_date) %>% nrow()
# sleep %>% distinct(user_id, merge_date) %>% nrow()
# --> close enough.

# Select which measures from 'derived' we want (i.e. don't copy over columns
# already present in 'sleep').
# keep <- c("user_id", "merge_date",
#           names(derived)[!names(derived) %in% names(sleep)])
# derived <- derived[keep]

# Merge
# sleep <- full_join(sleep, derived,
#                    by = c("user_id", "merge_date")) %>%
#   clean_names()
# print(names(sleep))

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
  lookup <- survey_data %>%
    select(ids_date, 
           t,
           user_id) %>%
    mutate(win_start = ids_date + weeks(w_start),
           win_end = ids_date + weeks(w_end)) %>%
    drop_na(win_start, win_end) %>%
    group_by(user_id) %>%
    group_split() %>%
    map_dfr(expand_individual) %>%
    as_tibble()
  with_sleep <- lookup %>%
    left_join(sleep_data, by = c("user_id", "merge_date"))
  return(with_sleep) 
}

# Extract sleep days for required windows
windows <- list(prev_2w = c(-2, 0),
                first_month = c(-12, -8),
                last_month = c(-4, 0))

opts <- map(windows, ~ add_sleep(survey, sleep, .x[1], .x[2]))

save(opts, survey, sleep, file = here("data", "clean", "opts.Rdata"))
