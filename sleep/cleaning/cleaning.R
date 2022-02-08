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
    rename(event = redcap_event_name,
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
        pid = as.numeric(as.factor(subject_id)),
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

redcap <- read_dta(here("data", "raw", "REDCAP data.dta"))

outcomes <- redcap %>%
  select(subject_id,
         redcap_event_name,
         contains("relapse"),
         contains("deterioration_2SDs")) %>%
  pivot_longer(!c(subject_id, redcap_event_name),
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

# Check: ensure that  each participant has a single, unique value of 'relapse'
# at each time point? 

outcomes %>%
  group_by(subject_id, t, y) %>%
  summarise(single_value = length(unique(value)) == 1) %>%
  filter(!single_value)

# Reshape back to WIDE

outcomes <- outcomes %>%
  filter(y %in% c("rel", "det")) %>%
  select(subject_id, t, y, value) %>%
  pivot_wider(id_cols = c("subject_id", "t"), names_from = "y",
              values_from = "value")

# Check overlap between survey and REDCAP data. -------------------------------

# Check participants match (n=632):
table(unique(survey$subject_id) %in% unique(redcap$subject_id))
# --> Yep.

# Check observations match:
a <- distinct(outcomes, subject_id, t) %>% mutate(from_a = "outcomes")
b <- distinct(survey, subject_id, t) %>% mutate(from_b = "survey")
full_join(a, b) %>% 
  filter(t > 0) %>%
  arrange(subject_id, t) %>%
  count(is.na(from_a) | is.na(from_b))
# --> All but 2, which is OK for now.
# TODO: query.

survey <- survey %>%
  full_join(outcomes, by = c("subject_id", "t"))

print(length(unique(survey$subject_id)))

# Derive alternative outcome measures -----------------------------------------

survey <- survey %>%
  group_by(subject_id, t) %>%
  mutate(rel_alt = case_when(rel == 1 & ids_total >= 25 ~ 2,
                             rel == 0 & ids_total >= 25 ~ 1,
                             rel == 0 ~ 3,
                             TRUE ~ NA_real_))

###############################################################################
####                                                                      #####
####                           Load FitBit data                           #####
####                                                                      #####
###############################################################################

## Check survey respondents missing IDS date ----------------------------------

missing_dates <- survey %>%
  group_by(subject_id, t) %>%
  summarise(n_missing = sum(is.na(ids_date))) %>%
  filter(n_missing > 0) %>%
  spread(t, n_missing) %>%
  mutate(across(`0`:`15`, replace_na, 0))

write_csv(missing_dates, "~/missing_dates.csv")
head(missing_dates)

## Import missing dates (from Faith via email) --------------------------------

fixed_dates <- read_csv(here("data", "raw", "fixed_dates.csv")) %>%
  gather(t, ids_date, -subject_id) %>%
  filter(ids_date != 0) %>%
  mutate(ids_date = dmy(ids_date),
         t = as.numeric(t))

survey <- survey %>%
  mutate(ids_date = ymd(ids_date)) %>%
  full_join(fixed_dates, by = c("subject_id", "t")) %>%
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
sleep <- left_join(sleep, te, by = c("time_interval" = "timeInterval_ID"))

# Check matches between survey/FitBit
length(unique(sleep$participant_name))

check_overlap(sleep, survey,
              "participant_name", "subject_id")

# 576 are in both FitBit sleep data *and* survey data.
# 2 are in FitBit data only.
# 47 are in survey data only.

# Load derived sleep measures from Dan ----------------------------------------

# NOTE: we're selecting a single observation per day, for now.

derived <- read_csv(here("data", "raw", "sleep_measures", "2022-01-27",
                         "output.csv")) %>%
  select(-`...1`) %>%
  mutate(merge_date = ymd(time_interval_readable)) %>%
  group_by(user_id, merge_date) %>%
  summarise(across(everything(),
                   ~ first(na.omit(.x))))

# Merge raw sleep data (from CSVs) with derived measures (from Dan) -----------

# Prepare raw FitBit data
sleep <- sleep %>%
  mutate(merge_date = as.Date(datetimeStart + hours(12)),
         user_id = participant_name) %>%
  group_by(user_id, merge_date) %>%
  summarise(across(everything(), ~ first(na.omit(.x))))

# Check counts in each dataset
derived %>% distinct(user_id, merge_date) %>% nrow()
sleep %>% distinct(user_id, merge_date) %>% nrow()
# --> close enough.

# Select which measures from 'derived' we want (i.e. don't copy over columns
# already present in 'sleep').

keep <- c("user_id", "merge_date",
          names(derived)[!names(derived) %in% names(sleep)])
derived <- derived[keep]

# Merge
sleep <- full_join(sleep, derived,
                   by = c("user_id", "merge_date"))
print(names(sleep))

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

survey <- survey %>%
  mutate(win_start = ids_date - days(13),
         win_end = ids_date,
         user_id = subject_id) 

lookup <- survey %>%
  drop_na(win_start, win_end) %>%
  group_split(user_id) %>%
  map_dfr(expand_individual)

merged <- lookup %>%
  left_join(sleep, by = c("user_id", "merge_date")) %>%
  full_join(survey, by = c("user_id", "t")) %>%
  as_tibble() 

save(merged, file = here("data", "clean", "merged.Rdata"))
