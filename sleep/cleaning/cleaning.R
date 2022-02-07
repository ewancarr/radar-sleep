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

# Check overlap between survey and REDCAP data. --------------------------------

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

full_join(data.frame(fitbit = unique(sleep$participant_name),
                     in_fitbit = TRUE),
          data.frame(survey = unique(survey$subject_id),
                     in_survey = TRUE),
          by = c("fitbit" = "survey")) %>%
  count(in_fitbit, in_survey)

# 576 are in both FitBit sleep data *and* survey data.
# 2 are in FitBit data only.
# 47 are in survey data only.

# Load derived sleep measures from Dan ----------------------------------------

derived <- read_csv(here("data", "raw", "sleep_measures", "2022-01-27",
                         "output.csv")) %>%
  select(-`...1`)

# Merge raw and derived sleep measures ----------------------------------------

CONTINUE HERE

# TODO: explain approach

sleep_sel <- sleep %>%
  select(subject_id = participant_name, 
         s_eff = sleep_efficiency,
         s_tst = total_sleep_time,
         t_start = datetimeStart,
         t_end = datetimeEnd) %>%
  mutate(merge_date = as.Date(t_end)) 

# NOTE: some participants have multiple "daily" assessments per day. For now,
# I'm just taking the first non-missing value. Need to think of better
# approach.

sleep_sel <- sleep_sel %>%
  group_by(subject_id, merge_date) %>%
  summarise(across(c(s_eff, s_tst, t_start, t_end), ~ first(na.omit(.x))))

expand_individual <- function(d, ...) {
    pmap_dfr(list(win_start = d$win_start,
                  win_end = d$win_end,
                  t = d$t,
                  subject_id = d$subject_id),
             function(win_start,
                      win_end,
                      ids_date,
                      t,
                      subject_id) {
               data.frame(subject_id = subject_id,
                          t = t,
                          merge_date = seq(win_start,
                                           win_end,
                                           by = "1 day"))
             })
}

survey <- survey %>%
  mutate(win_start = ids_date - days(13),
         win_end = ids_date) 

lookup <- survey %>%
  drop_na(win_start, win_end) %>%
  group_split(subject_id) %>%
  map_dfr(expand_individual)

merged <- lookup %>%
  left_join(sleep_sel, by = c("subject_id", "merge_date")) %>%
  full_join(survey, by = c("subject_id", "t")) %>%
  as_tibble() %>%
  select(subject_id, t, win_start, win_end, merge_date, ids_date,
         ids_total, s_eff, s_tst, rel, det, t_start, t_end) %>%
  group_by(subject_id, t) %>%
  mutate(n_obs = n()) %>%
  filter(t > 0)


derived %>%



# Check: 
