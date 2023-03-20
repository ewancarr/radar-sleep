# Title:        Data cleaning for RADAR sleep analyses
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(lubridate)
library(haven)
library(janitor)
library(naniar)
library(fs)
library(data.table)
library(dtplyr)
source(here("sleep", "functions.R"))

###############################################################################
####                                                                      #####
####                              Survey data                             #####
####                                                                      #####
###############################################################################

# Load survey data ------------------------------------------------------------

survey <- read_dta(here("data", "raw", "totaldataset.dta")) |>
  rename(user_id = subject_id,
         event = redcap_event_name,
         ids_date = IDSdate) |>
  clean_names() |>
  mutate(event = which_event(event),
         t = as.numeric(event),
         pid = as.numeric(as.factor(user_id)),
         male = as.numeric(gender) == 0,
         partner = as.numeric(marital_status) %in% c(1, 2),
              # Cohabiting or married and living with
         edyrs = as.numeric(education_years),
         ids_total = as.numeric(ids_total),
         site = case_when(recruitmentsite == 1 ~ "KCL",
                          recruitmentsite == 2 ~ "CIBER",
                          recruitmentsite == 3 ~ "AMSTERDAM"),
         audit = if_else(t == 0,
                         baseline_audit_total,
                         followup_audit_total)) |>
         group_by(pid) |>
         fill(audit, .direction = "updown") |>
         ungroup()

# Identify depression subtypes ------------------------------------------------

ids_items <- survey |>
  select(user_id,
         t,
         starts_with("ids_")) |>
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
         melancholic_depression = mood_reactivity & (num_trues >= 2))

# Re-score IDS to exclude sleep items -----------------------------------------

ids_items <- ids_items |>
  mutate(ids_total_byhand = sum(c_across(c(ids_1:ids_9, ids_10:ids_30)), na.rm = TRUE),
         # ↑ Excluding 9a and 9b here.
         ids_nosleep = sum(c_across(c(ids_5:ids_9, ids_10:ids_30)), na.rm = TRUE))
         # ↑ Excluding Q1 to Q4 which ask about sleep

survey <- ids_items |>
  select(user_id, t,
         atypical_depression,
         melancholic_depression,
         ids_nosleep) |>
  left_join(survey, by = c("user_id", "t"))

# Get outcomes from REDCAP.dta ------------------------------------------------

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
    # This should be empty.

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
# TODO: query.

survey <- survey |>
  full_join(outcomes, by = c("user_id", "t"))

print(length(unique(survey$user_id)))

###############################################################################
####                                                                      #####
####          Load medication lookup; derive medication measures          #####
####                                                                      #####
###############################################################################

# Get medication data from REDCAP
meds <- redcap |>
  select(user_id, t, matches("csri_6_[1-6]_1")) |>
  pivot_longer(-c(user_id, t)) |>
  mutate(med_no = parse_number(str_match(name,
                                         "^csri_6_([1-6])_1")[,2]),
         value = if_else(value == "",
                         NA_character_,
                         str_squish(str_to_lower(value)))) |>
  drop_na(value) |>
  select(user_id, t, med_no, value)

# Load lookup table 
med_lookup <- read_csv(here("data", "raw",
                            "medications", "medkey_complete.csv")) |>
  distinct(original, correct) |>
  mutate(across(everything(), str_to_lower))

# Load Matthew's categories
cat_matthew <- read_csv(here("data", "raw", "medications",
                             "Medication Types_MH.csv"),
                        col_types = "c_c",
                        col_names = c("medication_name", "cat_matthew"),
                        skip = 1) |>
  mutate(across(everything(), str_to_lower))

# Load additional lookup table
other_lookup <- read_csv(here("data", "raw", "medications",
                             "other_medications.csv"),
                         col_types = "cc")

# Apply Ewan's fixes
meds <- meds |>
  left_join(other_lookup, by = "value") |>
  mutate(value = if_else(!(value %in% med_lookup$original),
                         str_to_lower(value_fixed),
                         value)) |>
  select(-value_fixed)
                         
# Check: are all medications in the lookup table?
not_found <- !(meds$value %in% med_lookup$original)
table(not_found)

# Check: which are missing?
unique(paste(na.omit(meds$value[not_found])))
  
meds <- meds |>
  left_join(med_lookup, by = c("value" = "original")) |>
  left_join(cat_matthew, by = c("correct" = "medication_name")) |>
  select(user_id, t, med_no, value, correct,
         medication_category = cat_matthew) |>
  distinct(user_id, t, med_no, value, .keep_all = TRUE)

# Create simplified measures --------------------------------------------------

meds <- meds |>
  group_by(user_id, t) |>
  summarise(
    med_depress = any(medication_category %in% c("antidepressant",
                                                 "antipsychotic",
                                                 "anticonvulsant",
                                                 "stimulating antidepressant"),
                      na.rm = TRUE),
    med_sleep = any(medication_category %in% c("benzodiazepine",
                                               "hypnotic"),
                    na.rm = TRUE),
    med_other = any((!is.na(medication_category)) & 
                     (!(med_depress | med_sleep)))) |>
  ungroup()


survey <- left_join(survey, meds, by = c("t", "user_id")) |>
  ungroup() |>
  mutate(across(c(med_depress, med_sleep, med_other), replace_na, FALSE))

## Import missing 'ids_date' dates (from Faith via email) ---------------------

fixed_dates <- read_csv(here("data", "raw", "fixed_dates.csv")) |>
  rename(user_id = subject_id) |>
  gather(t, ids_date, -user_id) |>
  filter(ids_date != 0) |>
  mutate(ids_date = dmy(ids_date),
         t = as.numeric(t))

survey <- survey |>
  full_join(fixed_dates, by = c("user_id", "t")) |>
  mutate(ids_date = coalesce(ids_date.x, ids_date.y))

table(is.na(survey$ids_date))

# Hours of sunshine -----------------------------------------------------------

# Source
# http://data.un.org/Data.aspx?d=CLINO&f=ElementCode:15&c=2,5,6,7,10,15,18,19,20,22,24,26,28,30,32,34,36,38,40,42,44,46&s=CountryName:asc,WmoStationNumber:asc,StatisticCode:asc&v=1

sunshine <- read_csv(here("data", "raw", "sunshine", "UNdata.csv")) |>
  clean_names() |>
  filter(station_name %in% c("MADRID", "London-Gatwick", "DE BILT")) |>
  select(station_name,
         tolower(month.abb)) |>
  mutate(site = case_when(station_name == "DE BILT" ~ "AMSTERDAM",
                          station_name == "London-Gatwick" ~ "KCL",
                          station_name == "MADRID" ~ "CIBER")) |>
  pivot_longer(jan:dec, names_to = "month", values_to = "sunshine") 

# Get hours of sunshine 4 weeks ago, for each survey

survey <- survey |>
  mutate(month = str_to_lower(lubridate::month(ids_date - weeks(4),
                                               label = TRUE,
                                               abbr = TRUE))) |>
  left_join(sunshine, by = c("site", "month"))

# Save ------------------------------------------------------------------------

saveRDS(survey, file = here("data", "clean", "survey.rds"))
