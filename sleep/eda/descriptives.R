# Title:        Descriptives for sleep paper
# Author:       Ewan Carr
# Started:      2022-02-07

renv::activate()
library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(ggExtra)
library(patchwork)
library(naniar)
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)


###############################################################################
####                                                                      #####
####                                Table 1                               #####
####                                                                      #####
###############################################################################

sel <- drop_na(merged, ids_total, tst_med)

length(unique(sel$user_id))

tab_demo <- sel %>%
  ungroup() %>%
  select(age_at_enrolment, male, edyrs, ids_total, relb, det,
         medication_category, subtype) %>%
  tbl_summary(label = list(age_at_enrolment ~ "Age at enrolment",
                           male ~ "Male gender",
                           edyrs ~ "Years of education",
                           ids_total ~ "IDS total score",
                           relb ~ "Relapse (0/1)",
                           det ~ "Deterioration (0/1)",
                           medication_category = "Medications"),
              missing = "no",
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_n(statistic = "{N_nonmiss}; {p_miss}% missing",
        col_label = "**No. available**",
        last = TRUE)

tab_demo

# Number of participants/assessments ------------------------------------------

p <- sel %>%
  ungroup() %>%
  count(user_id, name = "no_obs") %>%
  count(no_obs, name = "no_part") %>%
  ggplot() +
  aes(x = no_obs,
      y = no_part) +
  geom_col() +
  theme_minimal(base_family = "Calibri") +
  scale_x_continuous(breaks = 1:8) +
  labs(x = "Number of assessments",
       y = "No. participants")
ggsave(p,
       filename = here("sleep", "writing", "figures", "n_assess.png"),
       dev = "png",
       width = 7,
       height = 5)


n_obs <- sel %>%
  filter(t > 0) %>%
  ungroup() %>%
  count(t)

sum(n_obs)
length(unique(sel$user_id))

n_obs %>%
  write_csv("~/tab.csv")



###############################################################################
####                                                                      #####
####                            Sleep measures                            #####
####                                                                      #####
###############################################################################

# Table summarising sleep measures --------------------------------------------

labels <- tribble(~var,           ~label,                                ~group,              ~index,
                  "tst_med",      "Total sleep time (median)",           "A. Sleep duration", 1,
                  "tib_med",      "Time in bed (median)",                "A. Sleep duration", 2,
                  "slpeff_med",   "Sleep efficiency (median)",           "B. Sleep quality",  3,
                  "sol_med",      "Sleep onset latency (median)",        "B. Sleep quality",  4,
                  "sol_var",      "Sleep onset latency (variance)",      "B. Sleep quality",  5,
                  "sfi_med",      "Sleep fragmentation index (median)",  "C. Fragmentation",  6,
                  "insom_prop",   "Probable insomnia (proportion)",      "C. Fragmentation",  7,
                  "hysom_prop",   "Probable hypersomnia (proportion)",   "C. Fragmentation",  8,
                  "son_med",      "Sleep onset (median)",                "D. Regularity",     9,
                  "abs_son_var",  "Sleep onset variance",                "D. Regularity",     10,
                  "soff_med",     "Sleep offset (median)",               "D. Regularity",     11,
                  "abs_soff_var", "Sleep offset variance",               "D. Regularity",     12,
                  "smid_med",     "Sleep midpoint (median)",             "D. Regularity",     13,
                  "smid_var",     "Sleep midpoint variance",             "D. Regularity",     14,
                  "sjl",          "Social jet lag (hours)",              "D. Regularity",     15,
                  "sri",          "Sleep regularity index (proportion)", "D. Regularity",     16)

odp <- function(x) { sprintf("%.2f", x) }

calc_summary <- function(x) {
  x_med <- odp(median(x, na.rm = TRUE))
  x_min <- odp(min(x, na.rm = TRUE))
  x_max <- odp(max(x, na.rm = TRUE))
  x_p25 <- odp(quantile(x, 1/4, na.rm = TRUE))
  x_p75 <- odp(quantile(x, 3/4, na.rm = TRUE))
  return(str_glue("{x_med} ({x_p25}, {x_p75}) [{x_min} to {x_max}]"))
}

calc_n <- function(x) {
  return(str_glue("{n_complete(x)} / {length(x)} ({round(pct_miss(x))}% missing)"))
}

tab_sleep <- merged %>%
  select(user_id, t,
         all_of(labels$var)) %>%
  pivot_longer(-(c(user_id, t)),
               names_to = "var",
               values_to = "value") %>%
  group_by(var) %>%
  summarise(col1 = calc_summary(value),
            col2 = calc_n(value)) %>%
  left_join(labels, by = "var") %>%
  arrange(index) %>%
  select(group, label, col1, col2) %>%
  mutate(across(c(group, label), ~ case_when(is.na(lag(.x)) ~ .x,
                                             lag(.x) == .x ~ "", 
                                             TRUE ~ .x))) 

gt(tab_sleep) %>%
  cols_label(group = "Category",
             label = "Measure",
             col1 = "Median (IQR) [range]",
             col2 = "N avail. / N total (% missing)") %>%
  cols_align(columns = c("col1", "col2"),
             align = "right")

# Repeat, but using change in sleep since 3 months ago ------------------------

labels_ch <- labels %>%
  mutate(var = paste0("cm3_", var))


tab_chsleep <- merged %>%
  select(user_id, t,
         all_of(labels_ch$var)) %>%
  pivot_longer(-(c(user_id, t)),
               names_to = "var",
               values_to = "value") %>%
  group_by(var) %>%
  summarise(col1 = calc_summary(value),
            col2 = calc_n(value)) %>%
  left_join(labels_ch, by = "var") %>%
  arrange(index) %>%
  select(group, label, col1, col2) %>%
  mutate(across(c(group, label), ~ case_when(is.na(lag(.x)) ~ .x,
                                             lag(.x) == .x ~ "", 
                                             TRUE ~ .x))) 

gt(tab_chsleep) %>%
  cols_label(group = "Category",
             label = "Measure",
             col1 = "Median (IQR) [range]",
             col2 = "N avail. / N total (% missing)") %>%
  cols_align(columns = c("col1", "col2"),
             align = "right")

###############################################################################
####                                                                      #####
####                                Figures                               #####
####                                                                      #####
###############################################################################

# Sleep measures --------------------------------------------------------------

p_sleep <- sel %>%
  select(user_id, t,
         all_of(labels$var)) %>%
  pivot_longer(-c(user_id, t),
               names_to = "measure",
               values_to = "value") %>%
  ggplot(aes(x = value,
             fill = measure)) +
  geom_density() +
  geom_histogram() +
  facet_wrap(~ measure,
             ncol = 4,
             scales = "free") +
  theme_minimal(base_family = "Calibri") +
  theme(legend.position = "none")
p_sleep
ggsave(p_sleep, 
       filename = "~/p_sleep.png",
       dev = "png",
       width = 7,
       height = 7,
       dpi = 300)

# Outcomes --------------------------------------------------------------------

sel %>%
  ungroup() %>%
  select(t, relb, det, ids_total, cm3_ids_total) %>%
  filter(t > 0) %>%
  group_by(t) %>%
  tbl_summary(by = "t")

