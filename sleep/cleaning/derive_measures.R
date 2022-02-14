# Title:        Derive measures for sleep analysis
# Author:       Ewan Carr
# Started:      2022-02-07

library(tidyverse)
library(here)
library(lubridate)
library(naniar)
library(janitor)

load(here("data", "clean", "opts.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####           Function to derive sleep measures for each window          #####
####                                                                      #####
###############################################################################

summarise_sleep <- function(sleep_data, min_days) {
  # Select only observations with minimum required days of data
  selected <- sleep_data %>%
    group_by(user_id, t) %>%
    drop_na(total_sleep_time_hours) %>%
    count(name = "n_days") %>%
    filter(n_days >= min_days) %>%
    left_join(sleep_data, by = c("user_id", "t"))

  # Summarise measures
  selected %>%
    group_by(user_id, t) %>%
    summarise(across(c(total_sleep_time,
                       time_in_bed,
                       sleep_efficiency,
                       sleep_onset_latency_minutes,
                       sleep_fragmentation_index_hours,
                       insomnia,
                       hypersomnia,
                       sleep_onset,
                       sleep_offset,
                       sleep_fragmentation_index_hours),
                     ~ mean(.x, na.rm = TRUE),
                     .names = "{.col}_mean"),
              across(c(awakenings),
                     ~ median(.x, na.rm = TRUE),
                     .names = "{.col}_med"),
              across(c(sleep_onset_latency_minutes),
                     ~ var(.x, na.rm = TRUE),
                     .names = "{.col}_var")) %>%
    return()
}

pre <- summarise_sleep(opts[[2]], 14) %>% mutate(window = "pre")
post <- summarise_sleep(opts[[3]], 14) %>% mutate(window = "post")

sleep_change <- bind_rows(pre, post) %>%
  pivot_longer(-c(user_id, t, window),
               names_to = "measure",
               values_to = "value") %>%
  pivot_wider(names_from = "window",
              values_from = "value") %>%
  mutate(diff = post - pre) %>%
  drop_na(diff) %>%
  pivot_wider(id_cols = c(user_id, t),
              names_from = "measure",
              values_from = c("pre", "post", "diff"))

merged <- survey %>%
  left_join(sleep_change, by = c("user_id", "t"))

save(merged, sleep_change, file = here("data", "clean", "merged.Rdata"))
