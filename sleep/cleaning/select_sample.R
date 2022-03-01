# Title:        Select sample for sleep paper
# Author:       Ewan Carr
# Started:      2022-02-25

library(tidyverse)
library(here)
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)

# Check available sample with information on outcomes/sleep -------------------

n_avail <- function(data, ...) {
  d <- drop_na(data, ...) 
  return(list(n_part = length(unique(d$user_id)),
              n_obs = nrow(d)))
}

avail <- list(`Total` = list(n_part = length(unique(merged$user_id)),
                             n_obs = nrow(merged)),
              `With relapse` = n_avail(merged, relb),
              `With deterioration` = n_avail(merged, det),
              `With IDS total score` = n_avail(merged, ids_total),
              `With total sleep time` = n_avail(merged, tst_med),
              `With change in total sleep time` = n_avail(merged, cm3_tst_med),
              `With relapse AND change in sleep time` = n_avail(merged, relb, cm3_tst_med),
              `With IDS score AND sleep time` = n_avail(merged, ids_total, tst_med),
              `With IDS score AND change in sleep time` = n_avail(merged, ids_total, cm3_tst_med)) %>%
  map_dfr(~ .x, .id = "label")

avail %>%
  rename(` ` = label,
         `No. participants` = n_part,
         `No. observations` = n_obs) %>%
  gt::gt()
