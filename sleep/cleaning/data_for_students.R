# Title:        Extract data for MSc students
# Author:       Ewan Carr
# Started:      2022-05-26

library(tidyverse)
library(here)

load(here("data", "clean", "merged.Rdata"), verbose = TRUE)

sleep <- merged |>
  select(user_id, t,
         slpeff_med, cm3_slpeff_med,
         tst_med, tst_var, cm3_tst_med,
         sol_med, sol_var,
         sjl,
         smid_var)

sleep <- sleep[!(rowSums(is.na(sleep)) == (ncol(sleep) - 2)), ]

write_csv(sleep,
          file = here("data", "clean", "student_project.csv"))

