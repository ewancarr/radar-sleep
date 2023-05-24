# Title:        Select sample for sleep paper
# Author:       Ewan Carr
# Started:      2022-02-25

library(tidyverse)
library(here)
library(naniar)
load(here("a-data", "clean", "for_modelling.Rdata"), verbose = TRUE)

print_n <- function(d, id = "user_id") {
  cat("\n\nParticipants: ", length(unique(d[[id]])),
      "\nObservations: ", nrow(d), "\n\n")
  return(d)
}

check_n <- function(d, var) {
  d <- drop_na(d, {{var}})
  cat("\n\nParticipants: ", length(unique(d$user_id)),
      "\nObservations: ", nrow(d), "\n\n")
}
    
# Export list of user IDs
write_lines(unique(merged$user_id),
            here("a-data", "raw", "synology", "users.txt"))

###############################################################################
####                                                                      #####
####                       Select analytical sample                       #####
####                                                                      #####
###############################################################################

# Sample 1: Relapse -----------------------------------------------------------

# Must have at least one measure of relapse, across time points (i.e. 3 monthly surveys).
# Must have measure of sleep (and change in sleep) at each time point.
# Must have required covariates (just age and sex for now) at each time point.

d1 <- dat |>
  drop_na(rel_mod,
          all_of(trans),
          all_of(covariates)) |>
  print_n() |>
  mutate(samp = "d1")

# Sample 2: Deterioration and IDS severity ------------------------------------

# NOTE: we have identical number with 'det' and 'ids_total'. So, combining 
# these into a single sample.

# * Must have at least one measure:
#    - Deterioration (det)
#    - IDS severity (ids_total) and lagged IDS severity (lag_ids_total)
# At each time point.
# * Must sleep measures (and change in sleep) at each time point.
# * Must have required covariates at each time point.

d2 <- dat |>
  drop_na(det, 
          all_of(trans),
          all_of(covariates)) |>
  print_n() |>
  mutate(samp = "d2")

# Save ------------------------------------------------------------------------

save(d1, d2, file = here("data", "clean", "analytical_samples.Rdata"))
