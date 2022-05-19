# Title:        Fit models in brms
# Author:       Ewan Carr
# Started:      2022-02-25

library(tidyverse)
library(here)
library(brms)
library(cmdstanr)
library(tidybayes)
library(scales)
library(gtsummary)
n_iter <- 4000
verbose <- FALSE
set_cmdstan_path("~/.cmdstan/cmdstan-2.29.2")
options(mc.cores = 2,
        brms.backend = "cmdstanr",
        brms.chains = 4)

# Functions -------------------------------------------------------------------

print_n <- function(d) {
  cat("\nNo. participants: ", length(unique(d$user_id)),
      "\nNo. observations: ", nrow(d), "\n\n")
}

med_summary <- function(x) {
  tdp <- function(n) { sprintf("%.2f", n) }
  n_total <- length(x)
  x <- na.omit(x)
  n_complete <- length(x)
  med <- tdp(median(x))
  mea <- tdp(mean(x))
  iqr_lo <- tdp(quantile(x, 0.25))
  iqr_hi <- tdp(quantile(x, 0.75))
  return(str_glue("{mea} | {med} [{iqr_lo}, {iqr_hi}] ({n_complete}/{n_total})"))
}


# Load data, analytical sample ------------------------------------------------
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)
load(here("data", "clean", "analytical_samples.Rdata"), verbose = TRUE)

# Select required variables ---------------------------------------------------

sleep_vars <- c("tst_med",      # Total sleep time, median
                "cm3_tst_med",  # Total sleep time, change in median
                "tst_var",      # Total sleep time, variance
                "slpeff_med",   # Sleep efficiency, median
                # TODO          # Sleep onset latency, median
                "sfi_med",      # Sleep fragmentation index, median
                "hysom_ever",   # Any days sleeping >= 10 hours
                "cm3_son_med",  # Sleep onset, change this month vs. 3m ago
                "cm3_soff_med", # Sleep offset, change this month vs. 3m ago
                "son_rel_var",  # Variance of relative sleep onset
                "soff_rel_var", # Variance of relative sleep offset
                "smid_med",     # Sleep midpoint, median this month
                "smid_var",     # Sleep midpoint, variance around median this month
                "sjl")          # Social jet lag
id <- c("pid", "t")
covariates <- c("age", "male")
outcomes <- c("relb", "det", "ids_total", "lag_ids_total")

dat <- select(merged, all_of(c(id, outcomes, sleep_vars, covariates)))

# Table summarising analytical samples ----------------------------------------

if (FALSE) {
  map_dfr(list(s1 = s1,
               s2 = s2,
               all = union(s1, s2)), 
      ~ ungroup(dat) |>
        filter(user_id %in% .x), .id = "sample") |>
    select(-user_id, -pid, -t) |>
    tbl_summary(by = sample)
}

# Figure summarising distribution of continuous variables ---------------------

continuous_vars <- c("age", "tst_med", "cm3_tst_med", "slpeff_med", "sfi_med",
                     "hysom_ever", "cm3_son_med", "cm3_soff_med", "smid_med",
                     "sjl", "tst_var", "smid_var", "son_", "rel_off")

if (verbose) {
  dat |>
    pivot_longer(all_of(c("age", sleep_vars))) |>
    ggplot(aes(x = value)) +
      geom_histogram(bins = 100) +
      facet_wrap(~ name, scales = "free")
}

# Scale continuous variables --------------------------------------------------

dat <- dat |>
  ungroup() |>
  mutate(across(where(is.numeric), scale, .names = "cz_{.col}"),
         across(where(is.numeric), scale, scale = FALSE, .names = "c_{.col}"),
         across(where(is.numeric), ~ log(.x), .names = "log_{.col}"))

# Select transformations to use in models -------------------------------------

# For some, we want to center only, for other we standardise AND center.

trans <- c("c_tst_med", 
           "c_cm3_tst_med", 
           "log_tst_var",
           "c_slpeff_med",
           "cz_sfi_med",
           "hysom_ever",
           "cm3_son_med",
           "cm3_soff_med",
           "log_son_rel_var",
           "log_soff_rel_var",
           "smid_med",
           "log_smid_var",
           "sjl")

cov <- c("cz_age", "male")

###############################################################################
####                                                                      #####
####            Binary logistic regression models for RELAPSE             #####
####                                                                      #####
###############################################################################

d_s1 <- filter(dat, user_id %in% s1)
print_n(d_s1)

fit_lr <- function(.form, ...) {
  brm(formula = .form,
      family = bernoulli(),
      prior = set_prior("normal(0, 1.5)", class = "b"),
      ...) |>
  add_criterion("loo")
}

construct_formula <- function(.y, .x, .adj, ...) {
  cov <- ifelse(.adj[1] == "",
                "",
                paste0(" + ", paste(.adj, collapse = " + ")))
  cat("Fitting model: ", .y, " ~ ", .x, cov, "\n")
  return(fit_lr(str_glue("{.y} ~ {.x}{cov} + (1 | pid)"), ...))
}

# Specify models
models_relb <- cross(list(y = "relb",
                          x = trans,
                          adj = list("", cov)))


# Fit models
fit_relb <- map(models_relb, function(i) {
                  construct_formula(.y   = i$y,
                                    .x   = i$x,
                                    .adj = i$adj,
                                    data = d_s1,
                                    thin = 10,
                                    iter = 10000) })

save(fit_relb, file = here("samples_relb.Rdata"))

###############################################################################
####                                                                      #####
####                    Binary models for DETERIORATION                   #####
####                                                                      #####
###############################################################################

d_s2 <- filter(dat, user_id %in% s2)
print_n(d_s2)

# Specify models --------------------------------------------------------------
models_det <- cross(list(y = "det",
                          x = trans,
                          adj = list("", cov)))

# Fit models ------------------------------------------------------------------
fit_det <- map(models_det,
               function(i) {
                 construct_formula(.y = i$y,
                                   .x = i$x,
                                   .adj = i$adj,
                                   data = d_s2,
                                   iter = 10000,
                                   thin = 10) })

save(fit_det, file = here("samples_det.Rdata"))

###############################################################################
####                                                                      #####
####                      Linear models for IDS score                     #####
####                                                                      #####
###############################################################################

construct_lm <- function(.x, .adj, ...) {
  cov <- ifelse(.adj[1] == "",
                "",
                paste0(" + ", paste(.adj, collapse = " + ")))
  return(brm(str_glue("ids_total ~ lag_ids_total + {.x}{cov} + (1 | pid)"), ...))
}

# Specify models --------------------------------------------------------------

models_ids <- cross(list(x = trans, adj = list("", cov)))

# Fit models ------------------------------------------------------------------

fit_ids <- map(models_ids, function(i) {
                 construct_lm(.x = i$x,
                              .adj = i$adj,
                              data = d_s2,
                              iter = 10000,
                              thin = 10) })

save(fit_ids, file = here("samples_ids.Rdata"))

###############################################################################
####                                                                      #####
####                       Save analytical datasets                       #####
####                                                                      #####
###############################################################################

save(d_s1, d_s2, file = here("analytical_data.Rdata"))
