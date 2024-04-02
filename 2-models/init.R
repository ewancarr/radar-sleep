# Title:        Prepare data for modelling
# Author:       Ewan Carr
# Started:      2022-02-25

library(tidyverse)
library(here)
library(brms)
library(cmdstanr)
library(tidybayes)
library(scales)
library(gtsummary)
library(marginaleffects)
library(janitor)
library(splines)
library(conflicted)
conflicts_prefer(dplyr::mutate,
                 dplyr::filter)
source(here("functions.R"), echo = TRUE)
set_cmdstan_path("~/.cmdstan/cmdstan-2.34.1")

# Set parameters
params <- list(verbose = FALSE,
               all_options = FALSE,
               run_sensitivity = FALSE,
               n_iter = 40000,
               n_thread = threading(5),
               n_thin = 10)

options(mc.cores = 20,
        brms.backend = "cmdstanr",
        brms.chains = 4)

# Set minimum number of days per period ---------------------------------------

# This is the minimum number of days that someone needs to provide sleep data to
# be included in a given 3-month period.

min_days <- 8

# Load prepared data ----------------------------------------------------------

merged <- readRDS(here("a-data", "clean", "merged.rds"))

# Select required variables ---------------------------------------------------

add_prefix <- \(v) expand_grid(c("wd_", "we_"), v) |> pmap_chr(paste0)

sleep_vars <- c("tst_med",
                "tst_var",
                "cm3_tst_med",
                "cm3_tst_var",
                "slpeff_med",
                "sfi_med",
                "sol_med",
                "sol_var",
                "smid_var",
                "smid_med",
                "cm3_smid_med",
                "cm3_smid_var",
                "sjl") |>
  add_prefix()

id <- c("user_id",
        "pid",
        "t",
        "n_days",
        "n_weekdays",
        "ids_date")

covariates <- c("age", "male",
                "atyp", "audit",
                "med_depress", "med_other", "med_sleep",
                "edyrs", "partner", 
                "sunshine")

outcomes <- c("rel_mod",
              "ids_total", "lag_ids_total",
              "ids_nosleep", "lag_ids_nosleep")

dat <- merged |> 
  ungroup() |> 
  select(all_of(c(id, outcomes, sleep_vars, covariates))) |>
  filter(t <= 24,
         n_days >= min_days)

# Select analytical samples ----------------------------------------------------

# 1. Depression relapse

s1 <- dat |>
  select(pid, t,
         rel_mod,
         all_of(covariates),
         all_of(sleep_vars)) |>
  drop_na() |>
  select(pid, t)

print_n(s1)

# 2. Depression severity

s2 <- dat |>
  select(pid, t,
         ids_total, lag_ids_total,
         ids_nosleep, lag_ids_nosleep,
         all_of(covariates),
         all_of(sleep_vars)) |>
  drop_na() |>
  select(pid, t)

print_n(s2)

# Scale continuous variables --------------------------------------------------

variables_to_log <- c("tst_var", "sol_var", "smid_var") |> add_prefix()

scale_variables <- function(d) {
  continuous_covariates <- c("age", "edyrs", "sunshine", "audit")
  d |>
    select(all_of(c("pid", sleep_vars, continuous_covariates))) |>
    ungroup() |>
    mutate(# Log-transform some variables
           across(all_of(add_prefix(c("tst_var", "sol_var", "smid_var"))),
                  log, .names = "{.col}_log")) |>
    mutate(# Person-mean 
           across(where(is.numeric),
                  \(x) mean(x, na.rm = TRUE),
                  .names = "{.col}_m"),
           # Person-mean centred
           across(where(is.numeric),
                  \(x) x - mean(x, na.rm = TRUE),
                  .names = "{.col}_pm"),
           .by = "pid") |>
    mutate(# Grand-mean centred
           across(where(is.numeric),
                  \(x) x - mean(x, na.rm = TRUE),
                  .names = "{.col}_gm"),
           across(where(is.numeric),
                  \(x) scale(x, center = FALSE),
                  .names = "{.col}z")) |>
  select(-all_of(c("pid", sleep_vars, continuous_covariates)),
         -matches("m_gmz|m_pmz|pm_gmz|varz$|medz$|logz$|log$"),
         -ends_with("m")) |>
  mutate(across(everything(), as.numeric))
}

# Select transformations to use in models -------------------------------------

trans <- c(# Sleep duration (4 measures) ======================================
           "tst_med",          # Median total sleep time
           "tst_var_log",      # Variance total sleep time
           "cm3_tst_med",      # Δ median total sleep time
           "cm3_tst_var",      # Δ variance total sleep time
           # Sleep quality (4 measures) =======================================
           "slpeff_med",       # Median sleep efficiency
           "sfi_med",          # Median sleep fragmentation index
           "sol_med",          # Median sleep onset latency
           "sol_var_log",      # Variance sleep onset latency
           # Sleep regularity (5 measures) ====================================
           "smid_med",         # Median sleep midpoint
           "smid_var_log",     # Variance sleep midpoint
           "cm3_smid_med",     # Δ median sleep midpoint
           "cm3_smid_var",     # Δ variance sleep midpoint
           "sjl")          # Mean social jet lag

zcov <- c("agez", "male",
          "atyp", "auditz",
          "med_depress", "med_other", "med_sleep",
          "edyrsz", "partner", 
          "sunshinez")

# Save ------------------------------------------------------------------------

save(dat, sleep_vars, trans, zcov, s1, s2,
     file = here("a-data", "clean", "for_modelling.Rdata"))
