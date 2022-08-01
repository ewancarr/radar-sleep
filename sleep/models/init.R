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
verbose <- FALSE
source(here("sleep", "functions.R"), echo = TRUE)
set_cmdstan_path("~/.cmdstan/cmdstan-2.29.2")

host <- Sys.info()[["nodename"]]
if (host == "air") {
  n_iter <- 4000
  n_thin <- 1
  options(mc.cores = 2,
          brms.backend = "cmdstanr",
          brms.chains = 4)
  n_thread <- threading(1)
} else if (host == "office") {
  n_iter <- 10000
  n_thin <- 1
  options(mc.cores = 20,
          brms.backend = "cmdstanr",
          brms.chains = 4)
  n_thread <- threading(5)
}

# Functions -------------------------------------------------------------------

make_names <- function(i) {
    is_adjusted <- ifelse(length(i$adj) == 1, "unadj", "adj")
    return(str_glue("{i$y}__{i$x}__{is_adjusted}"))
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

# Load prepared data ----------------------------------------------------------

load(here("data", "clean", "merged.Rdata"), verbose = TRUE)

# Select required variables ---------------------------------------------------

sleep_vars <- c(# Total sleep time 
                "tst_med",       # median
                "tst_var",       # variance
                "cm3_tst_med",   # change in median
                "cm3_tst_var",   # change in variance
                # Sleep onset (clock time)
                "cm3_son_med",   # change this month vs. 3m ago
                "son_scaled_med",# median (scaled)
                "son_rel_var",   # variance in relative sleep onset
                # Sleep offset (clock time)
                "cm3_soff_med",  # change in median, this month vs. 3m ago
                "soff_med",      # median
                "soff_rel_var",  # variance in relative sleep offset
                # Sleep efficiency
                "slpeff_med",    # median
                # Sleep onset latency
                "sol_med",       # median
                "sol_var",       # variance
                "cm3_sol_med",   # change in median
                "cm3_sol_var",   # change in variance
                # Sleep fragmentation index
                "sfi_med",      # median
                # Any days sleeping >= 10 hours
                "hysom_ever",
                # Sleep midpoint
                "smid_med",     # median this month
                "smid_var",     # variance around median this month
                "cm3_smid_med", # change in median
                "cm3_smid_var", # change in variance
                # Social jet lag
                "sjl")
id <- c("user_id", "pid", "t")
covariates <- c("age", "male", "atyp", 
                "meds_mdd", "meds_other", "meds_sleep",
                "edyrs", "partner")
outcomes <- c("rel", "rel_mod", "rel_5cat", 
              "ids_total", "lag_ids_total")

dat <- merged |> 
  ungroup() |> 
  select(all_of(c(id, outcomes, sleep_vars, covariates))) |>
  # NOTE: Dropping rows with missing sleep or covariate information here.
  drop_na(all_of(sleep_vars),
          all_of(covariates),
          ids_total)

# Select analytical samples ---------------------------------------------------

s1 <- dat |> drop_na(rel_mod) |> pluck("pid") |> unique()
s2 <- dat |> drop_na(ids_total, lag_ids_total) |> pluck("pid") |> unique()

# Scale continuous variables --------------------------------------------------
 
transformed <- dat |>
  select(all_of(c(sleep_vars, "age", "edyrs"))) |>
  ungroup() |>
  mutate(across(where(~ is.numeric(.x) & min(.x, na.rm = TRUE) > 0),
                ~ scale(log(.x)),
                .names = "zlog_{.col}"),
         across(where(is.numeric), ~ as.numeric(scale(.x)), .names = "z_{.col}")) |>
  select(-all_of(sleep_vars), -age, -edyrs,
         -starts_with("z_zlog"))
dat <- bind_cols(dat, transformed)

# Select transformations to use in models -------------------------------------

trans <- c(# Sleep duration (4 measures) ======================================
           # Total sleep time
           "z_tst_med",          # median (1)
           "zlog_tst_var",       # variance (2)
           # Change in total sleep time
           "z_cm3_tst_med",      # median (3)
           "z_cm3_tst_var",      # variance (4)
           # Sleep quality (4 measures) =======================================
           # Sleep efficiency
           "z_slpeff_med",       # median (1)
           # Sleep fragmentation index
           "z_sfi_med",          # median (2)
           # Sleep onset latency
           "z_sol_med",          # median (3)
           "zlog_sol_var",       # variance (4)
           # Sleep regularity (5 measures) ====================================
           # Sleep midpoint
           "z_smid_med",         # median this month (1)
           "zlog_smid_rel_var",  # variance around median this month (2)
           "z_cm3_smid_med",     # change in median (3)
           "z_cm3_smid_var",     # change in variance (4)
           # Social jet lag
           "z_sjl")              # (5)

zcov <- c("z_age", "male",
          "meds_mdd", "meds_other", "meds_sleep",
          "z_edyrs", "partner")

# Save ------------------------------------------------------------------------

save(dat, sleep_vars, trans, zcov, s1, s2,
     file = here("data", "clean", "for_modelling.Rdata"))
