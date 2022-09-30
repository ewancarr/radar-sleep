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
library(data.table)
source(here("sleep", "functions.R"), echo = TRUE)
set_cmdstan_path("~/.cmdstan/cmdstan-2.30.1")

# Set parameters
verbose <- FALSE
run_sensitivity <- FALSE
n_iter <- 40000
options(mc.cores = 20,
        brms.backend = "cmdstanr",
        brms.chains = 4)
n_thread <- threading(5)

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

merged <- readRDS(here("data", "clean", "merged.rds"))

# Select required variables ---------------------------------------------------

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
                "sjl")
     
id <- c("user_id", "pid", "t")

covariates <- c("age", "male",
                "atyp", "audit",
                "med_depress", "med_other", "med_sleep",
                "edyrs", "partner", 
                "sunshine")
outcomes <- c("rel", "rel_mod", "rel_5cat", 
              "ids_total", "lag_ids_total",
              "ids_nosleep", "lag_ids_nosleep")

dat <- merged |> 
  ungroup() |> 
  select(all_of(c(id, outcomes, sleep_vars, covariates)))

# Select analytical samples ---------------------------------------------------

s1 <- drop_na(dat,
              rel_mod,
              all_of(c(covariates,
                       sleep_vars))) |>
  pluck("pid") |> unique()
print(length(s1))

s2 <- drop_na(dat,
              ids_total,
              lag_ids_total,
              all_of(c(covariates,
                       sleep_vars))) |>
  pluck("pid") |> unique()
print(length(s2))

# Scale continuous variables --------------------------------------------------
 
transformed <- dat |>
  select(all_of(c(sleep_vars, "age", "edyrs", "sunshine", "audit"))) |>
  ungroup() |>
  mutate(across(where(~ is.numeric(.x) & min(.x, na.rm = TRUE) > 0),
                ~ scale(log(.x)),
                .names = "zlog_{.col}"),
         across(where(is.numeric),
                ~ as.numeric(scale(.x)),
                .names = "z_{.col}")) |>
  select(-all_of(sleep_vars),
         -age,
         -edyrs,
         -audit,
         -sunshine,
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
           "zlog_smid_var",      # variance this month (2)
           "z_cm3_smid_med",     # change in median (3)
           "z_cm3_smid_var",     # change in variance (4)
           # Social jet lag
           "z_sjl")              # (5)

zcov <- c("z_age", "male",
          "atyp", "z_audit",
          "med_depress", "med_other", "med_sleep",
          "z_edyrs", "partner", 
          "z_sunshine")

# Save ------------------------------------------------------------------------

save(dat, sleep_vars, trans, zcov, s1, s2,
     file = here("data", "clean", "for_modelling.Rdata"))
