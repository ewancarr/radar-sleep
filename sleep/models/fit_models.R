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
n_iter <- 1e4
n_thin <- 10
verbose <- FALSE
set_cmdstan_path("~/.cmdstan/cmdstan-2.29.2")

host <- Sys.info()[["nodename"]]
if (host == "air") {
  options(mc.cores = 2,
          brms.backend = "cmdstanr",
          brms.chains = 4)
  n_thread <- threading(1)
} else if (host == "opti") {
  options(mc.cores = 20,
          brms.backend = "cmdstanr",
          brms.chains = 4)
  n_thread <- threading(5)
}

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

cc <- function(x) {
  # Function to collapse a vector of covariates into formula
  if (x[1] == "") {
    return("") 
  } else {
    return(paste0(" + ", paste(x, collapse = " + ")))
  }
}

make_names <- function(i) {
    is_adjusted <- ifelse(length(i$adj) == 1, "unadj", "adj")
    return(str_glue("{i$y}__{i$x}__{is_adjusted}"))
}

# Load data, analytical sample ------------------------------------------------
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)
load(here("data", "clean", "analytical_samples.Rdata"), verbose = TRUE)

# Select required variables ---------------------------------------------------

sleep_vars <- c("tst_med",      # Total sleep time, median
                "cm3_tst_med",  # Total sleep time, change in median
                "tst_var",      # Total sleep time, variance
                "slpeff_med",   # Sleep efficiency, median
                "sol_med",      # Sleep onset latency, median
                "sfi_med",      # Sleep fragmentation index, median
                "hysom_ever",   # Any days sleeping >= 10 hours
                "cm3_son_med",  # Sleep onset, change this month vs. 3m ago
                "cm3_soff_med", # Sleep offset, change this month vs. 3m ago
                "son_rel_var",  # Variance of relative sleep onset
                "soff_rel_var", # Variance of relative sleep offset
                "smid_med",     # Sleep midpoint, median this month
                "smid_var",     # Sleep midpoint, variance around median this month
                "sjl")          # Social jet lag
id <- c("user_id", "pid", "t")
covariates <- c("age", "male", "atyp")
outcomes <- c("relb", "det", "ids_total", "lag_ids_total")

dat <- merged |> ungroup() |> select(all_of(c(id, outcomes, sleep_vars, covariates)))

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
                     "sjl", "tst_var", "smid_var", "son_med", "rel_off")

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
           "log_sol_med",
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

construct_formula <- function(.y, .x, .adj) {
  return(as.formula(str_glue("{.y} ~ {.x}{cc(.adj)} + (1 | pid)")))
}

fit_brm <- function(.form, type = "lr", ...) {
  if (type == "lr") {
    fam <- bernoulli()
    p <- set_prior("normal(0, 1.5)", class = "b")
  } else if (type == "lin") {
    fam <- gaussian()
    p <- set_prior("normal(0, 2)", class = "b")
  }
  brm(formula = .form, family = fam, prior = p, 
      control = list(adapt_delta = 0.999,
                     stepsize = 0.01,
                     max_treedepth = 15),
      threads = n_thread,
      ...) |>
    add_criterion("loo")
}

# Specify models
models_relb <- cross(list(y = "relb",
                          x = trans,
                          adj = list("", cov)))

# Fit models
fit_relb <- map(models_relb, function(i) {
                  fit_brm(.form = construct_formula(i$y, i$x, i$adj),
                          type = "lr",
                          data = d_s1,
                          iter = n_iter,
                          thin = n_thin) })

names(fit_relb) <- map(models_relb, make_names)
save(fit_relb, file = here("sleep", "models", "samples", "samples_relb.Rdata"))

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
fit_det <- map(models_det, function(i) {
                 fit_brm(.form = construct_formula(i$y, i$x, i$adj),
                         type = "lr",
                         data = d_s2,
                         iter = n_iter,
                         thin = n_thin) })

names(fit_det) <- map(models_det, make_names)
save(fit_det, file = here("sleep", "models", "samples", "samples_det.Rdata"))

###############################################################################
####                                                                      #####
####                      Linear models for IDS score                     #####
####                                                                      #####
###############################################################################

# Specify models --------------------------------------------------------------
models_ids <- cross(list(y = "ids_total",
                         x = trans, 
                         adj = list("lag_ids_total", c(cov, "lag_ids_total"))))



# Fit models ------------------------------------------------------------------
fit_ids <- map(models_ids, function(i) {
                 fit_brm(.form = construct_formula(i$y, i$x, i$adj),
                         type = "lin",
                         data = d_s2,
                         iter = n_iter,
                         thin = n_thin) })

names(fit_ids) <- map(models_ids, make_names)
save(fit_ids, file = here("sleep", "models", "samples", "samples_ids.Rdata"))

###############################################################################
####                                                                      #####
####               Test interaction with depression subtype               #####
####                                                                      #####
###############################################################################

opts <- cross(list(x = trans,
                   y = c("relb", "det", "ids_total")))

fit_inter <- opts |>
  map(function(i) {
      adj <- c("cz_age", "male")
      ty <- "lr"
      if (i$y == "ids_total") { adj <- c(adj, "lag_ids_total"); ty <- "lin" }
      .data <- d_s2
      if (i$y == "relb") { .data <- d_s1 }
      m_wo <- fit_brm(as.formula(str_glue("{i$y} ~ {i$x}{cc(adj)} + atyp + (1 | pid)")),
                      type = ty, data = .data, iter = n_iter, thin = n_thin)
      m_wi <- fit_brm(as.formula(str_glue("{i$y} ~ {i$x}{cc(adj)} + atyp + atyp:{i$x} + (1 | pid)")),
                      type = ty, data = .data, iter = n_iter, thin = n_thin)
      return(list(wo = m_wo, wi = m_wi)) 
  })

names(fit_inter) <- map_chr(opts, ~ str_glue("{.x$y}__{.x$x}"))

save(fit_inter, file = here("sleep", "models", "samples", "samples_inter.Rdata"))

###############################################################################
####                                                                      #####
####                       Save analytical datasets                       #####
####                                                                      #####
###############################################################################

save(d_s1, d_s2, file = here("data", "clean", "analytical_data.Rdata"))
