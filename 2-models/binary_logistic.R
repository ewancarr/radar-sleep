# Title:        Binary models for 'relapse'
# Author:       Ewan Carr
# Started:      2022-02-25

library(here)
source(here("models", "init.R"))
source(here("cleaning", "extra", "labels.R"))
source(here("functions.R"), echo = TRUE)

extract_ame <- function(.model, .label) {
  params <- str_match(.label, "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  trans <- params[, 3]
  cov <- params[, 4]
  marginaleffects(.model, variables = trans) |>
    summary() |>
    posteriordraws() |>
    filter(term == trans)
}

###############################################################################
####                                                                      #####
####                    Fit models for 'Modified relapse'                 #####
####                                                                      #####
###############################################################################

# Select sample ---------------------------------------------------------------

d_relapse <- right_join(dat, s1, by = c("pid", "t"))
d_relapse <- bind_cols(d_relapse, scale_variables(d_relapse))

# Specify models --------------------------------------------------------------

construct_formula <- function(.y, .x, .adj) {
  return(as.formula(str_glue("{.y} ~ {.x} + I({.x}^2) {cc(.adj)} + (1 | pid)")))
}

opt_relmod <- expand_grid(y = "rel_mod",
                          x = trans,
                          adj = list("", zcov))

# Fit models ------------------------------------------------------------------

do_lr <- function(.y, .x, .adj, .data, ...) {
  brm(construct_formula(.y, .x, .adj),
      data = .data,
      family = bernoulli(),
      prior = set_prior("normal(0, 1.5)", class = "b"),
      stan_model_args = list(stanc_options = list("O1")),
      control = list(adapt_delta = 0.95,
                     step_size = 0.05,
                     max_treedepth = 15),
      ...)
}

fit_relmod <- pmap(opt_relmod,
                   ~ do_lr(..1, ..2, ..3, 
                           d_relapse,
                           iter = n_iter,
                           thin = 10,
                           threads = n_thread))  
names(fit_relmod) <- make_names(opt_relmod)

# Extract average marginal effects --------------------------------------------

relmod_ame <- imap(fit_relmod, extract_ame)
save(relmod_ame, file = here("models", "samples", "relmod_ame.Rdata"))
rm(ame_relmod)

# Extract adjusted predictions ------------------------------------------------

relmod_pre <- imap(fit_relmod, extract_adjusted_predictions)
save(relmod_pre, file = here("models", "samples", "relmod_pre.Rdata"))
rm(relmod_pre)

# Save ------------------------------------------------------------------------

save(fit_relmod, file = here("models", "samples", "relmod_fit.Rdata"))
rm(fit_relmod)

###############################################################################
####                                                                      #####
####        Sensitivity analysis: differences by depression subtype       #####
####                                                                      #####
###############################################################################

formula_with_interaction <- function(.y, .x, .adj,
                                     interact = TRUE,
                                     modifier = "atyp") {
  if (interact) {
    return(as.formula(str_glue("{.y} ~ {.x} + I({.x}^2) + {modifier} + {.x}:{modifier} + I({.x}^2):{modifier}{cc(.adj)} + (1 | pid)")))
  } else {
    return(as.formula(str_glue("{.y} ~ {.x} + I({.x}^2) + {modifier}{cc(.adj)} + (1 | pid)")))
  }
}

test_interaction <- function(.y, .x, .adj, .data, ...) {
  # Specify formula with and without interaction term
  f_with <- formula_with_interaction(.y, .x, .adj, TRUE, "atyp")
  f_wo <- formula_with_interaction(.y, .x, .adj, FALSE, "atyp")
  # Fit models
  m_with <- brm(f_with, 
                data = .data,
                family = bernoulli(),
                prior = set_prior("normal(0, 1.5)", class = "b"),
                stan_model_args = list(stanc_options = list("O1")),
                control = list(adapt_delta = 0.999,
                               step_size = 0.01,
                               max_treedepth = 15),
                ...)
  m_wo <- brm(f_with, 
              data = .data,
              family = bernoulli(),
              prior = set_prior("normal(0, 1.5)", class = "b"),
              stan_model_args = list(stanc_options = list("O1")),
              control = list(adapt_delta = 0.999,
                             step_size = 0.01,
                             max_treedepth = 15),
              ...)
  return(list(wo = m_wo, wi = m_with))
}

if (run_sensitivity) {
  int_relmod <- pmap(opt_relmod, ~ test_interaction(..1, ..2, ..3,
                                                    d_relapse,
                                                    iter = n_iter,
                                                    threads = n_thread))
  names(int_relmod) <- make_names(opt_relmod)
  save(int_relmod, file = dest("relmod_int"))
}

# END.
