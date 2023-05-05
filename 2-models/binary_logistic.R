# Title:        Binary models for 'relapse'
# Author:       Ewan Carr
# Started:      2022-02-25

library(here)
library(data.table)
library(furrr)
plan(multisession, workers = 20)
source(here("2-models", "init.R"))
source(here("1-cleaning", "extra", "labels.R"))
source(here("functions.R"), echo = TRUE)

###############################################################################
####                                                                      #####
####                    Fit models for 'Modified relapse'                 #####
####                                                                      #####
###############################################################################

# Select sample ---------------------------------------------------------------

d_relapse <- right_join(dat, s1, by = c("pid", "t"))
d_relapse <- bind_cols(d_relapse, scale_variables(d_relapse))

# Specify models --------------------------------------------------------------

opt_relmod <- expand_grid(y = "rel_mod",
                          x = trans,
                          adj = list("", zcov),
                          cent = c("pm", "gm"),
                          days = c("wd", "we"))


construct_formula <- function(.y, .x, .adj, .cent, .days) {
  if (.cent == "gm") {
    # To estimate the total effect, use grand-mean centring
    xgm <- str_glue("{.days}_{.x}_gmz")
    return(as.formula(str_glue(
      "{.y} ~ {xgm} + I({xgm}^2) {cc(.adj)} + (1 | pid)"
      )))
  } else if (.cent == "pm") {
    # To estimate within- and between-person effects, use person-mean centring
    xpm <- str_glue("{.days}_{.x}_pmz")
    xm <- str_glue("{.days}_{.x}_mz")
    return(as.formula(str_glue(
      "{.y} ~ {xpm} + I({xpm}^2) + {xm} + I({xm}^2) {cc(.adj)} + (1 | pid)"
      )))
  }
}

# Fit models ------------------------------------------------------------------

do_lr <- function(y, x, adj, cent, days, data, ...) {
  brm(construct_formula(y, x, adj, cent, days),
      data = data,
      family = bernoulli(),
      prior = set_prior("normal(0, 1.5)", class = "b"),
      stan_model_args = list(stanc_options = list("O1")),
      control = list(adapt_delta = 0.95,
                     step_size = 0.05,
                     max_treedepth = 15),
      ...)
}

opt_relmod$fit <- 
  pmap(opt_relmod, \(y, x, adj, cent, days, ...) {
         do_lr(y, x, adj, cent, days, data = d_relapse,
               iter = params$n_iter,
               thin = params$n_thin,
               threads = params$n_thread)
      })

# Extract average marginal effect (AME) posteriors ----------------------------

get_draws <- function(f, x) {
  avg_slopes(f, variable = x) |>
    posterior_draws() |>
    pluck("draw") 
}

opt_relmod$ame <- pmap(opt_relmod, function(y, x, adj, cent, days, fit) {
   terms <- list(within = str_glue("{days}_{x}_pmz"),
                 between = str_glue("{days}_{x}_mz"),
                 total = str_glue("{days}_{x}_gmz"))
       if (cent == "pm") {
         return(list(within = get_draws(fit, terms$within),
                     between = get_draws(fit, terms$between)))
       } else if (cent == "gm") {
         return(list(total = get_draws(fit, terms$total)))
       }
})

# Extract adjusted predictions ------------------------------------------------

opt_relmod$pred <- 
  select(opt_relmod, fit, y, x, adj, cent, days) |>
  pmap(extract_adjusted_predictions)

# Save main models ------------------------------------------------------------

saveRDS(opt_relmod, 
        file = here("2-models", "samples", "final", 
                    str_glue("{ds()}_relmod_.rds")))
opt_relmod <- select(opt_relmod, -fit, -ame, -pred)

###############################################################################
####                                                                      #####
####        Sensitivity analysis: differences by depression subtype       #####
####                                                                      #####
###############################################################################

formula_with_interaction <- function(y, x, adj, days,
                                     interact = TRUE,
                                     modifier = "atyp") {
  # NOTE: We're using person-mean centered features only here (*_pmz)
  xpm <- str_glue("{days}_{x}_pmz")
  xm <- str_glue("{days}_{x}_mz")
  if (interact) {
    # Model WITH interaction term
    return(as.formula(str_squish(str_glue(
      "{y} ~ {xpm} + I({xpm}^2) + 
             {xm} + I({xm}^2) +
             {modifier} + {xpm}:{modifier} +
             I({xpm}^2):{modifier}
             {cc(adj)} + (1 | pid)"))))
  } else {
    # Model WITHOUT interaction term
    return(as.formula(str_squish(str_glue(
      "{y} ~ {xpm} + I({xpm}^2) + 
             {xm} + I({xm}^2)
             {cc(adj)} + (1 | pid)"))))
  }
}

test_interaction <- function(y, x, adj, days, data, ...) {
  # Specify formula with and without interaction term
  f_with <- formula_with_interaction(y, x, adj, days, interact = TRUE)
  f_wo <- formula_with_interaction(y, x, adj, days, interact = FALSE)
  # Fit models
  m_with <- brm(f_with, 
                data = data,
                family = bernoulli(),
                prior = set_prior("normal(0, 1.5)", class = "b"),
                stan_model_args = list(stanc_options = list("O1")),
                control = list(adapt_delta = 0.999,
                               step_size = 0.01,
                               max_treedepth = 15),
                ...) |>
    add_criterion("kfold")
  m_wo <- brm(f_with, 
              data = data,
              family = bernoulli(),
              prior = set_prior("normal(0, 1.5)", class = "b"),
              stan_model_args = list(stanc_options = list("O1")),
              control = list(adapt_delta = 0.999,
                             step_size = 0.01,
                             max_treedepth = 15),
              ...) |>
    add_criterion("kfold")
  return(list(wo = m_wo, wi = m_with))
}

if (params$run_sensitivity) {
  opt_relmod$int <- pmap(opt_relmod, 
                         \(y, x, adj, days, ...) {
                           test_interaction(y, x, adj, days,
                                            data = d_relapse,
                                            iter = 10000,
                                            thin = 5,
                                            threads = params$n_thread)
                         })
  saveRDS(opt_relmod,
          file = here("2-models", "samples", "final", 
                      str_glue("{ds()}_relmod_int.rds")))
}

# END.
