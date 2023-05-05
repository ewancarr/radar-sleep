# Title:        Fit linear mixed models for IDS
# Author:       Ewan Carr
# Started:      2022-06-14

source(here::here("2-models", "init.R"), echo = TRUE)
source(here("functions.R"), echo = TRUE)

# Specify sample --------------------------------------------------------------

d_ids <- right_join(dat, s2, by = c("pid", "t"))
d_ids <- bind_cols(d_ids, scale_variables(d_ids))

# Specify models --------------------------------------------------------------

opt_ids <- expand_grid(y = c("ids_total", "ids_nosleep"),
                       x = trans,
                       adj = list("", zcov),
                       cent = c("pm", "gm"),
                       days = c("wd", "we"))

# Generate formula for each model ---------------------------------------------

make_formula <- function(y, x, adj, cent, days) {
  if(cent == "gm") {
    xgm <- str_glue("{days}_{x}_gmz")
    return(as.formula(str_squish(str_glue(
      "{y} ~ lag_{y} + I(lag_{y}^2) +
       {xgm} + I({xgm}^2) {cc(adj)} + (1 | pid)"))))
  } else if (cent == "pm") {
    xpm <- str_glue("{days}_{x}_pmz")
    xm <- str_glue("{days}_{x}_mz")
    return(as.formula(str_squish(str_glue(
      "{y} ~ lag_{y} + I(lag_{y}^2) +
       {xpm} + I({xpm}^2) +
       {xm} + I({xm}^2)
       {cc(adj)} + (1 | pid)"))))
  }
}

opt_ids$formula <- pmap(opt_ids, make_formula)

# Fit each model --------------------------------------------------------------

opt_ids$fit <- pmap(opt_ids, \(formula, ...) {
                      brm(formula,
                          data = d_ids,
                          family = gaussian(),
                          prior = set_prior("normal(0, 3)", class = "b"),
                          stan_model_args = list(stanc_options = list("O1")),
                          threads = params$n_thread,
                          thin = params$n_thin,
                          iter = params$n_iter)
      })

# Average marginal effects ----------------------------------------------------

get_draws <- function(f, x) {
  avg_slopes(f, variables = x) |>
    posterior_draws() |>
    pluck("draw")
}

opt_ids$ame <- pmap(opt_ids, function(y, x, adj, cent, days, fit, ...) {
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

# Adjusted predictions --------------------------------------------------------

opt_ids$pred <-
  select(opt_ids, fit, y, x, adj, cent, days) |>
  pmap(extract_adjusted_predictions)

# Save main models ------------------------------------------------------------

saveRDS(opt_ids,
        file = here("2-models", "samples",
                    str_glue("{ds()}_ids.rds")))
opt_ids <- select(opt_ids, -fit, -ame, -pred)

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
      "{y} ~ lag_{y} +
             I(lag_{y}^2) +
             {xpm} + I({xpm}^2) +
             {xm} + I({xm}^2) +
             {modifier} +
             {xpm}:{modifier} + I({xpm}^2):{modifier}
             {cc(adj)} + (1 | pid)"))))
  } else {
    # Model WITHOUT interaction term
    return(as.formula(str_squish(str_glue(
      "{y} ~ lag_{y} +
             I(lag_{y}^2) +
             {xpm} + I({xpm}^2) +
             {xm} + I({xm}^2) +
             {modifier}
             {cc(adj)} + (1 | pid)"))))
  }
}

test_interaction <- function(y, x, adj, days, data, ...) {
  # Specify formula with and without interaction term
  f_with <- formula_with_interaction(y, x, adj, days, interact = TRUE)
  f_wo <- formula_with_interaction(y, x, adj, days, interact = FALSE)
  # Fit models
  m_with  <- brm(f_with,
                 data = data,
                 family = gaussian(),
                 prior = set_prior("normal(0, 3)", class = "b"),
                 stan_model_args = list(stanc_options = list("O1")),
                 ...) |>
                   add_criterion("kfold")
  m_wo <- brm(f_wo,
              data = data,
              family = gaussian(),
              prior = set_prior("normal(0, 3)", class = "b"),
              stan_model_args = list(stanc_options = list("O1")),
              ...) |>
                add_criterion("kfold")
  return(list(wo = m_wo, wi = m_with))
}

if (params$run_sensitivity) {
  opt_ids$int <- pmap(opt_ids,
                      \(y, x, adj, days, ...) {
                        print(c(y, x, length(adj), days))
                        test_interaction(y, x, adj, days,
                                         data = d_ids,
                                         iter = 10000,
                                         thin = 5,
                                         threads = params$n_thread)
                      })
  saveRDS(opt_ids,
          file = here("2-models", "samples", "final",
                      str_glue("{ds()}_ids_int.rds")))
}

# END.
