# Title:        Binary models for 'relapse'
# Author:       Ewan Carr
# Started:      2022-02-25

library(here)
source(here("sleep", "models", "init.R"))
source(here("sleep", "cleaning", "labels.R"))

dest <- function(label) {
  here("sleep", "models", "samples", paste0(label, ".Rdata"))
}

make_names <- function(model_list) {
  pmap_chr(model_list, ~ str_glue("{..1}__{..2}__{ifelse(length(..3) > 1, 'adj', 'unadj')}"))
}

get_prediction <- function(model,
                           term,
                           r = seq(-2, 2, 0.05), 
                           cluster = "pid",
                           draws = FALSE) {
  at_list <- list()
  at_list[[term]] <- r
  at_list[[cluster]] <- "new"
  at_list[["model"]] <- model
  nd <- do.call("typical", at_list)
  p <- predictions(model, newdata = nd, re_formula = NA)
  if (draws) {
    return(posteriordraws(p))
  } else {
  return(p)
  }
}

extract_draws <- function(.model, .label) {
  params <- str_match(.label, "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  trans <- params[, 3]
  cov <- params[, 4]
  # Get predictions at specified values
  atval <- get_prediction(model = .model,
                          term = trans,
                          r = seq(-2, 2, 0.2),
                          draws = TRUE)
  names(atval)[names(atval) == trans] <- "xvar"
  atval$term <- trans
  atval$adj <- cov
  # Get predictions for observed data
  atobs <- predictions(.model)
  names(atobs)[names(atobs) == trans] <- "xvar"
  atval$term <- trans
  atval$adj <- cov
  # Return
  return(list(atval = atval, atobs = atobs))
}

extract_ame <- function(.model, .label) {
  params <- str_match(.label, "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  trans <- params[, 3]
  cov <- params[, 4]
  marginaleffects(.model) |>
    summary() |>
    posteriordraws() |>
    filter(term == trans)
}

###############################################################################
####                                                                      #####
####                    Fit models for 'Modified relapse'                 #####
####                                                                      #####
###############################################################################

d_relapse <- filter(dat, pid %in% s1)

# Specify models --------------------------------------------------------------

construct_formula <- function(.y, .x, .adj, .k = 7) {
  return(as.formula(str_glue("{.y} ~ {.x} + I({.x}^2){cc(.adj)} + (1 | pid)")))
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
      control = list(adapt_delta = 0.999,
                     step_size = 0.01,
                     max_treedepth = 15),
      threads = n_thread,
      ...)
}

fit_relmod <- pmap(opt_relmod,
                   ~ do_lr(..1, ..2, ..3, 
                           d_relapse,
                           iter = n_iter))  
names(fit_relmod) <- make_names(opt_relmod)

# Extract posterior draws -----------------------------------------------------

draws_relmod <- imap(fit_relmod, ~ extract_draws(..1, ..2))
save(draws_relmod, file = dest("relmod_draws"))
rm(draws_relmod)

# Extract average marginal effects --------------------------------------------
ame_relmod <- imap(fit_relmod, ~ extract_ame(..1, ..2))
save(ame_relmod, file = dest("relmod_ame"))
rm(ame_relmod)


# Save ------------------------------------------------------------------------

save(fit_relmod, file = dest("relmod_fit"))
rm(fit_relmod)

# END.
