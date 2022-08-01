# Title:        Fit linear mixed models for IDS
# Author:       Ewan Carr
# Started:      2022-06-14

source(here::here("sleep", "models", "init.R"), echo = TRUE)
n_iter <- 4000

d_ids <- filter(dat, pid %in% s2)

# Specify models --------------------------------------------------------------

opt_ids <- expand_grid(y = "ids_total",
                       x = trans,
                       adj = list("", zcov))

# Fit models ------------------------------------------------------------------

do_lm <- function(.y, .x, .adj, .data, ...) {
  form <- as.formula(str_squish(str_glue(
            "{.y} ~ lag_{.y} + 
            I(lag_{.y}^2) +
            {.x} + I({.x}^2) +
            {.x}:lag_{.y} + {.x}:I(lag_{.y}^2) +
            I({.x}^2):lag_{.y} + I({.x}^2):I(lag_{.y}^2)
            {cc(.adj)} + (1 | pid)"
          )))
  brm(form,
      data = .data,
      family = gaussian(),
      prior = set_prior("normal(0, 2)", class = "b"),
      control = list(adapt_delta = 0.999,
                     step_size = 0.01,
                     max_treedepth = 15),
      threads = n_thread,
      iter = n_iter,
      ...)
}

fit_ids <- pmap(opt_ids, ~ do_lm(..1, ..2, ..3, d_ids))  
names(fit_ids) <- pmap_chr(opt_ids, ~ make_names(list(y = ..1, x = ..2, adj = ..3)))
save(fit_ids, file = here("sleep", "models", "samples", "ids_fit.Rdata"))

# Process posterior -----------------------------------------------------------

extract_ame <- function(.model, .label) {
  # Get x, y, covariates from model label
  params <- str_match(.label,
                      "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  x <- params[, 3]
  cov <- params[, 4]
  # Extract AVERAGE MARGINAL EFFECTS
  ame <- marginaleffects(.model) |> 
    summary() |> 
    posteriordraws() |>
    filter(term == x)
  # Get AVERAGE MARGINAL EFFECTS, stratified by previous IDS score
  ame_by <- marginaleffects(.model,
                            variables = x,
                            newdata = datagrid(lag_ids_total = seq(0, 74, 5),
                                               grid_type = "counterfactual")) |>
    summary(by = "lag_ids_total")
   ame_by <- as_tibble(ame_by)
   ame_by$cov <- cov
   ame_by$y <- y
   return(list(ame = ame, ame_by = ame_by))
}

extract_adjusted_predictions <- function(.model, .label,
                                         cluster_var = "pid",
                                         r = seq(-2, 2, 0.1)) {
  # Get x, y, covariates from model label
  params <- str_match(.label,
                      "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  x <- params[, 3]
  cov <- params[, 4]
  # Extract ADJUSTED PREDICTIONS at median value of lag_ids_total
  at_list <- list()
  at_list[[x]] <- r
  at_list[[cluster_var]] <- "new"
  at_list[["model"]] <- .model
  nd <- do.call("datagrid", at_list)
  pre <- predictions(.model,
                     newdata = nd,
                     re_formula = NA) |>
    posteriordraws()
  names(pre)[which(names(pre) == x)] <- "xvar"
  pre$term <- x
  pre$cov <- cov
  # Return
  return(pre)
}

# Average marginal effects ----------------------------------------------------

ids_ame <- imap(fit_ids, extract_ame) # NOTE: This takes about 20 minutes.
save(ids_ame, file = here("sleep", "models", "samples", "ids_ame.Rdata"))
rm(ids_ame)

# Adjusted predictions --------------------------------------------------------

ids_pre <- imap(fit_ids, extract_adjusted_predictions)
save(ids_pre, file = here("sleep", "models", "samples", "ids_pre.Rdata"))
rm(ids_pre)
