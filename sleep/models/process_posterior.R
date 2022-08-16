# Title:        Process sleep models
# Author:       Ewan Carr
# Started:      2022-06-20

library(tidyverse)
library(here)
library(marginaleffects)
source(here("sleep", "models", "init.R"))
load(here("sleep", "models", "samples", "ids_pre.Rdata"))
source(here("sleep", "cleaning", "extra", "labels.R"))

# Average marginal effects ----------------------------------------------------

load(here("sleep", "models", "samples", "relmod_ame.Rdata"), verbose = TRUE)
load(here("sleep", "models", "samples", "ids_ame.Rdata"), verbose = TRUE)

ame_draws <- bind_rows(map_dfr(ame_relmod, as_tibble, .id = "model"),
                       map_dfr(ids_ame, ~ as_tibble(.x$ame), .id = "model")) |>
  separate(model,
           into = c("y", "term", "adj"),
           sep = "__") |>
  mutate(adj = factor(adj,
                      levels = c("unadj", "adj"),
                      labels = c("Unadjust.", "Adjusted"))) |>
  filter(term != "hysom_ever") |>
  left_join(labels, by = c("term" = "x"))

ame_nosleep <- filter(ame_draws, y %in% c("ids_total", "ids_nosleep"))
ame_draws <- filter(ame_draws, y %in% c("rel_mod", "ids_total"))

saveRDS(ame_draws,
        file = here("sleep", "models", "processed", "ame_draws.rds"),
        compress = TRUE)

# Adjusted predictions --------------------------------------------------------

load(here("sleep", "models", "samples", "relmod_draws.Rdata"))
load(here("sleep", "models", "samples", "ids_pre.Rdata"))

post_relmod <- draws_relmod |>
    map_dfr(~ select(.x$atval, term, adj, xvar, draw)) |>
    mutate(y = "relmod")

post_ids <- ids_pre |>
    map_dfr(~ select(.x, term, adj = cov, xvar, draw)) |>
    mutate(y = "ids")

apre_draws <- bind_rows(post_relmod, post_ids) |>
  filter(term != "hysom_ever") |>
  as_tibble()

saveRDS(apre_draws, 
        file = here("sleep", "models", "processed", "apre_draws.rds"),
        compress = TRUE)

# Sensitivity analyses --------------------------------------------------------

# 1. Differences by depression subtype

load(here("sleep", "models", "samples", "relmod_int.Rdata"), verbose = TRUE)

# with_loo <- map(int_relmod, ~ map(.x, add_criterion, "loo", moment_match = TRUE))
with_loo <- map(int_relmod, 
                ~ map(.x, add_criterion, "loo"))

comparison <- map_dfr(with_loo, ~ loo_compare(.x$wo, .x$wi) |> 
                      as.data.frame() |>
                      rownames_to_column(),
                      .id = "model") |>
  select(model, rowname, elpd_diff) |>
  pivot_wider(names_from = rowname, values_from = elpd_diff) |>
  select(model, no_int = `.x$wo`, with_int = `.x$wi`)

saveRDS(comparison, file = here("sleep", "models", "processed",
                                "by_atypical.rds"))
  
# 2. Remove sleep items from RDS

saveRDS(ame_nosleep,
        file = here("sleep", "models", "processed", "ame_nosleep.rds"),
        compress = TRUE)
