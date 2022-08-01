# Title:        Process sleep models
# Author:       Ewan Carr
# Started:      2022-06-20

library(tidyverse)
library(here)
source(here("sleep", "models", "init.R"))
source(here("sleep", "cleaning", "labels.R"))

library(marginaleffects)

# Average marginal effects ----------------------------------------------------

load(here("sleep", "models", "samples", "relmod_ame.Rdata"))
load(here("sleep", "models", "samples", "ids_ame.Rdata"))

ame_draws <- bind_rows(map_dfr(ame_relmod, as_tibble, .id = "model"),
                       map_dfr(ids_ame, ~ as_tibble(.x$ame), .id = "model")) |>
  mutate(y = factor(str_match(model, "^[a-z]+")[, 1], 
                    levels = c("rel", "ids"),
                    labels = c("Relapse", "IDS-SR"))) |>
  filter(term != "hysom_ever") |>
  left_join(labels, by = c("term" = "x"))

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
