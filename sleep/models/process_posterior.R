# Title:        Process posterior samples for sleep models
# Author:       Ewan Carr
# Started:      2022-06-20

library(tidyverse)
library(here)
library(marginaleffects)
source(here("sleep", "models", "init.R"))
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

load(here("sleep", "models", "samples", "relmod_pre.Rdata"), verbose = TRUE)
load(here("sleep", "models", "samples", "ids_pre.Rdata"), verbose = TRUE)

# Process posterior predictions for 'relmod'

relmod_pre <- relmod_pre |>
  map_dfr("expval", .id = "model") |>
  separate(model, c("y", "term", "adj"), sep = "__") |>
  ungroup() |>
  filter(adj == "adj") |>
  select(y, term, xvar, .epred)
  
# Process posterior predictions for 'IDS-SR'

ids_pre <- ids_pre |>
  imap_dfr(function(.fit, .label) {
         params <- str_split(.label, "__", simplify = TRUE)
         y <- params[1]
         x <- params[2]
         adj <- params[3]
         # pick <- c("predicted", "conf.low", "conf.high")
         pred <- .fit[, c("draw", x)]
         names(pred) <- c("draw", "xvar")
         pred$term <- x
         pred$adj <- adj
         pred$y <- y
         return(pred)
  })
ids_pre <- ids_pre[ids_pre$adj == "adj", ]
rownames(ids_pre) <- NULL

save(relmod_pre, ids_pre,
     file = here("sleep", "models", "processed", "predictions.Rdata"))

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
