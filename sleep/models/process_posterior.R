# Title:        Process posteriors from sleep models
# Author:       Ewan Carr
# Started:      2022-05-16

library(tidyverse)
library(here)
library(brms)
library(tidybayes)
library(scales)
library(gtsummary)
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)
load(here("data", "clean", "analytical_samples.Rdata"), verbose = TRUE)

# Sleep measures --------------------------------------------------------------

trans <- c("c_tst_med", 
           "c_cm3_tst_med", 
           "c_slpeff_med",
           "c_sfi_med",
           "hysom_ever",
           "cm3_son_med",
           "cm3_soff_med",
           "log_son_rel_var",
           "log_soff_rel_var",
           "smid_med",
           "log_smid_var",
           "sjl")

###############################################################################
####                                                                      #####
####                       Binary models for RELAPSE                      #####
####                                                                      #####
###############################################################################

load(here("samples_relb.Rdata"), verbose = TRUE)

# For adjusted estimates only
post_relb <- map2_dfr(trans, fit_relb[13:24],
                      ~ .y |>
                        tidy_draws() |>
                        gather_variables() |>
                        filter(.variable %in% paste0("b_", trans)))

# Plot ------------------------------------------------------------------------

post_relb |>
  mutate(.value = exp(.value)) |>
  group_by(.variable) |>
  median_qi(.width = 0.89) |>
  arrange(-.value)

p_relb <- post_relb |>
  ggplot() +
    aes(y = .variable,
        x = exp(.value),
        fill = .variable) +
  stat_halfeye(.width = c(0.66, 0.89)) +
  geom_vline(xintercept = 1, color = "red") +
  scale_x_log10(n.breaks = 5) +
  theme(legend.position = "none")

ggsave(p_relb, filename = here("relb.png"), dev = "png", dpi = 300)

###############################################################################
####                                                                      #####
####                    Binary models for DETERIORATION                   #####
####                                                                      #####
###############################################################################

load(here("samples_det.Rdata"), verbose = TRUE)

# For adjusted estimates only
post_det <- map2_dfr(sleep_vars, fit_det[14:26],
                      ~ .y$raw %>%
                        tidy_draws() %>%
                        gather_variables() %>%
                        filter(str_detect(.variable, "b_z")))

# Plot ------------------------------------------------------------------------

post_det |>
  mutate(.value = exp(.value)) |>
  group_by(.variable) |>
  median_qi(.width = 0.89) |>
  arrange(-.value)

p_det <- post_det |>
  ggplot() +
    aes(y = .variable,
        x = exp(.value),
        fill = .variable) +
  stat_halfeye(.width = c(0.66, 0.89)) +
  geom_vline(xintercept = 1, color = "red") +
  scale_x_log10(n.breaks = 5) +
  theme(legend.position = "none")

ggsave(p_det, filename = here("det.png"), dev = "png", dpi = 300)

###############################################################################
####                                                                      #####
####                   Linear models for IDS total score                  #####
####                                                                      #####
###############################################################################

load(here("samples_ids.Rdata"), verbose = TRUE)

post_ids <- map2_dfr(sleep_vars, fit_ids[14:26],
                      ~ .y$raw %>%
                        tidy_draws() %>%
                        gather_variables() %>%
                        filter(.variable == paste0("b_z_", .x)))

post_ids |>
  group_by(.variable) |>
  median_qi(.width = 0.89) |>
  arrange(-.value)

p_ids <- post_ids |>
  ggplot() +
    aes(y = .variable,
        x = .value,
        fill = .variable) +
  stat_halfeye(.width = c(0.66, 0.89)) +
  geom_vline(xintercept = 0, color = "red") +
  theme(legend.position = "none")

ggsave(p_ids, filename = here("ids.png"), dev = "png", dpi = 300)

conditional_effects(fit_ids[[16]]$smooth, "zl_tst_var")
conditional_effects(fit_ids[[18]]$smooth)
conditional_effects(fit_ids[[23]]$raw)

hist(dat$z_tst_med)

# END.
