# Title:        Plot adjusted predictions from each model
# Author:       Ewan Carr
# Started:      2022-07-13

library(tidyverse)
library(here)
library(patchwork)
library(ggdist)
subset <- FALSE
load(here("data", "clean", "for_modelling.Rdata"), verbose = TRUE)
source(here("sleep", "cleaning", "extra", "labels.R"))
load(here("sleep", "models", "processed", "predictions.Rdata"), verbose = TRUE)

lab_rel <- "Relapse\n\n(Predicted\nprobability)"
lab_ids <- "IDS severity\n\n(Predicted\nscore)"

subset_draws <- function(d, subset) {
  if (subset) {
    d |>
      group_by(term, adj, xvar, y) |>
      sample_n(10) 
  } else {
    d
  }
}

plot_data <- 
  bind_rows(select(relmod_pre, y, term, xvar, prediction = .epred),
            select(ids_pre, y, term, xvar, prediction = draw)) |>
  left_join(labels, by = c("term" = "x")) |>
  mutate(y_label = factor(y, 
                          levels = c("rel_mod", "ids_total"),
                          labels = c(lab_rel, lab_ids)))

# Choose what to plot ---------------------------------------------------------

# expval    Draws from expectation of the posterior predictive distribution
# postpred  Draw from posterior predictive distribution to the data

features <- c("z_tst_med", "zlog_tst_var", "z_smid_med", "z_sfi_med")
plot_data <- plot_data |>
  filter(term %in% features) |>
  mutate(term = factor(term, levels = features),
         label_orig = reorder(factor(label_orig), as.numeric(term)))

# Plot for relapse ------------------------------------------------------------

colors <- c("#ffccbc", "#e64a19")

p_relapse <- plot_data |>
  filter(y  == "rel_mod") |>
  ggplot() +
  aes(x = xvar,
      y = prediction) +
    stat_lineribbon(.width = c(0.5, 0.89)) +
    scale_fill_manual(values = colors, labels = c("89%", "50%")) +
    facet_grid(cols = vars(label_orig),
               scale = "free",
               labeller = labeller(label_orig = label_wrap_gen(15))) +
    theme_ggdist() +
    facet_title_horizontal() +
    axis_titles_bottom_left() +
    coord_cartesian(ylim = c(0, 0.25)) +
    labs(title = "Depression relapse",
         fill = "Credible\nintervals",
         x = "Standard deviations difference in sleep feature",
         y = str_wrap("Predicted probability of relapse", 10)) +
    theme(text = element_text(family = "Times New Roman"))

# Plot for IDS-SR -------------------------------------------------------------

p_ids <- plot_data |>
  filter(y  == "ids_total",
         term %in% features) |>
  ggplot() +
  aes(x = xvar, y = prediction) +
    stat_lineribbon(.width = c(0.5, 0.89)) +
    scale_fill_manual(values = colors, labels = c("89%", "50%")) +
    facet_grid(cols = vars(label_orig),
               scale = "free",
               labeller = labeller(label_orig = label_wrap_gen(15))) +
    theme_ggdist() +
    facet_title_horizontal() +
    axis_titles_bottom_left() +
    coord_cartesian(ylim = c(24, 30)) +
    scale_y_continuous(breaks = 24:30) +
    labs(title = "Depression severity (IDS-SR)",
         fill = "Credible\nintervals",
         x = "Standard deviations difference in sleep feature",
         y = str_wrap("Predicted value of IDS-SR", 10)) +
    theme(text = element_text(family = "Times New Roman"))

# Combine and save ------------------------------------------------------------

p_combined <- p_relapse / p_ids

ggsave(p_combined,
       filename = here("sleep", "writing", "figures", 
                       str_glue("predictions.png")),
       dpi = 300, dev = "png", width = 10, height = 8)

