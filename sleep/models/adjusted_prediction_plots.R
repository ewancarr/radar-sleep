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

apre_draws <- readRDS(here("sleep", "models", "processed", "apre_draws.rds"))
apre_draws <- filter(apre_draws, adj == "adj")

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

plot_data <- apre_draws |>
  subset_draws(subset) |>
  left_join(labels, by = c("term" = "x")) |>
  mutate(y_label = factor(y, 
                          levels = c("relmod", "ids"),
                          labels = c(lab_rel, lab_ids)))

dummy <- tibble(y_label = unique(plot_data$y_label),
                xvar = 0) |>
  mutate(lo = case_when(y_label == lab_rel ~ 0,
                        y_label == lab_ids ~ 27),
         hi = case_when(y_label == lab_rel ~ 0.12,
                        y_label == lab_ids ~ 33)) |>
  pivot_longer(lo:hi, values_to = "draw")

make_the_plot <- function(d) {
  ggplot(d) +
  aes(x = xvar,
      y = draw) +
  stat_lineribbon() +
  geom_blank(data = dummy) +
  scale_fill_brewer(palette = "Reds") +
  scale_color_brewer(palette = "Reds") +
  facet_grid(cols = vars(label_orig),
               rows = vars(y_label), 
               scale = "free",
               labeller = labeller(label_orig = label_wrap_gen(15))) +
  theme_ggdist() +
  facet_title_horizontal() +
  axis_titles_bottom_left()
}

# 1: Sleep duration -----------------------------------------------------------

p1 <- plot_data |>
  filter(gr == "Duration") |>
  mutate(draw = if_else(y_label == lab_rel & draw > 0.15, NA_real_, draw)) |>
  make_the_plot() +
  labs(title = "Sleep duration",
       subtitle = "Adjusted predictions from logistic and linear regression models",
       y = "Adjusted\nprediction",
       x = "SD difference in sleep measure",
       fill = "Credible\nintervals")

ggsave(p1, 
       filename = here("sleep", "writing", "figures", 
                       str_glue("apre_duration.png")),
       dpi = 300, dev = "png", width = 12, height = 10)

# 2: Sleep quality ------------------------------------------------------------

p2 <- plot_data |>
  filter(gr == "Quality") |>
  mutate(draw = if_else(y_label == lab_rel & draw > 0.15, NA_real_, draw)) |>
  make_the_plot() +
  labs(title = "Sleep quality",
       subtitle = "Adjusted predictions from logistic and linear regression models",
       y = "Adjusted\nprediction",
       x = "SD difference in sleep measure",
       fill = "Credible\ninterval")

ggsave(p2, 
       filename = here("sleep", "writing", "figures", 
                       str_glue("apre_quality.png")),
       dpi = 300, dev = "png", width = 12, height = 9)

# 3: Sleep regularity ---------------------------------------------------------

p3 <- plot_data |>
  filter(gr == "Regularity") |>
  mutate(draw = if_else(y_label == lab_rel & draw > 0.15, NA_real_, draw)) |>
  make_the_plot() +
  labs(title = "Sleep regularity",
       subtitle = "Adjusted predictions from logistic and linear regression models",
       y = "Adjusted\nprediction",
       x = "SD difference in sleep measure",
       color = "Credible\ninterval")

ggsave(p3, 
       filename = here("sleep", "writing", "figures", 
                       str_glue("apre_regularity.png")),
       dpi = 300, dev = "png", width = 14, height = 8)
