# Title:        Calculate PAF for selected sleep features
# Author:       Ewan Carr
# Started:      2022-10-28

# https://cran.r-project.org/web/packages/graphPAF/vignettes/graph_PAF_vignette.pdf

library(tidyverse)
library(here)
library(AF)
library(lme4)
library(gt)
source(here("2-models", "init.R"))
source(here("1-cleaning", "extra", "labels.R"))

create_binary <- function(x) {
  bins <- cut(x,
              breaks = quantile(x, prob = seq(0, 1, 0.25)),
              labels = 1:4)
  return(ifelse(bins == 4, 1, 0))
}

d_relapse <- inner_join(dat, s1, join_by("pid", "t"))
d_relapse <- bind_cols(d_relapse, scale_variables(d_relapse))

d <- d_relapse |>
  mutate(across(c(male, atyp, med_depress, med_other, med_sleep, partner), 
                as.numeric),
         across(all_of(str_glue("we_{trans}_pmz")),
                create_binary, .names = "{.col}_bin")) |>
  as.data.frame()

get_paf <- function(f, .data) {
  f <- as.character(f)
  fit <- glm(as.formula(paste0("rel_mod ~ ",
                               f, 
                               " + agez + male + atyp + auditz + med_depress +",
                               "med_other + med_sleep + edyrsz + partner +",
                               "sunshinez" )),
            family = binomial(),
            data = .data)
  return(AFglm(object = fit,
               data = .data,
               exposure = f,
               clusterid = "pid"))
}

dp <- \(x) sprintf("%.1f", x)

map_dfr(trans, \(sleep_feature) {
  x <- str_glue("we_{sleep_feature}_pmz_bin")
  s <- summary(get_paf(x, d))
  return(c(sleep_feature = sleep_feature,
           x = x,
           est = s$AF[1],
           lo = s$confidence.interval[1], 
           hi = s$confidence.interval[2]))
  }) |>
  mutate(across(est:hi, \(x) as.numeric(x) * 100),
         cell = str_glue("{dp(est)} [{dp(lo)}, {dp(hi)}]"),
         x = str_glue("we_{sleep_feature}_pmz")) |>
  left_join(labels, join_by(x)) |>
  select(gr, label_orig, cell) |>
  gt()
