# Title:        Calculate PAF for selected sleep features
# Author:       Ewan Carr
# Started:      2022-10-28

# https://cran.r-project.org/web/packages/graphPAF/vignettes/graph_PAF_vignette.pdf

library(tidyverse)
library(here)
library(AF)
library(lme4)
source(here("models", "init.R"))

d <- filter(dat, pid %in% s1$pid) |> 
  as.data.frame()

dc <- d |>
  bind_cols(scale_variables(d)) |>
  drop_na(rel_mod, cm3_tst_var, zlog_tst_var, z_tst_med, z_age, male,
          atyp, z_audit, med_depress, med_other, med_sleep, 
          z_edyrs, partner, z_sunshine)

create_binary <- function(x) {
  bins <- cut(x,
              breaks = quantile(x, prob = seq(0, 1, 0.25)),
              labels = 1:4)
  return(ifelse(bins == 4, 1, 0))
}

dc <- dc |>
  mutate(across(c(male, atyp, med_depress, med_other, med_sleep, partner), 
                as.numeric),
         across(c(cm3_tst_var, zlog_tst_var, z_tst_med),
                create_binary, .names = "{.col}_bin"))

fit_glm <- function(x, d) {
  glm(as.formula(paste0("rel_mod ~ ", x, " + z_age + male + ",
                        "atyp + z_audit + med_depress + med_other + ",
                        "med_sleep + z_edyrs + partner + z_sunshine")),
    family = binomial(link = "logit"),
    data = d)
}

af <- map(c("cm3_tst_var_bin", "zlog_tst_var_bin", "z_tst_med_bin"),
    function(feature) {
    f <- fit_glm(feature, dc)
    return(AFglm(f, dc, feature, "pid"))
  })

map_dfr(af, function(af) {
      s <- summary(af) 
      return(c(feature = af$exposure,
               est = s$AF[1],
               lo = s$confidence.interval[1], 
               hi = s$confidence.interval[2]))
  }) |>
  mutate(across(c(est, lo, hi), ~ round(as.numeric(.x), 2))) |>
  gt::gt()

