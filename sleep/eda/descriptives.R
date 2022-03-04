# Title:        Descriptives for sleep paper
# Author:       Ewan Carr
# Started:      2022-02-07

renv::activate()
library(tidyverse)
library(here)
library(gt)
library(gtsummary)
library(ggExtra)
library(patchwork)
load(here("data", "clean", "merged.Rdata"), verbose = TRUE)

###############################################################################
####                                                                      #####
####                                Table 1                               #####
####                                                                      #####
###############################################################################

sel <- drop_na(merged, ids_total, tst_med)

tab_demo <- merged %>%
  ungroup() %>%
  select(age_at_enrolment, male, edyrs, ids_total, relb, det) %>%
  tbl_summary(label = list(age_at_enrolment ~ "Age at enrolment",
                           male ~ "Male gender",
                           edyrs ~ "Years of education",
                           ids_total ~ "IDS total score",
                           relb ~ "Relapse (0/1)",
                           det ~ "Deterioration (0/1)"),
              missing = "no",
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_n(statistic = "{N_nonmiss}; {p_miss}% missing",
        col_label = "**No. available**",
        last = TRUE)


###############################################################################
####                                                                      #####
####                                Figures                               #####
####                                                                      #####
###############################################################################

make_plot <- function(data, v, label) {
  plot_data <- data[, c(str_glue("{v}_med"), str_glue("{v}_var"))]
  plot_data <- drop_na(plot_data)
  names(plot_data) <- c("x", "y")
  p <- ggplot(plot_data) +
    aes(x = x, y = y) +
    geom_point(alpha = 0.2) +
    geom_smooth() + 
    labs(title = str_glue("{label}; n = {nrow(plot_data)}"),
         x = str_glue("Median (across 7 days)"),
         y = str_glue("Variance (across 7 days)"))
  return(ggMarginal(p, type = "histogram", fill = "white"))
}

p1 <- make_plot(merged, "slpeff", "Sleep efficiency")
p2 <- make_plot(merged, "tst", "Total sleep time")
p3 <- make_plot(merged, "sol", "Sleep onset latency")

labels <- tribble(~var,         ~label,                                ~group,              ~index,
                  "tst_med",    "Total sleep time (median)",           "A. Sleep duration", 1,
                  "tib_med",    "Time in bed (median)",                "A. Sleep duration", 2,
                  "slpeff_med", "Sleep efficiency (median)",           "B. Sleep quality",  3,
                  "sol_med",    "Sleep onset latency (median)",        "B. Sleep quality",  4,
                  "sfi_med",    "Sleep fragmentation index (median)",  "C. Fragmentation",  5,
                  "insom_prop", "Probable insomnia (proportion)",      "C. Fragmentation",  6,
                  "hysom_prop", "Probable hypersomnia (proportion)",   "C. Fragmentation",  7,
                  "son_var",    "Sleep onset (variance)",              "D. Regularity",     8,
                  "son_var",    "Sleep onset (median)",                "D. Regularity",     9,
                  "soff_var",   "Sleep offset (variance)",               "D. Regularity",   10,
                  "smid_var",   "Sleep midpoint variance",             "D. Regularity",     11,
                  "sjl",        "Social jet lag (hours)",              "D. Regularity",     12,
                  "sri",        "Sleep regularity index (proportion)", "D. Regularity",     13)


odp <- function(x) { sprintf("%.2f", x) }

calc_summary <- function(x) {
  x_med <- odp(median(x, na.rm = TRUE))
  x_min <- odp(min(x, na.rm = TRUE))
  x_max <- odp(max(x, na.rm = TRUE))
  x_p25 <- odp(quantile(x, 1/4, na.rm = TRUE))
  x_p75 <- odp(quantile(x, 3/4, na.rm = TRUE))
  return(str_glue("{x_med} ({x_p25}, {x_p75}) [{x_min} to {x_max}]"))
}

calc_n <- function(x) {
  return(str_glue("{n_complete(x)} / {length(x)} ({round(pct_miss(x))}% missing)"))
}

tab_sleep <- merged %>%
  select(user_id, t,
         all_of(labels$var)) %>%
  pivot_longer(-(c(user_id, t)),
               names_to = "var",
               values_to = "value") %>%
  group_by(var) %>%
  summarise(col1 = calc_summary(value),
            col2 = calc_n(value)) %>%
  left_join(labels, by = "var") %>%
  arrange(index) %>%
  select(group, label, col1, col2) %>%
  mutate(across(c(group, label), ~ case_when(is.na(lag(.x)) ~ .x,
                                             lag(.x) == .x ~ "", 
                                             TRUE ~ .x))) 


gt(tab_sleep)

merged %>%
  select(tst_med,

tab_sleep <- merged %>%
  ungroup() %>%
  select(tst_med, tib_med, slpeff_med, son_med, soff_med, sol_med,
         sfi_med, insom_prop, hysom_prop) %>%
  mutate(across(c(insom_prop, hysom_prop), ~ .x * 100)) %>%
  tbl_summary(label = list(tst_med ~ "Total sleep time (hours)",
                           tib_med ~ "Total time in bed (hours)",
                           slpeff_med ~ "Sleep efficiency (0-100)",
                           son_med ~ "Sleep onset",
                           soff_med ~ "Sleep offset",
                           sol_med ~ "Sleep onset latency",
                           sfi_med ~ "Sleep fragmentation index",
                           insom_prop ~ "Probable insomnia, % of days",
                           hysom_prop ~ "Probable hypersomnia, % of days"),
              missing = "no",
              statistic = list(all_continuous() ~ "{median} ({p25}, {p75}) [{min} to {max}]",
                               all_categorical() ~ "{n} ({p}%)")) %>%
  bold_labels() %>%
  add_n(statistic = "{N_nonmiss}; {p_miss}% missing",
        col_label = "**No. available**",
        last = TRUE) %>%
  modify_caption("**Sleep measures, based on 7+ days per month** (N = {N})")
tab_sleep


