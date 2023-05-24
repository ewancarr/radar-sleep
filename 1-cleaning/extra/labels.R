# Title:        Labels for RADAR sleep measures
# Author:       Ewan Carr
# Started:      2022-05-26

library(tidyverse)

relabel <- function(x) {
  transform <- case_when(
    str_detect(x, "_log_gmz$") ~ "Grand-mean centred, log-transformed, scaled",
    str_detect(x, "_log_pmz$") ~ "Person-mean centred, log-transformed, scaled",
    str_detect(x, "_gmz$") ~ "Grand-mean centred, scaled",
    str_detect(x, "_pmz$") ~ "Person-mean centred, scaled",
    str_detect(x, "z$") ~ "Scaled",
    TRUE ~ "")
  group <- case_when(str_detect(x, "tst") ~ "Duration",
                     str_detect(x, "slpeff") ~ "Quality",
                     str_detect(x, "son") ~ "Regularity",
                     str_detect(x, "soff") ~ "Regularity",
                     str_detect(x, "son_rel") ~ "Regularity",
                     str_detect(x, "soff_rel") ~ "Regularity",
                     str_detect(x, "sol") ~ "Quality",
                     str_detect(x, "sfi") ~ "Quality",
                     str_detect(x, "smid") ~ "Regularity",
                     str_detect(x, "sjl") ~ "Regularity")
  measure <- case_when(str_detect(x, "tst") ~ "Total sleep time",
                       str_detect(x, "slpeff") ~ "Sleep efficiency",
                       str_detect(x, "son") ~ "Sleep onset",
                       str_detect(x, "soff") ~ "Sleep offset",
                       str_detect(x, "son_rel") ~ "Relative sleep onset",
                       str_detect(x, "soff_rel") ~ "Relative sleep offset",
                       str_detect(x, "sol") ~ "Sleep onset latency",
                       str_detect(x, "sfi") ~ "Sleep fragmentation index",
                       str_detect(x, "smid") ~ "Sleep midpoint",
                       str_detect(x, "sjl") ~ "Social jet lag")
  units <- case_when(str_detect(x, "tst") ~ "hours",
                     str_detect(x, "slpeff") ~ "score (0-100)",
                     str_detect(x, "son") ~ "clock time",
                     str_detect(x, "soff") ~ "clock time",
                     str_detect(x, "son_rel") ~ "hours",
                     str_detect(x, "soff_rel") ~ "hours",
                     str_detect(x, "sol") ~ "hours",
                     str_detect(x, "sfi") ~ "score",
                     str_detect(x, "smid") ~ "clock time",
                     str_detect(x, "sjl") ~ "hours")
  x_label <- case_when(str_detect(x, "tst") ~ "Total hours of sleep",
                       str_detect(x, "slpeff") ~ "Sleep efficiecy",
                       str_detect(x, "son_scaled") ~ "Sleep onset, hours after midday",
                       str_detect(x, "son") ~ "Sleep onset, clock time",
                       str_detect(x, "soff") ~ "Sleep offset, clock time",
                       str_detect(x, "son_rel") ~ "Variance in sleep onset",
                       str_detect(x, "soff_rel") ~ "Variance in sleep offset",
                       str_detect(x, "sol") ~ "Sleep onset latency, hours",
                       str_detect(x, "sfi") ~ "Sleep fragmentation index",
                       str_detect(x, "smid") ~ "Sleep midpoint",
                       str_detect(x, "sjl") ~ "Social jet lag, hours")
  stat <- case_when(str_detect(x, "_med") ~ ", median",
                    str_detect(x, "_var") ~ ", variance",
                    TRUE ~ "")
  change <- if_else(str_detect(x, "cm3_"), "Δ ", "")
  return(c(x = x,
           ch = change,
           tr = transform,
           uni = units,
           mea = measure,
           gr = group,
           xl = x_label,
           st = stat))
}

labels <- map_dfr(trans, \(f) {
  bind_rows(relabel(paste0("we_", str_replace(f, "_log$", ""))),
            relabel(str_glue("we_{f}_pmz"))) |>
  mutate(label_trans = str_glue("{ch}{mea}{st}\n\n{tr}"),
         label_orig = str_glue("{ch}{mea}{st}"))
})