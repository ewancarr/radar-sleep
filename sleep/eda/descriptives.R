# Title:        Descriptives for sleep paper
# Author:       Ewan Carr
# Started:      2022-02-07

renv::activate()
library(tidyverse)
library(here)
library(gtsummary)
load(here("data", "clean", "clean.Rdata"), verbose = TRUE)

av2 %>%
  ungroup() %>%
  select(-user_id, -t) %>%
  tbl_summary()



av2 %>%
  ungroup() %>%
  select(-user_id, -t) %>%
  tbl_summary()

alt <- av2 %>%
  mutate(rel2 = case_when(rel == 1 ~ 1,
                          rel == 0 & ids_total > 25 ~ 0),
         rel3 = case_when(rel == 1 & ids_total > 25 ~ 1,
                          rel == 0 & ids_total <= 25 ~ 0)) %>%
  ungroup() 


tbl_summary(alt, include = c(rel2, rel3),
  type = list(rel2 ~ "categorical",
              rel3 ~ "categorical"))

table(av2$rel)
table(av2$rel_alt)

av2 %>%
  drop_na(rel) %>%
  ggplot(aes(x = ids_total, fill = factor(rel))) +
  geom_density(alpha = 0.5)
