# Title:        Multinomial logistic regression
# Author:       Ewan Carr
# Started:      2022-02-25

source(here("sleep", "models", "init.R"), echo = TRUE)

d_s1 <- filter(dat, user_id %in% s1)

# Multinomial model for each sleep measure --------------------------------

# Including quadratic effects, with and without covariates

fit_multinomial <- function(y, x, cov, .data, ...) {
  f <- str_glue("{y} ~ {x} + I({x}^2) {cc(cov)} + (1 | ID | pid)")
  brm(as.formula(f),
      family = categorical(),
      data = .data,
      ...)
}

opt <- expand_grid(x = trans,
                   cov = map(list(unadj = "", adj = zcov), cc))
model_labels <- pmap_chr(opt, ~ str_glue("{..1}__{ifelse(nchar(..2) == 0, 'adj', 'unadj')}"))
fit_mn <- pmap(opt, ~ fit_multinomial(y = "rel_5cat",
                                      x = ..1,
                                      cov = ..2,
                                      .data = d_s1,
                                      iter = 2000))
names(fit_mn) <- model_labels

save(fit_mn, file = "fit_mn.Rdata")

# Plot ------------------------------------------------------------------------

names(fit_mn)

draw_the_plot <- function(
plot_data <- predictions(fit_mn[[1]],
                         newdata = datagrid(z_tst_med = seq(-2, 2, 0.2)),
                         re_formula = NA) 

plot_data |>
  ggplot(aes(x = z_tst_med,
             y = predicted,
             ymin = conf.low,
             ymax = conf.high)) + 
  geom_line() + 
  geom_ribbon(alpha = .1) + 
  facet_wrap(~ group)


