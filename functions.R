# Convert REDCAP event name into survey number
which_event <- function(x) {
  if_else(x == "enrolment_arm_1",
          "0",
          str_replace(x, "_month_assessmen[t]*_arm_1", ""))
}

# Winsorize extreme values
winsor <- function(x, at = c(-5, 5)) { 
  x[x < at[1]] <- at[1]
  x[x > at[2]] <- at[2]
  return(x)
}

print_n <- function(d) {
  cat("\nNo. participants: ", length(unique(d$user_id)),
      "\nNo. observations: ", nrow(d), "\n\n")
}


# Function to collapse a vector of covariates into formula
cc <- function(x) {
  if (x[1] == "") {
    return("") 
  } else {
    return(paste0(" + ", paste(x, collapse = " + ")))
  }
}

derive_midpoint <- function(start, stop) {
  half <- round(0.5 * (interval(start, stop) / minutes(1)))
  midpoint <- start + minutes(half)
  return(hour(midpoint) + (minute(midpoint) / 60))
}

calculate_median <- function(x) {
  if (is.logical(x)) {
    return(as.logical(median(x, na.rm = TRUE))) 
  } else if (is.numeric(x) ) {
    return(median(x, na.rm = TRUE))
  } else {
    stop("Must be logical or numeric")
  }
}

construct_datagrid <- function(fit, y, x, r) {
  # Construct a data frame over which to compute the predictions.
  # Calculates the median of each variable.
  nd <- insight::get_data(fit)
  nd <- select(nd, -pid, -all_of(c({{x}}, {{y}})))
  nd <- summarise(nd, across(everything(), calculate_median)) 
  nd <- uncount(nd, length(r))
  nd[[x]] <- r
  return(nd)
}

make_names <- function(model_list) {
  pmap_chr(model_list, ~ str_glue("{..1}__{..2}__{ifelse(length(..3) > 1, 'adj', 'unadj')}"))
}

extract_adjusted_predictions <- function(fit,
                                         y,
                                         x,
                                         adj,
                                         cent,
                                         days) {
  # Construct data frame for predictions 
  suffix <- if_else(cent == "gm", "gmz", "pmz")
  x <- str_glue("{days}_{x}_{suffix}")
  cat(".")
  nd <- construct_datagrid(fit, y, x, r = seq(-2, 2, 0.1))
  # Generate predictions, using brms::posterior_epred
  predictions(fit,
              newdata = nd,
              type = "response",
              re_formula = NA) |>
  posterior_draws() |>
  select(all_of(c(x, "draw")))
}


# Difference between start of sleep and [that person's] median start of
# sleep [within this time period]. Variance value of days within time
# period.

tdiff <- function(i) {
  start_time <- as.POSIXct(i[[1]] * 3600, origin = i[1])
  end_time <- as.POSIXct(i[[2]] * 3600, origin = i[2])
  return(as.numeric(difftime(start_time,
                             end_time,
                             units = "hours"))) 
}

clock_diff <- function(i) {
  # This isn't pretty, but...
  # ---------------------------------------------------------------------------
  # I needed away to calculate the difference between two 24 hour clocks. i.e.
  # 23 vs. 02 = +3
  # 05 vs. 21 = -8
  # The approach taken here is to find the smallest interval between 'a' and
  # and 'b' assuming that these clock times are:
  # i.    On the same day
  # ii.   'a' is day before 'b'
  # iii.  'b' is day before 'a'
  # The function then returns the smallest absolute interval
  opts <- list(c("2021-01-01", "2021-01-01"),   # i.
               c("2021-01-01", "2021-01-02"),   # ii.
               c("2021-01-02", "2021-01-01"))   # iii.
  res <- vector(length = 3)
  for (o in seq_along(opts)) {
    end_time <- as.POSIXct(i[1] * 3600, origin = opts[[o]][1])
    start_time <- as.POSIXct(i[2] * 3600, origin = opts[[o]][2])
    res[o] <- as.numeric(difftime(start_time, end_time, units = "hours"))
  }
  return(as.numeric(res[which(abs(res) == min(abs(res)))])[1])
}


ds <- function() format(Sys.time(), "%Y-%m-%d-%H-%M")
