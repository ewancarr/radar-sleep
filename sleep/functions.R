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

construct_datagrid <- function(.model, r, x) {
  # Construct a data frame over which to compute the predictions.
  # Calculates the median of each variable.
  nd <- insight::get_data(.model)
  nd <- nd[, !(names(nd) %in% c(x, "pid")), drop = FALSE]
  nd <- setDT(nd)[, lapply(.SD, median, na.rm = TRUE), ][rep(1:.N, length(r))]
  nd[[x]] <- r
  nd <- as.data.frame(nd)
  for (v in c("male", "partner", "med_other", 
              "med_sleep", "med_depress", "atyp")) {
    if (v %in% names(nd)) {
      nd[[v]] <- as.logical(nd[[v]])
    }
  }
  return(nd)
}

make_names <- function(model_list) {
  pmap_chr(model_list, ~ str_glue("{..1}__{..2}__{ifelse(length(..3) > 1, 'adj', 'unadj')}"))
}

extract_adjusted_predictions <- function(.model,
                                         .label,
                                         cluster_var = "pid",
                                         r = seq(-2, 2, 0.1)) {
  # Get x, y, covariates from model label
  params <- str_match(.label,
                      "([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)__([0-9a-zA-Z_]+)")
  y <- params[, 2]
  x <- params[, 3]
  cov <- params[, 4]
  cat(".")
  # Construct data frame for predictions 
  nd <- construct_datagrid(.model, r, x)
  # EXTRACT: Expected Values of the Posterior Predictive Distribution ---------
  expval <- add_epred_draws(.model, newdata = nd, re_formula = NA) 
  names(expval)[which(names(expval) == x)] <- "xvar"
  expval$term <- x
  expval$cov <- cov
  # EXTRACT: Draws from the Posterior Predictive Distribution -----------------
  postpred <- add_predicted_draws(.model, newdata = nd, re_formula = NA)
  names(postpred)[which(names(postpred) == x)] <- "xvar"
  postpred$term <- x
  postpred$cov <- cov
  # Return
  return(list(expval = expval, postpred = postpred))
}
