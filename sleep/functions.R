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
