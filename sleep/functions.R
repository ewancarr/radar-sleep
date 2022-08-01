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
