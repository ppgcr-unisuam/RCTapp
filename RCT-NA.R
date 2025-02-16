as.NA <- function(dataset) {
  dataset[] <- lapply(dataset, function(x) {
    if (is.factor(x) || is.character(x)) {
      # For categorical data, exclude invalid entries
      x <- as.character(x)  # Ensure the column is treated as character
      x[x %in% c("", "-", "--", "NA", "na", "NULL")] <- NA  # Replace specific invalid values with NA
      x <- factor(x)  # Convert back to factor if the original data was a factor
      x
    } else {
      # Leave other types (e.g., logical) as-is
      x
    }
  })
  return(dataset)
}
