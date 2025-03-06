TABLE.1 <- function(dataset, variables, bw.factor, max.levels = 2, alpha = 0.05, n.digits = 3) {
  # Validate inputs
  if (is.null(dataset) || ncol(dataset) == 0) stop("Dataset is empty or invalid.")
  if (!all(variables %in% colnames(dataset))) stop("Some variables are not in the dataset.")
  if (length(unique(bw.factor)) < 2) stop("Between-group factor must have at least two levels.")
  
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # Calculate total rows for descript.res
  total_rows <- sum(sapply(variables, function(var) {
    if (is.factor(dataset[[var]]) || is.numeric(dataset[[var]]) && nlevels(as.factor(dataset[[var]])) <= max.levels) {
      nlevels(as.factor(dataset[[var]]))  # Add rows for each level of a categorical variable
    } else {
      1  # Add one row for a continuous variable
    }
  }))
  
  # Initialize the result matrix
  descript.res <- matrix("", nrow = total_rows, ncol = nlevels(bw.factor) + 4)  # +1 for Missing column
  colnames(descript.res) <- c("Variable", "Levels", levels(bw.factor), "Missing", "P-value")
  
  row_idx <- 1  # Row index for populating the matrix
  
  for (var in variables) {
    data_col <- dataset[[var]]
    
    # Calculate missing values
    missing_counts <- tapply(is.na(data_col), bw.factor, sum)
    total_missing <- paste(sum(is.na(data_col)), " (", round(sum(is.na(data_col)) / length(data_col) * 100, 1), "%)", sep = "")
    
    # Check for categorical variables
    if (is.factor(data_col) || is.numeric(data_col) && nlevels(as.factor(data_col)) <= max.levels) {
      data_col <- as.factor(data_col)
      # Handle missing levels
      # recode NA to Missing
      data_col[is.na(data_col)] <- "Missing"
      # Add "Missing" as a level before assigning it
      data_col <- factor(data_col, levels = c(levels(data_col)), exclude = NULL)
      levels_data <- levels(data_col)
      freq_table <- table(factor(data_col, levels = levels_data, exclude = NULL), bw.factor)
      p.value <- suppressWarnings(chisq.test(freq_table, simulate.p.value = TRUE)$p.value)
      # format p values < 0.001 as "<0.001"
      if (p.value < 0.001) {p.value <- "<0.001"}
      
      for (lvl in levels_data) {
        if (lvl %in% rownames(freq_table)) {
          freq_row <- sapply(seq_len(nlevels(bw.factor)), function(idx) {
            paste0(freq_table[lvl, idx], " (", round((freq_table[lvl, idx] / sum(freq_table[, idx])) * 100, n.digits), "%)")
          })
        } else {
          freq_row <- rep("0 (0.0%)", nlevels(bw.factor))
        }
        
        # Populate rows
        if (lvl == head(levels_data, 1)) {
          descript.res[row_idx, ] <- c(var, lvl, freq_row, total_missing, formatC(p.value, format = "f", digits = 3))
        } else {
          descript.res[row_idx, ] <- c("", lvl, freq_row, "", "")
        }
        row_idx <- row_idx + 1
      }
    }
    else if (length(unique(data_col, na.rm = TRUE)) > max.levels) {
      # Handle descriptive statistics
      suppressWarnings(data_col <- as.numeric(data_col))
      mean_sd <- tapply(data_col, bw.factor, function(x) {
        paste(round(mean(x, na.rm = TRUE), n.digits), " (", round(sd(x, na.rm = TRUE), n.digits), ")", sep = "")
      })
      p.value <- anova(lm(data_col ~ bw.factor))$`Pr(>F)`[1]
      # format p values < 0.001 as "<0.001"
      if (p.value < 0.001) {p.value <- "<0.001"}
      descript.res[row_idx, ] <- c(var, "", mean_sd, total_missing, formatC(p.value, format = "f", digits = 3))
      row_idx <- row_idx + 1
    }
  }
  
  # Convert to data frame for output
  descript.res <- as.data.frame(descript.res, check.names = FALSE, row.names = NULL)
  
  return(descript.res)
}
