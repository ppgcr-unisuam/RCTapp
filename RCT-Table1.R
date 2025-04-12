TABLE.1 <- function(dataset, variables, bw.factor, max.levels = 2, alpha = 0.05, n.digits = 3) {
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  total_rows <- sum(sapply(variables, function(var) {
    var_data <- dataset[[var]]
    if (is.factor(var_data) || is.numeric(var_data) && nlevels(as.factor(var_data)) <= max.levels) {
      nlevels(as.factor(var_data))  # não conta linha "Missing"
    } else {
      1
    }
  }))
  
  descript.res <- matrix("", nrow = total_rows, ncol = nlevels(bw.factor) + 5)
  colnames(descript.res) <- c("Variable", "Levels", levels(bw.factor), "Missing", "P-value", "Test")
  row_idx <- 1
  
  for (var in variables) {
    data_col <- dataset[[var]]
    total_missing <- paste0(sum(is.na(data_col)), " (", round(sum(is.na(data_col)) / length(data_col) * 100, 1), "%)")
    
    if (is.factor(data_col) || is.numeric(data_col) && nlevels(as.factor(data_col)) <= max.levels) {
      # NÃO adiciona "Missing" como nível
      data_col_factor <- factor(data_col, exclude = NULL)
      levels_data <- levels(data_col_factor)
      freq_table <- table(data_col_factor, bw.factor)
      
      # Teste estatístico
      if (nlevels(bw.factor) == 2 && nrow(freq_table) == 2) {
        p.value <- suppressWarnings(fisher.test(freq_table)$p.value)
        test_type <- "Fisher"
      } else {
        p.value <- suppressWarnings(chisq.test(freq_table, simulate.p.value = TRUE)$p.value)
        test_type <- "Chi-squared"
      }
      if (p.value < 0.001) {
        p.value <- "<0.001"
      } else {
        p.value <- sprintf("%.3f", p.value)
      }
      
      # Preencher a tabela (sem linha "Missing")
      for (lvl in levels_data) {
        if (is.na(lvl) || lvl == "Missing") next  # pula linha de Missing
        
        freq_row <- if (lvl %in% rownames(freq_table)) {
          sapply(seq_len(nlevels(bw.factor)), function(idx) {
            paste0(freq_table[lvl, idx], " (", round((freq_table[lvl, idx] / sum(freq_table[, idx])) * 100, n.digits), "%)")
          })
        } else {
          rep("0 (0.0%)", nlevels(bw.factor))
        }
        
        if (lvl == head(levels_data[levels_data != "Missing"], 1)) {
          descript.res[row_idx, ] <- c(var, lvl, freq_row, total_missing, p.value, test_type)
        } else {
          descript.res[row_idx, ] <- c("", lvl, freq_row, "", "", "")
        }
        row_idx <- row_idx + 1
      }
      
    } else {
      # Variável numérica contínua
      suppressWarnings(data_col <- as.numeric(data_col))
      mean_sd <- tapply(data_col, bw.factor, function(x) {
        paste(round(mean(x, na.rm = TRUE), n.digits), " (", round(sd(x, na.rm = TRUE), n.digits), ")", sep = "")
      })
      
      df <- data.frame(val = data_col, grp = bw.factor)
      df <- df[!is.na(df$val) & !is.na(df$grp), ]
      
      if (nlevels(bw.factor) == 2) {
        t_test <- t.test(val ~ grp, data = df, var.equal = FALSE)
        p.value <- t_test$p.value
        test_type <- "Welch"
      } else {
        p.value <- anova(lm(val ~ grp, data = df))$`Pr(>F)`[1]
        test_type <- "ANOVA"
      }
      if (p.value < 0.001) {
        p.value <- "<0.001"
      } else {
        p.value <- sprintf("%.3f", p.value)
      }
      
      descript.res[row_idx, ] <- c(var, "", mean_sd, total_missing, p.value, test_type)
      row_idx <- row_idx + 1
    }
  }
  
  descript.res <- as.data.frame(descript.res[1:(row_idx - 1), ], check.names = FALSE, row.names = NULL)
  return(descript.res)
}
