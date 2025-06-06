test.model.fit <- function(dataset,
                           variables,
                           ov.name,
                           covariate,
                           bw.factor,
                           wt.labels,
                           wt.values,
                           missing = c("Multiple imputation", "Mean imputation", "Complete cases"),
                           m.imputations,
                           alpha,
                           p.digits = 3,
                           diagnostics) {
  
  # confirma a estrutura dos dados
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # remove variáveis não usadas
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  Little_Test_Res <- NULL
  Upset_Plot <- NULL
  if(any(is.na(dataset))) {
    if ("Missing Data" %in% diagnostics) {
      # Little's MCAR test
      missingtest <- misty::na.test(
        dataset,
        digits = 2,
        p.digits = p.digits
      )
      
      Little_Test_Res <- paste0(
        "Little's Missing Completely at Random (MCAR) Test: ",
        paste(
          paste(
            names(missingtest$result$little),
            format(missingtest$result$little, digits = 3),
            sep = " = "
          ),
          collapse = ", "
        ))
      
      Upset_Plot <- naniar::gg_miss_upset(
        dataset,
        nsets = 10,
        nintersects = NA,
        order.by = "freq",
        keep.order = TRUE,
        empty.intersections = "on"
      )
    }
  } else {
    Little_Test_Res <- "No missing data detected."
    Upset_Plot <- plot(1, type = "n", xlab = "", ylab = "")
  }
  
  # preparação e análise do modelo misto
  ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
  TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
  GROUP_M <- rep(bw.factor, length(wt.labels))
  OUTCOME_ORIG <- c(as.matrix(dataset))
  OUTCOME_M <- c(as.matrix(dataset))
  COVARIATE_M <- c()
  if (!sjmisc::is_empty(covariate)) {
    COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
  }
  
  # cria o dataset com os valores após imputação ou não de dados
  if (!sjmisc::is_empty(covariate)) {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
  } else {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
  }
  # change TIME_M to numeric
  data_M$TIME_M <- as.numeric(as.character(data_M$TIME_M))
  
  # Atenção: nos diagnósticos, não é aplicado pooling estatístico das imputações.
  # Os gráficos e outputs usam apenas as imputações diretamente (sem pooling),
  # portanto devem ser interpretados apenas como ferramentas exploratórias.
  Imp_Data <- NULL
  Convergence <- NULL
  if(missing == "Multiple imputation"){
    if("Imputed Data" %in% diagnostics | "Convergence" %in% diagnostics){
      # fit linear mixed model (same as Table 2a and Tableb 2b)
      ini <- mice::mice(data = data_M, maxit = 0)
      pred <- ini$pred
      pred["OUTCOME_M", "ID_M"] <- -2
      imp <-
        mice::mice(
          data_M,
          pred = pred,
          method = "pmm",
          m = m.imputations,
          seed = 0,
          print = FALSE,
          maxit = 50
        )
    }
    
    if ("Imputed Data" %in% diagnostics) {
      Imp_Data <- mice::stripplot(
        imp,
        OUTCOME_M ~ TIME_M | GROUP_M,
        pch = c(1, 20),
        layout = c(1, length(unique(GROUP_M))),
        main = "Imputed Data",
        xlab = "Time",
        ylab = ov.name
      )
    }
    if ("Convergence" %in% diagnostics) {
      Convergence <- plot(imp, layout = c(1,2))
    }
  }
  
  # decide como lidar com os dados perdidos
  # força a exclusão de linhas com dados faltantes para os métodos de imputação
  if (missing == "Complete cases" | missing == "Multiple imputation") {
    if (!sjmisc::is_empty(covariate)) {
      include <- complete.cases(dataset, covariate)
    } else {
      include <- complete.cases(dataset)
    }
    dataset <- dataset[include, ]
    covariate <- covariate[include, ]
    bw.factor <- bw.factor[include]
    ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
    TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
    GROUP_M <- rep(bw.factor, length(wt.labels))
    OUTCOME_ORIG <- c(as.matrix(dataset))
    OUTCOME_M <- c(as.matrix(dataset))
    COVARIATE_M <- NULL
    if (!sjmisc::is_empty(covariate)) {
      COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
    }
  }
  
  if (missing == "Mean imputation") {
    # Calculate the mean for imputation for each group
    for (i in 1:length(wt.labels)) {
      temp.imp <- dataset[, i]
      for (j in 1:nlevels(bw.factor)) {
        temp.imp[which(is.na(temp.imp) & bw.factor == levels(bw.factor)[j])] <-
          mean(temp.imp[which(bw.factor == levels(bw.factor)[j])], na.rm = TRUE)
      }
      dataset[, i] <- temp.imp
    }
    OUTCOME_M <- c(as.matrix(dataset))
    
    # mean imputation of covariate data if any
    COVARIATE_M <- NULL
    if (!sjmisc::is_empty(covariate)) {
      covariate <- covariate %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
    }
  }
  
  # cria o dataset com os valores após imputação ou não de dados
  if (!sjmisc::is_empty(covariate)) {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
  } else {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
  }
  
  # fit linear mixed model (same as Table 2a and Tableb 2b)
  if (!sjmisc::is_empty(covariate)) {
    my_formula <- as.formula(paste0("OUTCOME_M ~ TIME_M * GROUP_M + ",
                                    paste0(names(COVARIATE_M), collapse = " + ")))
    environment(my_formula) <- parent.frame()
    mod1 <- eval(substitute(
      nlme::lme(fixed = f, random = r, data = d),
      list(f = my_formula, r = ~ 1 | ID_M / TIME_M, d = data_M)
    ))
  } else {
    my_formula <- as.formula("OUTCOME_M ~ TIME_M * GROUP_M")
    environment(my_formula) <- parent.frame()
    mod1 <- eval(substitute(
      nlme::lme(fixed = f, random = r, data = d),
      list(f = my_formula, r = ~ 1 | ID_M / TIME_M, d = data_M)
    ))
  }
  
  x <- NULL
  Shapiro_Wilk_Res <- NULL
  if ("Scaled Residuals" %in% diagnostics) {
    residuals <- residuals(mod1, type = "normalized")
    Scaled_Res <- hist(
      x = residuals,
      freq = FALSE,
      main = "Scaled Residuals",
      xlab = "Residuals",
      ylab = "Density"
    )
    Shapiro_Wilk_out <- shapiro.test(residuals)
    # format p value with <0.001 if lower
    Shapiro_Wilk_out$p.value <- ifelse(Shapiro_Wilk_out$p.value < 0.001,
                                       "<0.001",
                                       round(Shapiro_Wilk_out$p.value, digits = p.digits)
    )
    Shapiro_Wilk_Res <- paste0(
      "Shapiro-Wilk test for scaled residuals: ",
      paste(
        paste(
          names(Shapiro_Wilk_out),
          format(Shapiro_Wilk_out, nsmall = 3),
          sep = " = "
        ),
        collapse = ", "
      ))
  }
  
  Comp_Plus_Res <- NULL
  if ("Component-Plus-Residual" %in% diagnostics) {
    Comp_Plus_Res <- effects::Effect(c("TIME_M", "GROUP_M"), mod1, residuals = TRUE)
  }
  
  VIF <- NULL
  if("Variance Inflation Factor" %in% diagnostics){
    VIF <- car::vif(mod1)
  }
  
  return(list(
    'Scaled_Res' = Scaled_Res, # residuals
    'Shapiro_Wilk_Res' = Shapiro_Wilk_Res, # residuals
    'Comp_Plus_Res' = Comp_Plus_Res, # residuals
    'VIF' = VIF, # multicollinarity
    'Little_Test_Res' = Little_Test_Res, # missing data
    'Upset_Plot' = Upset_Plot, # missing data
    'Imp_Data' = Imp_Data, # imputed data
    'Convergence' = Convergence # imputed data
  ))
}
