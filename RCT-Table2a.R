TABLE.2a <- function(dataset,
                     variables,
                     covariate,
                     bw.factor,
                     control.g,
                     wt.labels,
                     wt.values,
                     missing = c("Multiple imputation", "Mean imputation", "Complete cases"),
                     m.imputations,
                     alpha,
                     n.digits) {
  # This function outputs a comparison table for two-way mixed-models
  # dataset: a 2D dataframe (rows: participants, columns: variables)
  # variables: a 1D variable labels (within-group)
  # covariate: a 2D dataframe (rows: participants, columns: covariates)
  # bw.factor: a 1D between-group factor
  # wt.labels: a 1D variable labels for each level
  # wt.values: a 1D variable values for each level
  # missing: method for handling missing balues (complete.cases, mean inputation, multiple imputation)
  # m.imputations: number of imputations
  # alpha: the type-I error level
  # n.digits: number of decimal places to be presented for continuous variables
  
  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }
  
  # default values
  if (alpha == "") {
    alpha = 0.05
  }
  if (n.digits == "") {
    n.digits = 3
  }
  
  # confirma a estrutura dos dados
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # remove variáveis não usadas
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  # inicializa a matriz de resultados
  t.labels <- c("Within-subjects",
                "Between-subjects",
                "N",
                "Outcome",
                "P-value")
  mix.mod.res <- matrix(NA,
                        nrow = length(head(t.labels, -1)),
                        ncol = nlevels(bw.factor) * length(wt.labels))
  rownames(mix.mod.res) <- head(t.labels, -1)
  colnames(mix.mod.res) <- rep("", nlevels(bw.factor) * length(wt.labels))
  
  # matriz de resultados intra-grupo
  wt.diff <- matrix(NA,
                    nrow = length(t.labels),
                    ncol = nlevels(bw.factor) * (length(wt.labels) - 1))
  rownames(wt.diff) <- t.labels
  colnames(wt.diff) <- rep("", nlevels(bw.factor) * (length(wt.labels) - 1))
  
  # matriz de resultados entre-grupos
  N.comb <- dim(combn(nlevels(bw.factor), 2))[2]
  bw.diff <- matrix(NA, nrow = length(t.labels) + 1, ncol = N.comb * (length(wt.labels) - 1))
  rownames(bw.diff) <- c(t.labels, "SMD¹")
  colnames(bw.diff) <- rep("", N.comb * (length(wt.labels) - 1))
  
  # matriz de resultados do modelo
  model.res <-
    matrix(NA, nrow = length(head(t.labels, -1)), ncol = 1)
  rownames(model.res) <- head(t.labels, -1)
  colnames(model.res) <- c("Mixed-model effects")
  
  # matriz de resultados de interação
  interaction <- c()
  
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
  
  # decide como lidar com os dados perdidos
  if (missing == "Complete cases") {
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
  
  if (missing == "Multiple imputation") {
    dataset <- dataset
    bw.factor <- bw.factor
    ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
    TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
    GROUP_M <- rep(bw.factor, length(wt.labels))
    OUTCOME_ORIG <- c(as.matrix(dataset))
    OUTCOME_M <- c(as.matrix(dataset))
    # mean imputation of baseline data if any
    OUTCOME_M[which(is.na(OUTCOME_M) &
                      TIME_M == levels(TIME_M)[1])] <-
      mean(OUTCOME_M[which(!is.na(OUTCOME_M) &
                             TIME_M == levels(TIME_M)[1])])
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
  
  # fit linear mixed model
  if (missing != "Multiple imputation") {
    if (!sjmisc::is_empty(covariate)) {
      mod1 <-
        nlme::lme(
          fixed = as.formula(paste0(
            "OUTCOME_M ~ TIME_M * GROUP_M + ",
            paste0(names(COVARIATE_M), collapse = " + ")
          )),
          random = ~ 1 | ID_M / TIME_M,
          data = data_M
        )
    } else {
      mod1 <-
        nlme::lme(
          fixed = OUTCOME_M ~ TIME_M * GROUP_M,
          random = ~ 1 | ID_M / TIME_M,
          data = data_M
        )
    }
    mod1.aov <- anova(mod1)
  } else {
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
    if (!sjmisc::is_empty(covariate)) {
      mod1 <-
        with(data = imp,
             nlme::lme(
               fixed = as.formula(paste0(
                 "OUTCOME_M ~ TIME_M * GROUP_M + ",
                 paste0(names(COVARIATE_M), collapse = " + ")
               )),
               random = ~ 1 | ID_M / TIME_M
             ))
      mod1.aov <- quiet(miceadds::mi.anova(imp, formula = paste0(
        "OUTCOME_M ~ TIME_M * GROUP_M + ",
        paste0(names(COVARIATE_M), collapse = " + ")
      )))
    } else {
      mod1 <-
        with(data = imp,
             nlme::lme(
               fixed = OUTCOME_M ~ TIME_M * GROUP_M,
               random = ~ 1 | ID_M / TIME_M
             ))
      mod1.aov <- quiet(miceadds::mi.anova(imp, formula = "OUTCOME_M ~ TIME_M * GROUP_M"))
    }
    mod1.aov <- mod1.aov$anova.table[1:3, 2:5]
    mod1.aov <- rbind(rep(NA, 4), mod1.aov)
  }
  
  # calcula e preenche o N e o DESFECHO da tabela de resultados
  N <- c()
  desfecho <- c()
  media <- c()
  p.values <- c()
  for (i in 1:length(wt.labels)) {
    for (j in 1:nlevels(bw.factor)) {
      # dados válidos
      N <-
        rbind(N, (paste("(n = ", sum(
          !is.na(OUTCOME_ORIG[TIME_M == wt.values[i] &
                                GROUP_M == levels(bw.factor)[j]])
        ), ")", sep = "")))
      # média (DP)
      desfecho <-
        rbind(desfecho, paste(
          format(round(
            mean(OUTCOME_M[TIME_M == wt.values[i] &
                             GROUP_M == levels(bw.factor)[j]], na.rm = TRUE), digits = n.digits
          ), nsmall = n.digits),
          " (",
          format(round(
            sd(OUTCOME_M[TIME_M == wt.values[i] &
                           GROUP_M == levels(bw.factor)[j]], na.rm = TRUE), digits = n.digits
          ), nsmall = n.digits),
          ")",
          sep = ""
        ))
    }
  }
  
  mix.mod.res[1, ] <- rep(wt.labels, each = nlevels(bw.factor))
  mix.mod.res[2, ] <- rep(levels(bw.factor), length(wt.labels))
  mix.mod.res[3, ] <- N
  mix.mod.res[4, ] <- desfecho
  
  # p-valores para efeito de interação
  p.value <- mod1.aov[4, 4]
  if (p.value < alpha) {
    flag <- "*"
  } else {
    flag <- ""
  }
  if (p.value < 0.001) {
    p.value <- "<0.001"
  } else {
    p.value <- paste("=", format(round(p.value, digits = 3), nsmall = 3), sep = "")
  }
  interaction <- paste(
    "Interaction: F(",
    format(round(mod1.aov[4, 1], digits = 0), nsmall = 0),
    ",",
    format(round(mod1.aov[4, 2], digits = 0), nsmall = 0),
    ") = ",
    format(round(mod1.aov[4, 3], digits = 3), nsmall = 3),
    ", p",
    p.value,
    flag,
    sep = ""
  )
  
  # p-valores para efeitos principais
  for (i in 2:3) {
    p.value <- mod1.aov[i, 4]
    if (p.value < alpha) {
      flag <- "*"
    } else {
      flag <- ""
    }
    if (p.value < 0.001) {
      p.value <- "<0.001"
    } else {
      p.value <- paste("=", format(round(p.value, digits = 3), nsmall = 3), sep = "")
    }
    model.res[i - 1, ] <- paste(
      "Main effect: F(",
      format(round(mod1.aov[i, 1], digits = 0), nsmall = 0),
      ",",
      format(round(mod1.aov[i, 2], digits = 0), nsmall = 0),
      ") = ",
      format(round(mod1.aov[i, 3], digits = 3), nsmall = 3),
      ", p",
      p.value,
      flag,
      sep = ""
    )
  }
  model.res[3, 1] <- interaction
  
  # calcula e preenche a subtabela WITHIN-GROUP (SAME LINEAR MIXED MODEL)
  mult.within <- summary(pairs(emmeans::emmeans(mod1, ~ TIME_M |
                                                  GROUP_M), reverse = FALSE), infer = c(TRUE, TRUE))
  wt <- c()
  wt.pvalues <- c()
  for (i in 1:nlevels(bw.factor)) {
    group.data <- mult.within[mult.within[, 2] == levels(bw.factor)[i], ]
    group.data <- group.data[1:(length(wt.labels) - 1), ]
    # reverse signs due to mult.within order
    estimate <- -round(as.numeric(group.data[, 3]), digits = n.digits)
    low.ci <- -round(as.numeric(group.data[, 7]), digits = n.digits)
    upp.ci <- -round(as.numeric(group.data[, 6]), digits = n.digits)
    wt <- cbind(wt, t(paste(
      estimate, " (", low.ci, " to ", upp.ci, ")", sep = ""
    )))
    p.values <- as.numeric(group.data[, 9])
    for (k in 1:(length(wt.labels) - 1)) {
      ifelse(p.values[k] < 0.001,
             wt.pvalues <- c(wt.pvalues, "<0.001*"),
             "")
      ifelse(p.values[k] > 0.001 &
               p.values[k] < alpha,
             wt.pvalues <- c(wt.pvalues, paste(format(
               round(p.values[k], digits = 3), nsmall = 3
             ), "*", sep = "")),
             "")
      ifelse(p.values[k] > alpha, wt.pvalues <- c(wt.pvalues, format(
        round(p.values[k], digits = 3), nsmall = 3
      )), "")
    }
  }
  wt.diff[1, ] <- rep(paste(wt.labels[-1], wt.labels[1], sep = " - "), times = nlevels(bw.factor))
  wt.diff[2, ] <- rep(levels(bw.factor), each = length(wt.labels) - 1)
  wt.diff[4, ] <- wt
  wt.diff[5, ] <- wt.pvalues
  
  # calcula e preenche a subtabela BETWEEN-GROUP (NEW LINEAR MIXED MODEL: CHANGE WITH BASELINE AS COVARIATE)
  bw <- c()
  bw.pvalues <- c()
  smd.values <- c()
  
  for (i in 2:(length(wt.labels))) {
    ID <- seq(1:length(bw.factor))
    BASELINE_M <- matrix(OUTCOME_M, ncol = length(wt.labels), byrow = FALSE)[, 1]
    FOLLOWUP_M <- matrix(OUTCOME_M, ncol = length(wt.labels), byrow = FALSE)[, i]
    CHANGE_M <- FOLLOWUP_M - BASELINE_M
    COVARIATE_M <- COVARIATE_M[1:length(ID), ]
    
    if (!sjmisc::is_empty(covariate)) {
      df <- data.frame(ID, bw.factor, BASELINE_M, CHANGE_M, COVARIATE_M, check.names = FALSE)
      mod2 <- nlme::lme(
        as.formula(paste0("CHANGE_M ~ bw.factor * BASELINE_M + ", paste0(colnames(COVARIATE_M), collapse = " + "))),
        random = ~ 1 | ID,
        data = df
      )
    } else {
      df <- data.frame(ID, bw.factor, BASELINE_M, CHANGE_M, check.names = FALSE)
      mod2 <- nlme::lme(CHANGE_M ~ bw.factor + BASELINE_M, random = ~ 1 | ID, data = df)
    }
    
    mod2.sum <- summary(multcomp::glht(mod2, linfct = multcomp::mcp(bw.factor = "Tukey")),
                        test = multcomp::adjusted("holm"))
    
    confint_vals <- confint(mod2.sum, level = 1 - alpha)$confint
    group_comparisons <- rownames(confint_vals)
    
    # obter efeitos marginais e IC
    emm <- emmeans::emmeans(mod2, ~ bw.factor)
    pairs_emm <- summary(pairs(emm), infer = c(TRUE, TRUE), level = 1 - alpha)
    
    for (comp in seq_len(nrow(confint_vals))) {
      est <- round(confint_vals[comp, "Estimate"], digits = n.digits)
      lwr <- round(confint_vals[comp, "lwr"], digits = n.digits)
      upr <- round(confint_vals[comp, "upr"], digits = n.digits)
      pval <- summary(mod2.sum)$test$pvalues[comp]
      
      bw <- rbind(bw, paste0(est, " (", lwr, " to ", upr, ")"))
      
      if (pval < alpha) {
        flag <- "*"
      } else {
        flag <- ""
      }
      if (pval < 0.001) {
        pval <- "<0.001"
      } else {
        pval <- format(round(pval, digits = 3), nsmall = 3)
      }
      bw.pvalues <- rbind(bw.pvalues, paste0(pval, flag))
      
      # calcular SMD com base nas estimativas marginais
      smd_est <- pairs_emm[comp, "estimate"]
      smd_sd <- pairs_emm[comp, "SE"] * sqrt(2)  # pooled SD
      smd_d <- as.numeric(smd_est / smd_sd)
      smd_lower <- as.numeric(pairs_emm[comp, "lower.CL"] / smd_sd)
      smd_upper <- as.numeric(pairs_emm[comp, "upper.CL"] / smd_sd)
      
      estimate <- round(smd_d, digits = n.digits)
      lower <- round(smd_lower, digits = n.digits)
      upper <- round(smd_upper, digits = n.digits)
      smd.values <- rbind(smd.values, paste0(estimate, " (", lower, " to ", upper, ")"))
    }
  }
  
  bw.diff[1, ] <- rep(paste(wt.labels[-1], wt.labels[1], sep = " - "), each = choose(nlevels(bw.factor), 2))
  bw.diff[2, ] <- rep(group_comparisons, times = length(wt.labels) - 1)
  bw.diff[4, ] <- bw
  bw.diff[5, ] <- bw.pvalues
  bw.diff[6, ] <- smd.values

  # apresenta os resultados na tela
  print(
    paste(
      "Table 2a: Two-way linear mixed model analysis (",
      toString(variables),
      ").",
      sep = ""
    ),
    quote = FALSE
  )
  print(cbind(mix.mod.res, model.res), quote = FALSE)
  print(paste("Interaction effect: ", interaction, sep = ""), quote = FALSE)
  print(cbind(wt.diff), quote = FALSE)
  print(cbind(bw.diff), quote = FALSE)
  print(
    "SMD¹ = Standardized Mean Difference calculated from marginal estimates (Cohen's d).",
    quote = FALSE
  )
  print("", quote = FALSE)
  
  f.test.res <- cbind(c(t.labels[1:2], c("")), model.res[1:3, 1])
  
  # output results
  return(list(
    'mix.mod.res' = mix.mod.res,
    'wt.diff' = wt.diff,
    'bw.diff' = bw.diff,
    'f.test.res' = f.test.res
  ))
}
