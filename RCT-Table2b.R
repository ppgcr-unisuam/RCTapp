TABLE.2b <- function(dataset,
                     variables,
                     covariate,
                     bw.factor,
                     control.g,
                     wt.labels,
                     wt.values,
                     missing = c("complete.cases", "mean.imputation", "multiple.imputation"),
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
  dataset <- data.frame(dataset)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # remove variáveis não usadas
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  # inicializa a matriz de resultados
  t.labels <- c("Within-subjects",
                "Between-subjects",
                "N",
                "Outcome",
                "P-value")
  mix.mod.res <- matrix("",
                        nrow = length(t.labels),
                        ncol = nlevels(bw.factor) * length(wt.labels))
  rownames(mix.mod.res) <- t.labels
  colnames(mix.mod.res) <- rep("", nlevels(bw.factor) * length(wt.labels))
  
  # matriz de resultados intra-grupo
  wt.diff <- matrix("",
                    nrow = length(t.labels),
                    ncol = nlevels(bw.factor) * (length(wt.labels) -
                                                   1))
  rownames(wt.diff) <- t.labels
  colnames(wt.diff) <- rep("", nlevels(bw.factor) * (length(wt.labels) - 1))
  
  # matriz de resultados entre-grupos
  bw.diff <- matrix("", nrow = length(t.labels) + 1, ncol = (length(wt.labels)))
  rownames(bw.diff) <- c(t.labels, "SMD¹")
  colnames(bw.diff) <- rep("", (length(wt.labels)))
  
  # matriz de resultados do modelo
  model.res <- matrix("", nrow = length(t.labels), ncol = 1)
  rownames(model.res) <- t.labels
  colnames(model.res) <- c("Mixed-model")
  
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
    for (i in 1:length(wt.labels)) {
      COVARIATE_M <- rbind(COVARIATE_M, covariate)
    }
  }
  
  # decide como lidar com os dados perdidos
  if (missing == "complete.cases") {
    include <- complete.cases(dataset)
    dataset <- dataset[include == TRUE, ]
    covariate <- covariate[include == TRUE, ]
    bw.factor <- bw.factor[include == TRUE]
    ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
    TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
    GROUP_M <- rep(bw.factor, length(wt.labels))
    OUTCOME_ORIG <- c(as.matrix(dataset))
    OUTCOME_M <- c(as.matrix(dataset))
    COVARIATE_M <- c()
    if (!sjmisc::is_empty(covariate)) {
      for (i in 1:length(wt.labels)) {
        COVARIATE_M <- rbind(COVARIATE_M, covariate)
      }
    }
  }
  
  if (missing == "mean.imputation") {
    # calcula a média para imputação para cada grupo
    for (i in 1:length(wt.labels)) {
      temp.imp <- dataset[, i]
      for (j in 1:nlevels(bw.factor)) {
        temp.imp[which(is.na(temp.imp) &
                         bw.factor == levels(bw.factor)[j])] <-
          mean(temp.imp[which(bw.factor == levels(bw.factor)[j])], na.rm = TRUE)
      }
      dataset[, i] <- temp.imp
    }
    OUTCOME_M <- c(as.matrix(dataset))
    COVARIATE_M <- c()
    if (!sjmisc::is_empty(covariate)) {
      for (i in 1:length(wt.labels)) {
        for (j in 1:ncol(covariate)) {
          covariate[is.na(covariate[, j])] <- mean(covariate[, j], na.rm = TRUE)
        }
        COVARIATE_M <- rbind(COVARIATE_M, covariate)
      }
    }
  }
  
  if (missing == "multiple.imputation") {
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
    COVARIATE_M <- c()
    if (!sjmisc::is_empty(covariate)) {
      for (i in 1:length(wt.labels)) {
        for (j in 1:ncol(covariate)) {
          covariate[is.na(covariate[, j])] <- mean(covariate[, j], na.rm = TRUE)
        }
        COVARIATE_M <- rbind(COVARIATE_M, covariate)
      }
    }
  }
  
  # cria o dataset com os valores após imputação ou não de dados
  if (!sjmisc::is_empty(covariate)) {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
  } else {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
  }
  
  # fit linear mixed model
  if (missing != "multiple.imputation") {
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
        method = "2l.pan",
        m = m.imputations,
        seed = 0,
        print = FALSE
      )
    if (!is.null(covariate)) {
      mod1 <-
        with(data = imp,
             nlme::lme(
               fixed = as.formula(paste0(
                 "OUTCOME_M ~ TIME_M * GROUP_M + ",
                 paste0(colnames(COVARIATE_M), collapse = " + ")
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
                             GROUP_M ==levels(bw.factor)[j]], na.rm = TRUE), digits = n.digits
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
    "F(",
    format(round(mod1.aov[4, 1], digits = 0), nsmall = 0),
    ",",
    format(round(mod1.aov[4, 2], digits = 0), nsmall = 0),
    ")=",
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
  
  # calcula e preenche a subtabela BETWEEN-GROUP (SAME LINEAR MIXED MODEL)
  bw <- c()
  bw.pvalues <- c()
  smd.values <- c()
  mult.between <- summary(pairs(emmeans::emmeans(mod1, ~ GROUP_M |
                                                   TIME_M), reverse = FALSE), infer = c(TRUE, TRUE))
  for (i in 1:(length(wt.labels))) {
    group.data <- mult.between[i, ]
    # reverse signs due to mult.within order
    estimate <- -round(as.numeric(group.data[, 3]), digits = n.digits)
    low.ci <- -round(as.numeric(group.data[, 7]), digits = n.digits)
    upp.ci <- -round(as.numeric(group.data[, 6]), digits = n.digits)
    bw <- cbind(bw, t(paste(
      estimate, " (", low.ci, " to ", upp.ci, ")", sep = ""
    )))
    p.values <- as.numeric(group.data[, 9])
    ifelse(p.values < 0.001, bw.pvalues <- c(bw.pvalues, "<0.001*"), "")
    ifelse(p.values > 0.001 &
             p.values < alpha,
           bw.pvalues <- c(bw.pvalues, paste(format(
             round(p.values, digits = 3), nsmall = 3
           ), "*", sep = "")),
           "")
    ifelse(p.values > alpha, bw.pvalues <- c(bw.pvalues, format(round(
      p.values, digits = 3
    ), nsmall = 3)), "")
    
    group_data <- as.character(bw.factor)
    group_data[bw.factor == bw.factor] <- 0
    group_data[bw.factor != control.g] <- 1
    data <- data.frame(group_data, OUTCOME_M[TIME_M == i])
    smd <- stddiff::stddiff.numeric(data = data,
                           gcol = 1,
                           vcol = 2)
    estimate <- round(smd[7], digits = n.digits)
    lower <- round(smd[8], digits = n.digits)
    upper <- round(smd[9], digits = n.digits)
    smd.values <- rbind(smd.values, t(paste(
      estimate, " (", lower, " to ", upper, ")", sep = ""
    )))
    
  }
  bw.diff[1, ] <- wt.labels
  bw.diff[2, ] <- rep(paste(levels(bw.factor)[2], levels(bw.factor)[1], sep = " - "), length(wt.labels))
  bw.diff[4, ] <- bw
  bw.diff[5, ] <- bw.pvalues
  bw.diff[6, ] <- smd.values
  
  # apresenta os resultados na tela
  print(
    paste(
      "Table 2b: Two-way linear mixed model analysis (",
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
  
  # output results
  return(list(
    'mix.mod.res' = mix.mod.res,
    'wt.diff' = wt.diff,
    'bw.diff' = bw.diff
  ))
}
