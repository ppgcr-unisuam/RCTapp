test.model.fit <- function(dataset,
                           variables,
                           covariate,
                           bw.factor,
                           wt.labels,
                           wt.values,
                           missing = c("complete.cases", "mean.imputation", "multiple.imputation"),
                           alpha,
                           p.digits = 3,
                           diagnostics) {
  
  # confirma a estrutura dos dados
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # remove variáveis não usadas
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  Little_Test_Res <- NULL
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
  }
  
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
  
  # epty plot
  Comp_Plus_Res <- NULL
  if ("Component-Plus-Residual" %in% diagnostics) {
    Comp_Plus_Res <- effects::Effect(c("TIME_M", "GROUP_M"), mod1, residuals = TRUE)
  }
  
  VIF <- NULL
  if("Variance Inflation Factor" %in% diagnostics){
    VIF <- car::vif(mod1)
  }
  
  return(list(
    'Little_Test_Res' = Little_Test_Res,
    'Comp_Plus_Res' = Comp_Plus_Res,
    'VIF' = VIF
  ))
}
