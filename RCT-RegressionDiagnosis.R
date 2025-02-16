test.model.fit <- function(dataset,
                           variables,
                           covariate,
                           bw.factor,
                           wt.labels,
                           alpha,
                           p.digits = 3,
                           diagnostics) {
  
  # confirma a estrutura dos dados
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  
  # remove variáveis não usadas
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  # preparação e análise do modelo misto
  ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
  TIME_M <-
    as.factor(c(rep(seq(
      1, length(wt.labels)
    ), each = length(bw.factor))))
  GROUP_M <- rep(bw.factor, length(wt.labels))
  OUTCOME_ORIG <- c(as.matrix(dataset))
  OUTCOME_M <- c(as.matrix(dataset))
  COVARIATE_M <- c()
  if (!sjmisc::is_empty(covariate)) {
    for (i in 1:length(wt.labels)) {
      COVARIATE_M <- rbind(COVARIATE_M, covariate)
    }
    names(COVARIATE_M) <- names(covariate)
  }
  
  # decide como lidar com os dados perdidos
  # "complete.cases" only for original dataset diagnosis
  include <- complete.cases(dataset)
  dataset <- dataset[include == TRUE, ]
  covariate <- covariate[include == TRUE, ]
  bw.factor <- bw.factor[include == TRUE]
  ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
  TIME_M <-
    as.factor(c(rep(seq(
      1, length(wt.labels)
    ), each = length(bw.factor))))
  GROUP_M <- rep(bw.factor, length(wt.labels))
  OUTCOME_ORIG <- c(as.matrix(dataset))
  OUTCOME_M <- c(as.matrix(dataset))
  COVARIATE_M <- c()
  if (!sjmisc::is_empty(covariate)) {
    for (i in 1:length(wt.labels)) {
      COVARIATE_M <- rbind(COVARIATE_M, covariate)
    }
  }
  names(COVARIATE_M) <- names(covariate)
  
  # cria o dataset com os valores após imputação ou não de dados
  if (!sjmisc::is_empty(covariate)) {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
  } else {
    data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
  }
  
  # fit linear mixed model (same as Table 2a and Tableb 2b)
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
  
  if (sjmisc::is_empty(diagnostics)) {
    diagnostics.res <- "Regression diagnosis not analyzed."
  } else {
    diagnostics.res <- c()
  }
  
  outliers.res <- "Bonferroni's outlier detection not analyzed."
  if ("Outlier detection" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      outliers.res,
      sep = "<br/>"
    )
  }
  
  influencial.res <- "Cook's influential observations not analyzed."  
  if ("Influencial observations" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      influencial.res,
      sep = "<br/>"
    )
  }
  
  normality.res <- "Shapiro-Wilk test for normality of residues not analyzed."
  if ("Normality of residues" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      normality.res,
      sep = "<br/>"
    )
  }
  
  homocedasticity.res <- "Breusch-Pagan test for homoscedasticity not analyzed."
  if ("Homocedasticity" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      homocedasticity.res,
      sep = "<br/>"
    )
  }
  
  multicollinearity.res <- "Multicollinearity not analyzed."
  if ("Multicollinearity" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      multicollinearity.res,
      sep = "<br/>"
    )
  }
  
  autocorrelation.res <- "Autocorrelation not analyzed."
  if ("Autocorrelation" %in% diagnostics) {
    diagnostics.res <- paste0(
      diagnostics.res,
      autocorrelation.res,
      sep = "<br/>"
    )
  }
  
  # # Influential Observations
  # print("Cook's influential observations")
  # print("Cook's D cutoff (4/n)")
  # D <- cooks.distance(fit)
  # # identify D values > 4/(n-k-1)
  # cutoff <- 4 / (nrow(dataset))
  # print(cutoff)
  # print("")
  # print("Cook's D largest than cutoff (4/n)")
  # print(sort(D[D > cutoff], decreasing = TRUE))
  # print("")
  #
  # # Normality of Residuals
  # print("Shapiro-Wilk test for normality of residues")
  # sresid <- resid(fit) / sd(resid(fit))
  # print(shapiro.test(sresid)) # p value non-sign: normal distribution of residuals
  # print("")
  #
  # # Homoscedasticity
  # print("Breusch-Pagan test for homoscedasticity")
  # lmtest::bptest(fit)
  #
  # # Collinearity
  # print("Multicollinearity")
  # print(car::vif(fit)) # variance inflation factors
  # print("")
  # print("Variance inflation factors (sqrt(vif) > 4")
  # print(car::vif(fit)[sqrt(car::vif(fit)) > 2]) # problem?
  # print("")
  #
  # # Non-independence of Errors
  # print("Durbin-Watson test for autocorrelation")
  # car::durbinWatsonTest(fit)
  
  return(list(
    'diagnostics.res' = diagnostics.res
  ))
}
