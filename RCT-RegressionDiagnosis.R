test.model.fit <- function(dataset,
                           variables,
                           covariate,
                           bw.factor,
                           wt.labels,
                           wt.values,
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
  TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
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
  names(COVARIATE_M) <- names(covariate)
  
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
  
  Comp_Plus_Res <- NULL
  if ("Component-Plus-Residual" %in% diagnostics) {
    Comp_Plus_Res <- effects::Effect(c("TIME_M", "GROUP_M"), mod1, residuals = TRUE)
  }
  
  Inf_Fixed <- NULL
  Inf_Random <- NULL
  if("Influence" %in% diagnostics){
    Inf_Fixed <- car::infIndexPlot(influence(mod1, "ID_M"))
    Inf_Random <- car::infIndexPlot(influence(mod1, "ID_M"), var = "var.cov.comps")
  }
  
  VIF <- NULL
  if("Variance Inflation Factor" %in% diagnostics){
    VIF <- car::vif(mod1)
  }
  
  return(list(
    'Comp_Plus_Res' = Comp_Plus_Res,
    'Inf_Fixed' = Inf_Fixed,
    'Inf_Random' = Inf_Random,
    'VIF' = VIF
  ))
}
