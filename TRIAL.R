# sem baseline
DESFECHO.4 <- colnames(cbind(SATISF_1, SATISF_2, SATISF_3))
# COVARIAVEIS
COVARIATE <- NULL

TABLE.2b(dataset = BANCO, variables = DESFECHO.3, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES[-1], missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
