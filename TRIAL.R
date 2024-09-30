# fases
FASES <- c("BL", "12w", "26w", "52w")
FASES.N <- c(0, 12, 26, 52)

# com baseline
DESFECHO.1 <- colnames(cbind(INTENS_DOR, INTENS_DOR_1, INTENS_DOR_2, INTENS_DOR_3))
# sem baseline
DESFECHO.4 <- colnames(cbind(SATISF_1, SATISF_2, SATISF_3))
# COVARIAVEIS
COVARIATE <- NULL

# HANDLING MISSING VALUES: "complete.cases", "mean.imputation", "last.value.carried.forward"
MISSING <- "multiple.imputation"
M <- 5

TABLE.2a(dataset = BANCO, variables = DESFECHO.1, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
	alpha = ALPHA, n.digits = 2)

TABLE.2b(dataset = BANCO, variables = DESFECHO.3, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES[-1], missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)

FIGURE.1(dataset = BANCO, variables = DESFECHO.1, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", xlabs = "Weeks", 
	ylab = "Pain intensity", alpha = ALPHA)
