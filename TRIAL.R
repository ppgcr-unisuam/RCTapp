FASES.N <- c(0, 12, 26, 52)

# sem baseline
DESFECHO.4 <- colnames(cbind(SATISF_1, SATISF_2, SATISF_3))
# COVARIAVEIS
COVARIATE <- NULL

TABLE.2b(dataset = BANCO, variables = DESFECHO.3, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES[-1], missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
tbl.mix.mod.res <- cbind(row.names = rownames(tbl$mix.mod.res), tbl$mix.mod.res)
tbl.mix.mod.res <- as.data.frame(tbl.mix.mod.res, row.names = rownames(tbl.mix.mod.res))
colnames(tbl.mix.mod.res)[1] <- "Variable"

tbl.wt.diff <- cbind(row.names = rownames(tbl$wt.diff), tbl$wt.diff)
tbl.wt.diff <- as.data.frame(tbl.wt.diff, row.names = rownames(tbl.wt.diff))
colnames(tbl.wt.diff)[1] <- "Variable"

tbl.bw.diff <- cbind(row.names = rownames(tbl$bw.diff), tbl$bw.diff)
tbl.bw.diff <- as.data.frame(tbl.bw.diff, row.names = rownames(tbl.bw.diff))
colnames(tbl.bw.diff)[1] <- "Variable"

flextable::save_as_docx(
  flextable::flextable(tbl.mix.mod.res),
  flextable::flextable(tbl.wt.diff),
  flextable::flextable(tbl.bw.diff),
  path = "Table 2a (outcome 2) UNFORMATTED.docx"
)


FIGURE.1(dataset = BANCO, variables = DESFECHO.1, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", xlabs = "Weeks", 
	ylab = "Pain intensity", alpha = ALPHA)
