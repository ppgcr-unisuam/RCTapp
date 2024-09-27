# LAST MODIFIED: AGO 22, 2024

# REINICIA TODAS AS VARIÁVEIS
rm(list = ls(all = TRUE))

# OCULTA TODOS OS AVISOS
options(warn = -1)
quartz.options(width = 9, height = 4, dpi = 150)

# INSTALA E ABRE OS PACOTES UTILIZADOS
# most packages work fine if installed from CRAN
packs.cran <-
  c(
    "emmeans",
    "flextable",
    "foreign",
    "lme4",
    "MASS",
    "Matrix",
    "mice",
    "miceadds",
    "mitml",
    "multcomp",
    "mvtnorm",
    "misty",
    "nlme",
    "readxl",
    "ridittools",
    "Rmisc",
    "stddiff",
    "survival",
    "TH.data"
  )

for (i in 1:length(packs.cran)) {
  if (!require(packs.cran[i], character.only = TRUE, quietly = TRUE))
    install.packages(packs.cran[i], character.only = TRUE)
}

# ANÁLISE COMPARATIVA
source("RCT-Figure1.R") # numeric variables, plot of descriptive analysis (mean and CI)
source("RCT-Table2a.R") # numeric variables, linear mixed model analysis, between- AND within-factor WITH baseline adjustment
source("RCT-Table2b.R") # numeric variables, linear mixed model analysis, between- AND within-factor WITHOUT baseline adjustment
source("RCT-Table3.R") #  ordinal variables, ridit analysis, ONLY within-factor
source("RCT-Missingness.R") # missing data analysis

# CARREGA O BANCO
Banco_RCT_RIO_2021 <- data.frame(readxl::read_excel("Banco_RCT_RIO_2021.xlsx", sheet = 1))
attach(Banco_RCT_RIO_2021)
GRUPOS <- Banco_RCT_RIO_2021[, 2]
GRUPOS <- factor(GRUPOS, levels = c("0", "1"), labels = c("CFT", "Control"))
FASES <- c("BL", "12w", "26w", "52w")
FASES.N <- c(0, 12, 26, 52)
ALPHA <- 0.05

# DELINEMANENTO DO ESTUDO
DESFECHO.1 <- colnames(cbind(INTENS_DOR, INTENS_DOR_1, INTENS_DOR_2, INTENS_DOR_3))
DESFECHO.2 <- colnames(cbind(ODI_TOTAL, ODI_1_TOTAL, ODI_2, ODI_3))
DESFECHO.3 <- colnames(cbind(PERC_EF_1, PERC_EF_2, PERC_EF_3))
DESFECHO.5 <- colnames(cbind(CAT_B, CAT, CAT_2, CAT_3))
DESFECHO.6 <- colnames(cbind(FEAR_B, FEAR, FEAR_2, FEAR_3))
DESFECHO.7 <- colnames(cbind(ANX_B, ANX, ANX_2, ANX_3))
DESFECHO.8 <- colnames(cbind(DEP_M_B, DEP_M, DEP_M_2, DEP_M_3))
DESFECHO.9 <- colnames(cbind(ISO_B, ISO, ISOL_2, ISO_3))
DESFECHO.10 <- colnames(cbind(STRESS_B, STRESS, STRESS_2, STRESS_3))
# sem baseline
DESFECHO.4 <- colnames(cbind(SATISF_1, SATISF_2, SATISF_3))

# COVARIAVEIS
COVARIATE <- NULL

# HANDLING MISSING VALUES: "complete.cases", "mean.imputation", "last.value.carried.forward"
MISSING <- "multiple.imputation"
M <- 5

dev.new()
layout(matrix(seq(1:10), nrow = 2, ncol = 5, byrow = TRUE), heights = rep(1), widths = rep(1, 1, 1))

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.1, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
	alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.1, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", xlabs = "Weeks", 
	ylab = "Pain intensity", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.2, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
	alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.2, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
	xlabs = "Weeks", ylab = "Disability", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.5, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.5, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "CAT", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.6, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.6, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "FEAR_B", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.7, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.7, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "ANX_B", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.8, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.8, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "DEP_M_B", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.9, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.9, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "ISO_B", alpha = ALPHA)

TABLE.2a(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.10, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES, missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.10, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N, missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "STRESS_B", alpha = ALPHA)

TABLE.2b(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.3, bw.factor = GRUPOS, covariate = COVARIATE, control.g = "Control", wt.labels = FASES[-1], missing = MISSING, m.imputations = M, 
         alpha = ALPHA, n.digits = 2)
FIGURE.1(dataset = Banco_RCT_RIO_2021, variables = DESFECHO.3, bw.factor = GRUPOS, covariate = COVARIATE, wt.labels = FASES.N[-1], missing = "mean.imputation", m.imputations = M, 
         xlabs = "Weeks", ylab = "PERC_EF", alpha = ALPHA)

detach(Banco_RCT_RIO_2021)
