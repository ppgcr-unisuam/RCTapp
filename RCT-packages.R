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
