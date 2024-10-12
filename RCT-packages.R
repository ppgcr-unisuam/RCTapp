# most packages work fine if installed from CRAN
packs.cran <-
  c(
    "emmeans::emmeans",
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
    "survival"
  )

# load all libraries
for (pack in packs.cran) {
  suppressMessages(library(pack, character.only = TRUE))
}

# patch to handle rank-deficient fixed model matrix in lme
remotes::install_github("bbolker/nlme")