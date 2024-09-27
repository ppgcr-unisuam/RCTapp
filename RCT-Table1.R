TABLE.1 <- function(dataset, variables, bw.factor, max.levels, alpha, n.digits) {
    # LAST MODIFIED: AUG 24, 2022

    # This function outputs a descriptive table, by groups [continuous: mean
    # (SD); categorical: n (%)].  dataset: a 2D dataframe (rows: participants,
    # columns: variables) variables: a 1D variable labels bw.factor: a 1D
    # between-group factor max.levels: the maximum number of levels in a
    # categorical variable alpha: the type-I error level n.digits: number of
    # decimal places to be presented for continuous variables

    # default values
    if (max.levels == "") {
        max.levels = 2
    }
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

    # inicializa a matriz de resultados descritivos (+1 coluna para identificar
    # os níveis das variáveis categóricas)
    descript.res <- matrix("", nrow = length(variables), ncol = 1 + nlevels(bw.factor) +
        1)
    colnames(descript.res) <- c("Levels", paste("Group_", levels(bw.factor), sep = ""),
        "P-value")
    rownames(descript.res) <- variables

    # analisa as variáveis selecionadas
    for (i in 1:length(variables)) {
        # analisa as variaveis selecionadas
        categorical <- c()
        if (class(dataset[, i]) == "character") {
            # analise de variaveis categoricas calcula os parametros
            # descritivos
            categorical <- c(categorical, i)
            freq.abs <- format(round(t(table(factor(dataset[, i], exclude = NULL),
                bw.factor)), digits = 0), nsmall = 0)
            freq.rel <- format(round(t(table(factor(dataset[, i], exclude = NULL),
                bw.factor))/colSums(table(factor(dataset[, i], exclude = NULL), bw.factor)) *
                100, digits = n.digits), nsmall = n.digits)
            freq <- paste(freq.abs, rep(" (", length(freq.abs)), freq.rel, rep("%)",
                length(freq.abs)), sep = "")
            # compara os grupos
            p.value <- chisq.test(factor(dataset[, i], exclude = NULL), bw.factor,
                correct = TRUE)$p.value
            flag <- ""
            if (p.value < alpha) {
                flag <- "*"
            }
            if (p.value < 0.001) {
                p.value <- "<0.001*"
            }
            # preenche a tabela de resultados
            tempdata <- cbind(levels(factor(dataset[, i], exclude = NULL)), matrix(freq,
                nrow = nlevels(factor(dataset[, i], exclude = NULL)), byrow = TRUE),
                c(paste(format(round(p.value, digits = n.digits), nsmall = n.digits),
                  flag, sep = ""), rep("", nlevels(factor(dataset[, i], exclude = NULL)))))
            rownames(tempdata) <- c(colnames(dataset)[i], rep("", nlevels(factor(dataset[,
                i], exclude = NULL)) - 1))
            # sinaliza os dados perdidos
            tempdata[is.na(tempdata)] <- "Missing"
            descript.res <- rbind(descript.res, tempdata)
        } else {
            # analise de variaveis continuas calcula os parametros descritivos
            descript.res[i, 2:(1 + nlevels(bw.factor))] <- paste(format(round(tapply(as.numeric(dataset[,
                i]), bw.factor, mean, na.rm = TRUE), digits = n.digits), nsmall = n.digits),
                " (", format(round(tapply(as.numeric(dataset[, i]), bw.factor, sd,
                  na.rm = TRUE), digits = n.digits), nsmall = n.digits), ")", sep = "")
            # compara os grupos
            p.value <- anova(lm(as.numeric(dataset[, i]) ~ bw.factor))[[5]][1]
            flag <- ""
            if (p.value < alpha) {
                flag <- "*"
            }
            if (p.value < 0.001) {
                p.value <- "<0.001*"
            }
            # preenche a tabela de resultados
            descript.res[i, dim(descript.res)[2]] <- paste(format(round(p.value,
                digits = n.digits), nsmall = n.digits), flag, sep = "")
        }
    }

    # remove as variaveis nao analisadas
    descript.res <- descript.res[!apply(descript.res == "", 1, all), ]

    # apresenta os resultados na tela
    print("Table 1: Between-group descriptive analysis [mean (SD) or count (%)].",
        quote = FALSE)
    print(descript.res, quote = FALSE)
    print("", quote = FALSE)
    print("", quote = FALSE)
    return("descript.res")
}
