TABLE.3 <- function(dataset, variables, bw.factor, control.g, wt.labels, alpha, n.digits) {
    # This function outputs a comparison table for two-way cross-tables
    # dataset: a 2D dataframe (rows: participants, columns: variables)
    # variables: a 1D variable labels (within-group) bw.factor: a 1D
    # between-group factor control.g: a character label for the control group
    # in ridit analysis wt.labels: a 1D variable labels for each level alpha:
    # the type-I error level n.digits: number of decimal places to be presented
    # for continuous variables

    # default values
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

    # inicializa a matriz de resultados
    t.labels <- c("Between-subjects", "Level", wt.labels)
    c.labels <- seq(min(na.omit(dataset)), max(na.omit(dataset)))
    cross.tab.res <- matrix("", nrow = length(t.labels), ncol = nlevels(bw.factor) *
        length(c.labels))
    rownames(cross.tab.res) <- t.labels
    colnames(cross.tab.res) <- rep("", nlevels(bw.factor) * length(c.labels))

    # matriz de resultados entre-grupos
    bw.diff <- matrix("", nrow = length(t.labels), ncol = 1)
    rownames(bw.diff) <- t.labels
    colnames(bw.diff) <- rep("", 1)
    cross.tab.res[1, ] <- rep(levels(bw.factor), each = length(c.labels))
    cross.tab.res[2, ] <- rep(c.labels, nlevels(bw.factor))

    # calcula e preenche o N e o DESFECHO da tabela de resultados
    z.p <- c()
    for (i in 1:length(wt.labels)) {
        desfecho <- c()
        for (j in 1:nlevels(bw.factor)) {
            N <- table(factor(dataset[bw.factor == levels(bw.factor)[j], i], levels = c.labels))
            N.perc <- format(round(N/sum(N) * 100, digits = n.digits), nsmall = n.digits)
            desfecho <- cbind(desfecho, t(paste(N, " (", N.perc, "%)", sep = "")))
        }
        # ridit analysis
        r1 <- meanridits(table(dataset[, i], bw.factor), margin = 2, ref = control.g)[levels(bw.factor) ==
            control.g]
        r2 <- meanridits(table(dataset[, i], bw.factor), margin = 2, ref = control.g)[levels(bw.factor) !=
            control.g]
        se.diff <- seriditdiff(table(dataset[, i], bw.factor)[levels(bw.factor) ==
            control.g], table(dataset[, i], bw.factor)[levels(bw.factor) != control.g])
        z.score <- abs((r2 - r1)/se.diff)
        p.value <- 2 * pnorm(abs(z.score), lower.tail = F)
        if (p.value < alpha) {
            flag <- "*"
        } else {
            flag <- ""
        }
        if (p.value < 0.001) {
            p.value <- "<0.001"
        } else {
            p.value <- paste("=", format(round(p.value, digits = 3), nsmall = 3),
                sep = "")
        }
        z.p <- rbind(z.p, paste("z=", format(round(z.score, digits = 3), nsmall = 3),
            ", p", p.value, sep = ""))
        cross.tab.res[i + 2, ] <- desfecho
    }
    bw.diff[, 1] <- rbind("", "", z.p)

    # apresenta os resultados na tela
    print("Table 3: Two-way cross-table analysis.", quote = FALSE)
    print(cbind(cross.tab.res, bw.diff), quote = FALSE)
    print("", quote = FALSE)

    # missingness analysis
    missing.data(dataset = dataset, variables = variables, covariate = NULL)

    return("cross.tab.res")
}
