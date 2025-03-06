FIGURE.2 <-
  function(dataset,
           variables,
           covariate,
           bw.factor,
           wt.labels,
           wt.values,
           missing = c("complete.cases", "mean.imputation", "multiple.imputation"),
           m.imputations,
           xlabs,
           ylab,
           legend.opt,
           alpha,
           n.digits) {
    # This function outputs a plot for two-way mixed-models.
    # dataset: a 2D dataframe (rows: participants, columns: variables)
    # variables: a 1D variable labels (within-group)
    # covariate: a 2D dataframe (rows: participants, columns: covariates)
    # bw.factor: a 1D between-group factor
    # wt.labels: a 1D variable labels for each level
    # wt.values: a 1D vector of values for each level
    # missing: method for handling missing balues (mean inputation, last value carried forward)
    # xlabs: a 1D vector of labels for the X axis (within-group factor)
    # ylabs: a string label for the Y axis (outcome)
    # n.digits: number of decimal places to be presented for continuous variables
    
    quiet <- function(x) {
      sink(tempfile())
      on.exit(sink())
      invisible(force(x))
    }
    
    # default values
    if (alpha == "") {
      alpha = 0.05
    }
    if (n.digits == "") {
      n.digits = 3
    }
    
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
    }
    
    # decide como lidar com os dados perdidos
    if (missing == "complete.cases") {
      if (!sjmisc::is_empty(covariate)) {
        include <- complete.cases(dataset, covariate)
      } else {
        include <- complete.cases(dataset)
      }
      dataset <- dataset[include, ]
      covariate <- covariate[include, ]
      bw.factor <- bw.factor[include]
      ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
      TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
      GROUP_M <- rep(bw.factor, length(wt.labels))
      OUTCOME_ORIG <- c(as.matrix(dataset))
      OUTCOME_M <- c(as.matrix(dataset))
      COVARIATE_M <- NULL
      if (!sjmisc::is_empty(covariate)) {
        COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
      }
    }
    
    if (missing == "mean.imputation") {
      # Calculate the mean for imputation for each group
      for (i in 1:length(wt.labels)) {
        temp.imp <- dataset[, i]
        for (j in 1:nlevels(bw.factor)) {
          temp.imp[which(is.na(temp.imp) & bw.factor == levels(bw.factor)[j])] <-
            mean(temp.imp[which(bw.factor == levels(bw.factor)[j])], na.rm = TRUE)
        }
        dataset[, i] <- temp.imp
      }
      OUTCOME_M <- c(as.matrix(dataset))
      
      # mean imputation of covariate data if any
      COVARIATE_M <- NULL
      if (!sjmisc::is_empty(covariate)) {
        covariate <- covariate %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
      }
    }
    
    if (missing == "multiple.imputation") {
      dataset <- dataset
      bw.factor <- bw.factor
      ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
      TIME_M <- as.factor(c(rep(wt.values, each = length(bw.factor))))
      GROUP_M <- rep(bw.factor, length(wt.labels))
      OUTCOME_ORIG <- c(as.matrix(dataset))
      OUTCOME_M <- c(as.matrix(dataset))
      # mean imputation of baseline data if any
      OUTCOME_M[which(is.na(OUTCOME_M) &
                        TIME_M == levels(TIME_M)[1])] <-
        mean(OUTCOME_M[which(!is.na(OUTCOME_M) &
                               TIME_M == levels(TIME_M)[1])])
      # mean imputation of covariate data if any
      COVARIATE_M <- NULL
      if (!sjmisc::is_empty(covariate)) {
        covariate <- covariate %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
        COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
      }
    }
    
    # cria o dataset com os valores após imputação ou não de dados
    if (!sjmisc::is_empty(covariate)) {
      data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
    } else {
      data_M <- data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
    }
    
    # fit linear mixed model
    if (missing != "multiple.imputation") {
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
      mod1.aov <- anova(mod1)
    } else {
      ini <- mice::mice(data = data_M, maxit = 0)
      pred <- ini$pred
      pred["OUTCOME_M", "ID_M"] <- -2
      imp <-
        mice::mice(
          data_M,
          pred = pred,
          method = "pmm",
          m = m.imputations,
          seed = 0,
          print = FALSE,
          maxit = 50
        )
      if (!sjmisc::is_empty(covariate)) {
        mod1 <-
          with(data = imp,
               nlme::lme(
                 fixed = as.formula(paste0(
                   "OUTCOME_M ~ TIME_M * GROUP_M + ",
                   paste0(names(COVARIATE_M), collapse = " + ")
                 )),
                 random = ~ 1 | ID_M / TIME_M
               ))
        mod1.aov <- quiet(miceadds::mi.anova(imp, formula = paste0(
          "OUTCOME_M ~ TIME_M * GROUP_M + ",
          paste0(names(COVARIATE_M), collapse = " + ")
        )))
      } else {
        mod1 <-
          with(data = imp,
               nlme::lme(
                 fixed = OUTCOME_M ~ TIME_M * GROUP_M,
                 random = ~ 1 | ID_M / TIME_M
               ))
        mod1.aov <- quiet(miceadds::mi.anova(imp, formula = "OUTCOME_M ~ TIME_M * GROUP_M"))
      }
      mod1.aov <- mod1.aov$anova.table[1:3, 2:5]
      mod1.aov <- rbind(rep(NA, 4), mod1.aov)
    }
    
    # calcula e preenche o N e o DESFECHO da tabela de resultados
    N <- c()
    desfecho <- c()
    media <- c()
    p.values <- c()
    for (i in 1:length(wt.labels)) {
      for (j in 1:nlevels(bw.factor)) {
        # dados válidos
        N <-
          rbind(N, (paste("(n = ", sum(
            !is.na(OUTCOME_ORIG[TIME_M == wt.values[i] &
                                  GROUP_M == levels(bw.factor)[j]])
          ), ")", sep = "")))
        # média (DP)
        desfecho <-
          rbind(desfecho, paste(
            format(round(
              mean(OUTCOME_M[TIME_M == wt.values[i] &
                               GROUP_M == levels(bw.factor)[j]], na.rm = TRUE), digits = n.digits
            ), nsmall = n.digits),
            " (",
            format(round(
              sd(OUTCOME_M[TIME_M == wt.values[i] &
                             GROUP_M == levels(bw.factor)[j]], na.rm = TRUE), digits = n.digits
            ), nsmall = n.digits),
            ")",
            sep = ""
          ))
      }
    }
    
    # p-valores para efeito de interação
    p.value <- mod1.aov[4, 4]
    if (p.value < alpha) {
      flag <- "*"
    } else {
      flag <- ""
    }
    if (p.value < 0.001) {
      p.value <- "<0.001"
    } else {
      p.value <- paste("=", format(round(p.value, digits = 3), nsmall = 3), sep = "")
    }
    interaction <- paste(
      "Interaction: F(",
      format(round(mod1.aov[4, 1], digits = 0), nsmall = 0),
      ",",
      format(round(mod1.aov[4, 2], digits = 0), nsmall = 0),
      ") = ",
      format(round(mod1.aov[4, 3], digits = 3), nsmall = 3),
      ", p",
      p.value,
      flag,
      sep = ""
    )
    
    # calculate CI
    myCI <-
      Rmisc::group.CI(
        OUTCOME_M ~ GROUP_M * as.factor(TIME_M),
        data = cbind(GROUP_M, as.factor(TIME_M), OUTCOME_M),
        ci = 1 - alpha
      )
    myCI[, 2] <- rep(wt.labels, each = nlevels(bw.factor))
    
    # add symbols and horizontal jitter
    jitter <-
      rep(seq(
        from = -0.15,
        to = 0.15,
        length.out = nlevels(bw.factor)
      ),
      each = length(wt.labels))
    jitter <-
      matrix(
        jitter,
        nrow = nlevels(bw.factor),
        ncol = length(unique(wt.labels)),
        byrow = TRUE
      )
    symbols <- c(15, 16, 17, 18)
    
    # interaction plot with %CI
    
    # set narrow margins
    par(mar = c(5, 5, 1, 1))
    
    # start empty area plot with main setup
    plot(
      NA,
      xaxt = "n",
      xlab = "Endpoint",
      ylab = ylab,
      xlim = c(min(wt.values) - 1.5, max(wt.values) + 1.5),
      ylim = c(min(myCI[, 5]) - min(myCI[, 5]) *
                 0.2, max(myCI[, 3]) + max(myCI[, 3]) * 0.2)
    )
    
    # plot CI intervals (vertical lines)
    for (i in 1:dim(myCI)[1]) {
      lines(
        x = rep(wt.values[which(myCI[i, 2] == wt.labels)], 2) + jitter[i],
        y = c(myCI[i, 5], myCI[i, 3]),
        lty = "solid",
        lwd = 1.5,
        col = "darkgrey"
      )
      axis(side = 1,
           at = wt.values,
           labels = wt.labels)
    }
    # plot CI intervals (symbols at end lines)
    for (i in 1:dim(myCI)[1]) {
      points(
        x = rep(wt.values[which(myCI[i, 2] == wt.labels)], 2) + jitter[i],
        y = c(myCI[i, 5], myCI[i, 3]),
        pch = "−",
        cex = 1.5,
        lwd = 1.5,
        col = "darkgrey"
      )
      axis(side = 1,
           at = wt.values,
           labels = wt.labels)
    }
    # plot point estimates
    for (i in 1:nlevels(bw.factor)) {
      lines(
        x = wt.values + jitter[i],
        y = myCI[myCI[, 1] == i, 4],
        type = "b",
        pch = symbols[i],
        lwd = 1,
        cex = 1
      )
      axis(side = 1,
           at = wt.values,
           labels = wt.labels)
    }
    # add interaction label
    if (mod1.aov[4, 4] < alpha) {
      text(
        x = min(wt.values) - 1.5,
        y = min(myCI[, 5]) - min(myCI[, 5]) * 0.2,
        labels = interaction,
        adj = c(0, 0),
        cex = 0.75
      )
    }
    if(legend.opt != "none"){
      # plot legend
      legend(
        x = legend.opt,
        legend = levels(GROUP_M),
        pch = symbols,
        cex = 1.25,
        bty = "n",
        horiz = FALSE,
        x.intersp = 1
      )
    }
  }
