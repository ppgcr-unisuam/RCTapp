FIGURE.2 <- function(dataset,
                     variables,
                     covariate,
                     bw.factor,
                     wt.labels,
                     wt.values,
                     missing = c("Multiple imputation", "Mean imputation", "Complete cases"),
                     m.imputations,
                     xlabs,
                     ylab,
                     legend.opt,
                     alpha,
                     n.digits) {
  
  # defaults
  if (alpha == "") alpha <- 0.05
  if (n.digits == "") n.digits <- 3
  
  dataset <- data.frame(dataset, check.names = FALSE)
  bw.factor <- factor(bw.factor, exclude = NULL)
  dataset <- dataset[, colnames(dataset) %in% variables]
  
  ID_M <- rep(seq(1:length(bw.factor)), length(wt.labels))
  TIME_M <- as.factor(rep(wt.values, each = length(bw.factor)))
  GROUP_M <- rep(bw.factor, length(wt.labels))
  OUTCOME_M <- c(as.matrix(dataset))
  
  COVARIATE_M <- NULL
  if (!sjmisc::is_empty(covariate)) {
    COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
  }
  
  # decide como lidar com os dados perdidos
  if (missing == "Complete cases") {
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
  
  if (missing == "Mean imputation") {
    for (i in 1:length(wt.labels)) {
      temp.imp <- dataset[, i]
      for (j in 1:nlevels(bw.factor)) {
        temp.imp[which(is.na(temp.imp) & bw.factor == levels(bw.factor)[j])] <-
          mean(temp.imp[which(bw.factor == levels(bw.factor)[j])], na.rm = TRUE)
      }
      dataset[, i] <- temp.imp
    }
    OUTCOME_M <- c(as.matrix(dataset))
    
    if (!sjmisc::is_empty(covariate)) {
      covariate <- covariate %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      COVARIATE_M <- do.call(rbind, replicate(length(wt.labels), covariate, simplify = FALSE))
    }
  }
  
  if (missing == "Multiple imputation") {
    # Atenção: sem pooling real, apenas 1 imputação é usada para plot
    data_M <- if (!sjmisc::is_empty(covariate)) {
      data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
    } else {
      data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
    }
    
    ini <- mice::mice(data = data_M, maxit = 0)
    pred <- ini$pred
    pred["OUTCOME_M", "ID_M"] <- -2
    
    imp <- mice::mice(
      data_M,
      pred = pred,
      method = "pmm",
      m = m.imputations,
      seed = 0,
      print = FALSE,
      maxit = 50
    )
    
    data_M <- complete(imp, 1)  # usa a 1ª imputação
    TIME_M <- data_M$TIME_M
    GROUP_M <- data_M$GROUP_M
    OUTCOME_M <- data_M$OUTCOME_M
    if (!sjmisc::is_empty(covariate)) {
      COVARIATE_M <- data_M[, names(covariate), drop = FALSE]
    } else {
      COVARIATE_M <- NULL
    }
  }
  
  # cria base para modelo
  data_M <- if (!sjmisc::is_empty(covariate)) {
    data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, COVARIATE_M, check.names = FALSE)
  } else {
    data.frame(ID_M, TIME_M, GROUP_M, OUTCOME_M, check.names = FALSE)
  }
  
  if (!sjmisc::is_empty(covariate)) {
    mod <- nlme::lme(
      fixed = as.formula(paste0("OUTCOME_M ~ TIME_M * GROUP_M + ",
                                paste0(names(COVARIATE_M), collapse = " + "))),
      random = ~1 | ID_M / TIME_M,
      data = data_M
    )
  } else {
    mod <- nlme::lme(
      fixed = OUTCOME_M ~ TIME_M * GROUP_M,
      random = ~1 | ID_M / TIME_M,
      data = data_M
    )
  }
  
  emm <- emmeans::emmeans(mod, ~ TIME_M * GROUP_M)
  emm_df <- as.data.frame(emm)
  
  # Gráfico com escala de cinza, fontes maiores, sem grid, e eixos visíveis
  
  # Primeiro, converte TIME_M para numérico com base nos níveis
  emm_df$TIME_NUM <- as.numeric(as.character(emm_df$TIME_M))
  levels_map <- setNames(wt.values, wt.values)  # garante que wt.values sejam numéricos
  
  p <- ggplot2::ggplot(emm_df, ggplot2::aes(x = TIME_NUM, y = emmean, group = GROUP_M, color = GROUP_M)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(0.2)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.2), size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = emmean - SE * qnorm(1 - alpha / 2),
                                        ymax = emmean + SE * qnorm(1 - alpha / 2)),
                           width = 0.2, position = ggplot2::position_dodge(0.2)) +
    ggplot2::labs(x = xlabs, y = ylab, color = "Group") +
    ggplot2::scale_color_grey(start = 0.2, end = 0.8) +
    ggplot2::scale_x_continuous(breaks = wt.values, labels = wt.labels) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = legend.opt,
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black"),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 13),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 13)
    )  
  return(p)
}
