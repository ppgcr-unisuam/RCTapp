missing.data <-
  function(test.missing,
           dataset,
           variables,
           covariate,
           digits = 3,
           p.digits = 3) {
    # This function outputs the Little's Missing Completely at Random (MCAR) Test
    # test.missing: the missing data test to be performed
    # dataset: a 2D dataframe (rows: participants, columns: variables)
    # variables:
    # digits: number of digits to display
    # p.digits: number of digits to display the p-values
    
    # confirma a estrutura dos dados
    dataset <- data.frame(dataset)
    
    # remove variáveis não usadas
    dataset <- dataset[, colnames(dataset) %in% variables]
    if (!is.null(covariate)) {
      dataset <- cbind(dataset, covariate)
    }
    
    # Little's MCAR test
    missingtest <- misty::na.test(
      dataset,
      digits = digits,
      p.digits = p.digits
    )
    
    if(test.missing == "Little"){
      missingtest.res <- paste0(
          "Little's Missing Completely at Random (MCAR) Test: ",
        paste(
          paste(
            names(missingtest$result$little),
            format(missingtest$result$little, digits = 3),
            sep = " = "
          ),
          collapse = ", "
        ))
    }
    
    print(missingtest.res, quote = FALSE)
    print("", quote = FALSE)
    return(missingtest.res)
  }
