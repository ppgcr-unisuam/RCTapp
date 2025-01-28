test.model.fit <- function(model.fit, dataset) {
  fit <- model.fit
  
  # par(mfrow = c(2, 5))
  
  # linearity of residues ?
  # linearity of quantitative predictors ?
  
  # Assessing Outliers
  print("Bonferroni's outlier detection")
  print(car::outlierTest(fit)) # Bonferonni p-value for most extreme obs
  print("")
  #    car::qqPlot(fit, main = "QQ Plot") #qq plot for studentized resid
  #    car::leveragePlots(fit) # leverage plots
  
  # Influential Observations
  # Cook's D plot
  D <- cooks.distance(fit)
  # identify D values > 4/(n-k-1)
  print("Cook's D cutoff (4/n)")
  cutoff <- 4 / (nrow(dataset))
  print(cutoff)
  print("")
  print("Cook's D largest than cutoff (4/n)")
  print(sort(D[D > cutoff], decreasing = TRUE))
  print("")
  #    plot(fit, which = 4, cook.levels = cutoff)
  # Influence Plot
  #    car::influencePlot(fit,
  #                  id.method = "identify",
  #                  main = "Influence Plot",
  #                  sub = "Circle size is proportinal to Cook's Distance")
  
  # Normality of Residuals
  # qq plot for studentized residuals
  #    car::qqPlot(fit, main = "QQ Plot")
  # distribution of studentized residuals
  sresid <- resid(fit) / sd(resid(fit))
  print("Shapiro-Wilk test for residues")
  print(shapiro.test(sresid)) # p value non-sign: normal distribution of residuals
  print("")
  #    hist(sresid, freq = FALSE,
  #         main = "Distribution of Studentized Residuals")
  #    xfit <- seq(min(sresid), max(sresid), length = 40)
  #    yfit <- dnorm(xfit)
  #    lines(xfit, yfit)
  
  # Evaluate homoscedasticity
  # Breusch-Pagan test
  # lmtest::bptest(fit)
  # plot studentized residuals vs. fitted values
  #    car::spreadLevelPlot(fit)
  
  # Evaluate Collinearity
  print("Variance inflation factors")
  print(car::vif(fit)) # variance inflation factors
  print("")
  print("Variance inflation factors (sqrt(vif) > 4")
  print(car::vif(fit)[sqrt(car::vif(fit)) > 2]) # problem?
  print("")
  
  # Evaluate Nonlinearity
  # component + residual plot
  #    car::crPlots(fit)
  # Ceres plots
  #    car::ceresPlots(fit)
  
  # Evaluate Non-independence of Errors
  # Test for Autocorrelated Errors
  #    car::durbinWatsonTest(fit)
}
