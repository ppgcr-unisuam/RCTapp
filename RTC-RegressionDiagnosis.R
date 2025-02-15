test.model.fit <- function(model.fit, dataset, diagnostics) {
  fit <- model.fit
  
  # Outliers
  print("Bonferroni's outlier detection")
  print(car::outlierTest(fit)) # Bonferonni p-value for most extreme obs
  print("")

  # Influential Observations
  print("Cook's influential observations")
  print("Cook's D cutoff (4/n)")
  D <- cooks.distance(fit)
  # identify D values > 4/(n-k-1)
  cutoff <- 4 / (nrow(dataset))
  print(cutoff)
  print("")
  print("Cook's D largest than cutoff (4/n)")
  print(sort(D[D > cutoff], decreasing = TRUE))
  print("")

  # Normality of Residuals
  print("Shapiro-Wilk test for normality of residues")
  sresid <- resid(fit) / sd(resid(fit))
  print(shapiro.test(sresid)) # p value non-sign: normal distribution of residuals
  print("")

  # Homoscedasticity
  print("Breusch-Pagan test for homoscedasticity")
  lmtest::bptest(fit)

  # Collinearity
  print("Multicollinearity")
  print(car::vif(fit)) # variance inflation factors
  print("")
  print("Variance inflation factors (sqrt(vif) > 4")
  print(car::vif(fit)[sqrt(car::vif(fit)) > 2]) # problem?
  print("")
  
  # Non-independence of Errors
  print("Durbin-Watson test for autocorrelation")
  car::durbinWatsonTest(fit)
}
