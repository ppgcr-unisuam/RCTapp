sap <- function(BGF,
                treatmentNames,
                controlgroup,
                endpointNames,
                endpointValues,
                alpha,
                BV,
                OV,
                OutcomeName,
                COV,
                missing,
                MICEresamples,
                regressionDiag,
                OVRidit,
                OutcomeNameRidit) {
  
  # create a text reporting the statistical analysis plan using the entries above
  table1 <- paste0(
    "<b>Descriptive analysis</b><br>",
    "<br>"
  )
  
  table2 <- paste0(
    "<b>Inferencial analysis</b><br>",
    "The primary analysis was conducted using a linear mixed model to test for interaction and main effects of group (",
    paste0(treatmentNames, collapse = ", "),
    ") and time (" ,
    paste0(endpointNames, collapse = ","),
    ") on the outcome variable (",
    OutcomeName,
    ").",
    "<br>"
  )
  
  table3 <- paste0(
    "<b>Riddit analysis</b><br>",
    "<br>"
  )
  
  report <- paste(table1, table2, table3, sep = "<br>")
  
  return(list('report' = report))
}