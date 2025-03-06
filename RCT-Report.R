sap <- function(BGF,
                treatmentNames,
                controlgroup,
                endpointNames,
                endpointValues,
                alpha,
                showPvalue,
                BV,
                OV,
                OutcomeName,
                COV,
                missing,
                MICEresamples,
                regressionDiag,
                OVRidit,
                OutcomeNameRidit) {
  
  software <- paste0(
    "<b>Software</b><br>",
    "Statistical analysis was performed using RCTapp Shiny app (<a href=\"https://doi.org/10.5281/zenodo.13848816\" target=\"_blank\">https://doi.org/10.5281/zenodo.13848816</a>).",
    "<br>",
    "<br>"
  )
  
  # create a text reporting the statistical analysis plan using the entries above
  table1 <- paste0(
    "<b>Descriptive analysis</b><br>",
    "Descriptive statistics were calculated for categorical variables using frequency tables with counts and percentages, and for continuous variables by reporting the mean and standard deviation across groups (",
    paste0(treatmentNames, collapse = ", "), ").",
    if(showPvalue){
      " Inferential statistics involved chi-square tests for categorical variables and one-way ANOVA (t-test) for continuous variables to assess differences across groups."
    },
    " Missing data was reported as the total number and percentage of missing values for each variable.",
    "<br>",
    "<br>"
  )
  
  table2 <- paste0(
    "<b>Inferencial analysis</b><br>",
    "The primary analysis was conducted using a linear mixed model (participants and time as random factors) to test for interaction and main effects of group (",
    paste0(treatmentNames, collapse = ", "),
    ") and time (" ,
    paste0(endpointNames, collapse = ", "),
    ") on the outcome variable (",
    OutcomeName,
    "), adjusted for baseline.",
    if(!sjmisc::is_empty(COV)){
      paste0(" The model was also adjusted for the following covariates: ", paste0(COV, collapse = ", "), ".")
    },
    if(!!sjmisc::is_empty(missing)){
      paste0(" Missing data was handled using ",
             if(missing == "Multiple imputation"){
               paste0("Multiple imputation by Chained Equations (predictive mean matching method) with ", MICEresamples, " resamples.")
             } else if (missing == "Mean imputation"){
               "mean imputation."
             } else if (missing == "Complete cases"){
               "complete case analysis."
             }
      )
    },
    if(!sjmisc::is_empty(regressionDiag)){
      paste0(" Regression diagnostics were performed to check the assumptions of the linear mixed model, specifically regarding ",
             tolower(paste0(regressionDiag, collapse = ", ")), ".")
    },
    " Effect sizes for primary outcomes were calculated as Cohen's <i>d</i> (standardized mean difference, SMD) from estimated marginal means and standard error (SE) estimates from the primary adjusted analysis.",
    " Two-sided P values < ", format(round(alpha, digits = 3), nsmall = 3), " were considered to indicate statistical evidence of significance.",
    "<br>"
  )
  
  figure2 <- paste0(
    "Interaction plot was constructed to presente the outcome variable (",
    OutcomeName,
    ") across groups (",
    paste0(treatmentNames, collapse = ", "),
    ") at baseline and follow-up time points (",
    paste0(endpointNames, collapse = ", "),
    ").",
    " Dots and vertical bars represent the mean and ", round((1-alpha)*100, digits = 0), "% confidence intervals, respectively.",
    "<br>",
    "<br>"
  )
  
  table3 <- paste0(
    "<b>Riddit analysis</b><br>",
    "Categorical variables (",
    OutcomeNameRidit,
    ") were analyzed using the Ridit method to estimate the probability of a participant in a treatment group having a different outcome value than a participant in the control group.",
    "<br>"
  )
  
  report <- paste0(software, table1, table2, figure2, table3, collapse = "<br>")
  
  return(list('report' = report))
}
