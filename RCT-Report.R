sap <- function(BGF,
                treatmentNames,
                controlgroup,
                timepointNames,
                timepointValues,
                alpha,
                showPvalue,
                effectSize,
                BV,
                OV,
                OutcomeName,
                COV,
                missing,
                MICEresamples,
                regressionDiag,
                OVRidit,
                OutcomeNameRidit) {
  
  # get package names
  packages <- grateful::scan_packages(
    pkgs = "All"
  )
  
  software <- paste0(
    "Statistical analysis was performed using RCTapp Shiny app (<a href=\"https://doi.org/10.5281/zenodo.13848816\" target=\"_blank\">https://doi.org/10.5281/zenodo.13848816</a>). ",
    "The following pagkages were used for data management, analysis, and visualization: ",
    paste0(packages[,1], " (", packages[,2], ")", collapse = ", "),
    "."
  )
  
  # create a text reporting the statistical analysis plan using the entries above
  table1 <- paste0(
    "Descriptive statistics were calculated for categorical variables using frequency tables with counts and percentages, and for continuous variables by reporting the mean and standard deviation across groups (",
    paste0(treatmentNames, collapse = ", "), ").",
    if(showPvalue){
      " Inferential statistics involved Fisher or chi-square tests for categorical variables and Welch or one-way analysis of variance for continuous variables to assess differences across groups."
    },
    " Missing data was reported as the total number and percentage of missing values for each variable."
  )
  
  table2 <- paste0(
    "The primary analysis was conducted using a linear mixed model (participants and time as random factors) to test for interaction and main effects of group (",
    paste0(treatmentNames, collapse = ", "),
    ") and time (" ,
    paste0(timepointNames, collapse = ", "),
    ") on the outcome variable (",
    OutcomeName,
    "), adjusted for baseline values.",
    if(!sjmisc::is_empty(controlgroup)){
      paste0(" The reference group was ", controlgroup, ".")
    },
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
    }
  )
  
  figure2 <- paste0(
    "Interaction plot was constructed to present the outcome variable (",
    OutcomeName,
    ") across groups (",
    paste0(treatmentNames, collapse = ", "),
    ") at baseline and follow-up time points (",
    paste0(timepointNames, collapse = ", "),
    ").",
    " Dots and vertical bars represent the mean and ", round((1-alpha)*100, digits = 0), "% confidence intervals, respectively."
  )
  
  if(!sjmisc::is_empty(OutcomeNameRidit)){
    table3 <- paste0(
      "Categorical variables (",
      OutcomeNameRidit,
      ") were analyzed using the Ridit method to estimate the probability of a participant in a treatment group having a different outcome value than a participant in the control group."
    )
  }
  
  p.value <- paste0(
    "Two-sided P values < ", format(round(alpha, digits = 3), nsmall = 3), " were considered to indicate statistical evidence of significance."
  )
  
  # gsub to remove parentesis of a text
  rule <- gsub("\\(", "", effectSize)
  rule <- gsub("\\)", "", rule)
  rule <- gsub(" ", "", rule)
  rule <- tolower(rule)
  
  e.size <- paste0(
    "Effect sizes for primary outcomes were calculated as Cohen's <i>d</i> (standardized mean difference, SMD) from estimated marginal means and standard error (SE) estimates from the primary adjusted analysis.",
    " According to ", effectSize, ", effect sizes were interpreted as ",
    esize_inter(rule),
    "."
  )
  
  report <- paste0(
    "<b>Software</b>",
    "<br>",
    software,
    "<br>",
    "<br>",
    "<b>Descriptive analysis</b>",
    "<br>",
    table1,
    "<br>",
    "<br>",
    "<b>Inferencial analysis</b>",
    "<br>",
    table2,
    "<br>",
    "<br>",
    figure2,
    "<br>",
    "<br>",
    table3,
    "<br>",
    "<br>",
    "<b>Significance and interpretation</b>",
    "<br>",
    p.value,
    "<br>",
    e.size
  )
  
  return(list(
    'software' = software,
    'table1' = table1,
    'table2' = table2,
    'figure2' = figure2,
    'table3' = table3,
    'p.value' = p.value,
    'e.size' = e.size,
    'report' = report
  ))
}
