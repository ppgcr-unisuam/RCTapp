# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# create file with citations
grateful::cite_packages(
  output = "file",
  out.dir = file.path(getwd(), "www"),
  out.format = "html",
  pkgs = "All"
)

# copy favicon folder to the www dir
R.utils::copyDirectory(from = "favicon_io", to = file.path(dir.name, "favicon_io"))

# load libraries
library(dplyr)
library(htmltools)

# source all scripts
source("RCT-NA.R", local = TRUE) # to convert specific values to NA
source("RCT-Table1.R", local = TRUE) # numeric and categorical variables, descriptive analysis, between-factor
source("RCT-Table2a.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITH baseline adjustment
source("RCT-Table2b.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITHOUT baseline adjustment
source("RCT-Figure2.R", local = TRUE) # numeric variables, plot of descriptive analysis (mean and CI)
source("RCT-Table3.R", local = TRUE) #  ordinal variables, ridit analysis, ONLY within-factor
source("RCT-Diagnosis.R", local = TRUE) # diagnosis on original dataset and model
source("RCT-Report.R", local = TRUE) # report the statistical analysis plan in text format
source("RCT-EffectSizes.R", local = TRUE) # effect size interpretation and reporting in text format

# use this code to debug
# rsconnect::showLogs()

ui <- shiny::fluidPage(
  rintrojs::introjsUI(),
  
  # add custom CSS to justify
  tags$style('ul.nav-pills{display: flex !important;justify-content: center !important;}'),
  
  # add favicon
  shiny::tags$head(
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/favicon.ico"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/android-chrome-192x192.png"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/android-chrome-512x512.png"),
    shiny::tags$link(rel = "apple-touch-icon", href = "www/favicon_io/apple-touch-icon.png"),
    shiny::tags$link(rel = "icon", href = "www/favicon_io/favicon-16x16.png"),
    shiny::tags$link(rel = "icon", href = "www/favicon_io/favicon-32x32.png"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/favicon.ico"),
  ),
  
  # add font awesome
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  # use shinythemes
  theme = shinythemes::shinytheme("flatly"),
  
  # use shinyjs
  shinyjs::useShinyjs(),
  
  # status bar
  shinybusy::add_busy_bar(color = "#FF0000"),
  
  # Application title
  shiny::titlePanel(
    list(
      fontawesome::fa("users"),
      fontawesome::fa("shuffle"),
      fontawesome::fa("users"),
      shiny::HTML("<strong>RCTapp</strong>")
    ),
    windowTitle = "RCTapp | Randomized Clinical Trial"
  ),
  
  shiny::HTML(
    "<a href=\"https://doi.org/10.5281/zenodo.13848815\" style=\"vertical-align:middle;\"><img src=\"https://zenodo.org/badge/DOI/10.5281/zenodo.10439718.svg\" alt=\"DOI\"  style=\"vertical-align:top;\"></a>"
  ),
  
  shiny::br(),
  shiny::br(),
  
  # change color of fileInput button
  tags$head(tags$style(
    shiny::HTML(".btn-file {background-color: #2C3E50;}")
  )),
  
  # Main panel for displaying outputs ----
  shiny::tabsetPanel(
    id = "tabs",
    type = "tabs",
    shiny::tabPanel(
      title = list(fontawesome::fa("database"), "Data"),
      # split panel into 2 columns
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::br(),
          rintrojs::introBox(
            shiny::wellPanel(
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::actionButton(
                    inputId = "guide1",
                    icon = shiny::icon("info-circle"),
                    label = shiny::HTML("<strong>Study design</strong>"),
                    style = "color: black; background-color: transparent; border-color: transparent;"
                  ),
                ),
                style = "text-align:center;"
              ),
              shiny::br(),
              shiny::tabsetPanel(
                id = "tabset1",
                shiny::tabPanel(
                  title = "Setup",
                  shiny::br(),
                  # add checkbox for between-subject factors (BGF)
                  shinyWidgets::virtualSelectInput(
                    inputId = "BGF",
                    label = "Treatment groups",
                    choices = NULL,
                    selected = NA,
                    showValueAsTags = TRUE,
                    search = TRUE,
                    multiple = FALSE,
                    width = "100%"
                  ),
                  # show options for control group
                  shinyWidgets::virtualSelectInput(
                    inputId = "controlgroup",
                    label = "Control group",
                    choices = NULL,
                    selected = NA,
                    showValueAsTags = TRUE,
                    search = TRUE,
                    multiple = FALSE,
                    width = "100%"
                  ),
                  # number of endpoits
                  shiny::numericInput(
                    inputId = "endpointN",
                    label = "Endpoints",
                    value = 2,
                    min = 2,
                    step = 1,
                    width = "100%"
                  ),
                  # show text for endpoint
                  shiny::textInput(
                    inputId = "endpointValues",
                    label = "Endpoint value (csv)",
                    value = "0,1",
                    width = "100%"
                  ),
                  shiny::fluidRow(
                    shiny::column(
                      6,
                      # alpha level for statistical significance
                      shiny::numericInput(
                        inputId = "alpha",
                        label = "Alpha level",
                        value = 0.05,
                        min = 0.001,
                        max = 0.999,
                        step = 0.001,
                        width = "100%"
                      ),
                    ),
                    shiny::column(
                      6,
                      # effect size interpretation rules
                      shinyWidgets::virtualSelectInput(
                        inputId = "effectSize",
                        label = "Effect size",
                        choices = c("Cohen (1988)", "Gignac (2016)", "Sawilowsky (2009)", "Lovakov (2021)"),
                        selected = "Cohen (1988)",
                        showValueAsTags = TRUE,
                        search = FALSE,
                        multiple = FALSE,
                        width = "100%"
                      ),
                    ),
                  ),
                ),
                shiny::tabPanel(
                  title = "Aesthetics",
                  shiny::br(),
                  # show text input to change treatment group names
                  shiny::textInput(
                    inputId = "treatmentNames",
                    label = "Treatment labels (csv)",
                    value = "Control,Intervention",
                    width = "100%"
                  ),
                  # show text for endpoint names
                  shiny::textInput(
                    inputId = "endpointNames",
                    label = "Endpoint labels (csv)",
                    value = "Baseline,Follow-up",
                    width = "100%"
                  ),
                ),
              ),
            ),
            data.step = 2,
            data.intro = "Set up the treatment groups selecting a variable name from the dataset.Define the control group and the number of endpoints. Specify the endpoint values in equal time units (e.g., days). Define the alpha level for statistical significance and the effect size interpretation rule.
            <br>
            <br>
            Adjust the aesthetics of the treatment and endpoint by providing custom labels."
          ),
        ),
        shiny::column(
          9,
          shiny::br(),
          rintrojs::introBox(
            shiny::wellPanel(
              shiny::fluidRow(
                shiny::column(
                  11,
                  # input Excel file data
                  shiny::fileInput(
                    inputId = "InputFile",
                    label = NULL,
                    multiple = FALSE,
                    buttonLabel = list(fontawesome::fa("file-excel"), "Upload"),
                    accept = c(".xlsx"),
                    width = "100%",
                    placeholder = "XLSX file"
                  ),
                  style = "text-align:center;"
                ),
                shiny::column(
                  1,
                  shiny::tags$a(
                    id = "restart",
                    class = "btn btn-primary",
                    href = "javascript:history.go(0)",
                    shiny::HTML('<i class="fa fa-refresh fa-1x"></i>'),
                    title = "restart",
                    style = "color:white; border-color:white; border-radius:100%;"
                  ),
                  style = "text-align:center;"
                ),
              ),
              DT::DTOutput(outputId = "rawtable"),
            ),
            data.step = 1,
            data.intro = "Upload the data in wide format (one row per subject, multiple columns for repeated measures)."
          ),
        ),
      ),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("list-check"), "Analysis"),
      # split panel into 3 columns
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::br(),
          
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                9,
                shiny::actionButton(
                  inputId = "guide2",
                  icon = shiny::icon("info-circle"),
                  label = shiny::HTML("<strong>Participants'characteristics</strong>"),
                  style = "color: black; background-color: transparent; border-color: transparent;"
                ),
              ),
              shiny::column(
                3,
                # add action button to run table 1
                shiny::actionButton(
                  inputId = "runTable1",
                  icon = shiny::icon("play"),
                  label = "Table 1",
                  style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
                ),
              ),
              style = "text-align:center;"
            ),
            shiny::br(),
            shiny::tabsetPanel(
              id = "tabset1",
              shiny::tabPanel(
                title = "Setup",
                shiny::br(),
                # add checkbox for baseline variables (BV)
                shinyWidgets::virtualSelectInput(
                  inputId = "BV",
                  label = "Baseline variables (all types)",
                  choices = NULL,
                  selected = NA,
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  width = "100%"
                ),
                # add checkbox for show p-value
                shiny::checkboxInput(
                  inputId = "showPvalue",
                  label = "Show P-value",
                  value = FALSE,
                  width = "100%"
                ),
              ),
              shiny::tabPanel(
                title = "Aesthetics",
                shiny::br(),
                # show maxlevels for between-subject factors
                shiny::numericInput(
                  inputId = "maxlevels",
                  label = "Max levels (categorical variables only)",
                  value = 5,
                  min = 1,
                  max = 10,
                  step = 1,
                  width = "100%"
                ),
              ),
            ),
          ),
        ),
        shiny::column(
          4,
          shiny::br(),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  inputId = "guide3",
                  icon = shiny::icon("info-circle"),
                  label = shiny::HTML("<strong>Numerical outcome</strong>"),
                  style = "color: black; background-color: transparent; border-color: transparent;"
                ),
              ),
              shiny::column(
                3,
                # add button to run analysis
                shiny::actionButton(
                  inputId = "runTable2",
                  icon = shiny::icon("play"),
                  label = "Table 2",
                  style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
                ),
              ),
              shiny::column(
                3,
                # add button to run analysis
                shiny::actionButton(
                  inputId = "runFigure2",
                  icon = shiny::icon("play"),
                  label = "Figure 2",
                  style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
                ),
              ),
              style = "text-align:center;"
            ),
            shiny::br(),
            # add tabset
            shiny::tabsetPanel(
              id = "tabset2",
              shiny::tabPanel(
                title = "Setup",
                shiny::br(),
                # add checkbox for outcome variables (OV)
                shinyWidgets::virtualSelectInput(
                  inputId = "OV",
                  label = "Outcome variable (all columns)",
                  choices = NULL,
                  selected = NA,
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  width = "100%"
                ),
                # show checkbox for indicating outcomes has baseline data
                shiny::checkboxInput(
                  inputId = "hasBaseline",
                  label = "Baseline data is available",
                  value = TRUE,
                  width = "100%"
                ),
                # add checkbox for covariates (COV)
                shinyWidgets::virtualSelectInput(
                  inputId = "COV",
                  label = "Covariates",
                  choices = NULL,
                  selected = NA,
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  width = "100%"
                ),
                shiny::fluidRow(
                  shiny::column(
                    8,
                    # show options for missing data
                    shinyWidgets::virtualSelectInput(
                      inputId = "missing",
                      label = "Missing data imputation",
                      choices = c("Multiple imputation", "Mean imputation", "Complete cases"),
                      selected = "Multiple imputation",
                      showValueAsTags = TRUE,
                      search = TRUE,
                      multiple = FALSE,
                      width = "100%"
                    ),
                  ),
                  shiny::column(
                    4,
                    # number of resamples
                    shiny::numericInput(
                      inputId = "MICEresamples",
                      label = "Resamples",
                      value = 50,
                      min = 1,
                      max = 100,
                      step = 1,
                      width = "100%"
                    ),
                  ),
                ),
                # show options for regression diagnosis
                shinyWidgets::virtualSelectInput(
                  inputId = "regressionDiag",
                  label = "Diagnosis",
                  choices = c(
                    "Scaled Residuals",
                    "Component-Plus-Residual",
                    "Variance Inflation Factor",
                    "Missing Data",
                    "Imputed Data",
                    "Convergence"
                  ),
                  selected = c(
                    "Scaled Residuals",
                    "Component-Plus-Residual",
                    "Variance Inflation Factor",
                    "Missing Data",
                    "Imputed Data",
                    "Convergence"
                  ),
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  width = "100%"
                ),
              ),
              shiny::tabPanel(
                title = "Aesthethics",
                shiny::br(),
                # show text input to change outcome name
                shiny::textInput(
                  inputId = "OutcomeName",
                  label = "Outcome label",
                  value = "Outcome",
                  width = "100%"
                ),
                # options for legend
                shinyWidgets::virtualSelectInput(
                  inputId = "legendOptions",
                  label = "Legend position",
                  choices = c(
                    "none",
                    "top",
                    "topleft",
                    "topright",
                    "bottom",
                    "bottomleft",
                    "bottomright",
                    "left",
                    "right",
                    "center"
                  ),
                  selected = "top",
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = FALSE,
                  width = "100%"
                ),
              ),
            ),
          ),
        ),
        # add column for buttons
        shiny::column(
          4,
          shiny::br(),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(
                9,
                shiny::actionButton(
                  inputId = "guide4",
                  icon = shiny::icon("info-circle"),
                  label = shiny::HTML("<strong>Categorical outcome</strong>"),
                  style = "color: black; background-color: transparent; border-color: transparent;"
                ),
              ),
              shiny::column(
                3,
                # add button to run analysis
                shiny::actionButton(
                  inputId = "runTable3",
                  icon = shiny::icon("play"),
                  label = "Table 3",
                  style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
                ),
              ),
              style = "text-align:center;"
            ),
            shiny::br(),
            shiny::tabsetPanel(
              id = "tabset4",
              shiny::tabPanel(
                title = "Setup",
                shiny::br(),
                # add checkbox for outcome variables (OV)
                shinyWidgets::virtualSelectInput(
                  inputId = "OVRidit",
                  label = "Outcome variable (all columns)",
                  choices = NULL,
                  selected = NA,
                  showValueAsTags = TRUE,
                  search = TRUE,
                  multiple = TRUE,
                  width = "100%"
                ),
              ),
              shiny::tabPanel(
                title = "Aesthetics",
                shiny::br(),
                # show text input to change outcome name
                shiny::textInput(
                  inputId = "OutcomeNameRidit",
                  label = "Outcome label",
                  value = "Outcome",
                  width = "100%"
                ),
              ),
            ),
          ),
        ),
      ),
    ),
    # tab for reporting the statistical analysis plan
    shiny::tabPanel(
      title = "Plan",
      icon = fontawesome::fa("file-alt"),
      shiny::wellPanel(
        # show text output
        shiny::htmlOutput("SAP"),
        shiny::br(),
        # download Word format
        shiny::downloadButton(
          outputId = "downloadSAP",
          label = "Download SAP (.DOCX)",
          style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
        ),
        shiny::br(),
        shiny::br(),
      ),
    ),
    # tab for table 1 of results
    shiny::tabPanel(
      title = "Table 1",
      icon = fontawesome::fa("table"),
      shiny::wellPanel(
        # show table of results
        DT::dataTableOutput("table1"),
        shiny::br(),
        # download Word format
        shiny::downloadButton(
          outputId = "downloadTable1",
          label = "Download Table 1 (.DOCX)",
          style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
        ),
        shiny::br(),
        shiny::br(),
      ),
    ),
    # tab for table 2 of results
    shiny::tabPanel(
      title = "Table 2",
      icon = fontawesome::fa("table"),
      shiny::wellPanel(
        # show table of results
        DT::dataTableOutput("table2"),
        shiny::br(),
        # show output text of table 2
        shiny::htmlOutput("textTable2"),
        # download Word format
        shiny::br(),
        shiny::downloadButton(
          outputId = "downloadTable2",
          label = "Download Table 2 (.DOCX)",
          style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
        ),
        shiny::br(),
        shiny::br(),
      ),
    ),
    # tab for plot of results
    shiny::tabPanel(
      title = "Figure 2",
      icon = fontawesome::fa("chart-line"),
      shiny::wellPanel(
        # show plot of results
        shiny::plotOutput("plot"),
        shiny::br(),
        # download TIFF format
        shiny::downloadButton(
          outputId = "downloadFigure2",
          label = "Download Figure 2 (.TIFF)",
          style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
        ),
        shiny::br(),
        shiny::br(),
      ),
    ),
    # tab for table 3 of results
    shiny::tabPanel(
      title = "Table 3",
      icon = fontawesome::fa("table"),
      shiny::wellPanel(
        # show table of results
        DT::dataTableOutput("table3"),
        shiny::br(),
        # download Word format
        shiny::downloadButton(
          outputId = "downloadTable3",
          label = "Download Table 3 (.DOCX)",
          style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
        ),
        shiny::br(),
        shiny::br(),
      ),
    ),
    shiny::tabPanel(
      icon = list(fontawesome::fa("stethoscope")),
      title = "Diagnostics",
      shiny::wellPanel(
        # add tab panel
        shiny::tabsetPanel(
          id = "diagTab",
          type = "pills",
          shiny::tabPanel(
            title = "Scaled Residuals",
            icon = fontawesome::fa("chart-column"),
            shiny::br(),
            # add plot
            shiny::plotOutput("plotDiag3"),
            shiny::br(),
            # show output text from scaled residuals
            shiny::htmlOutput("tableDiag3"),
          ),
          shiny::tabPanel(
            title = "Component-Plus-Residual",
            icon = fontawesome::fa("table-columns"),
            shiny::br(),
            # add plot
            shiny::plotOutput("plotDiag5")
          ),
          shiny::tabPanel(
            title = "Variance Inflation Factor",
            icon = fontawesome::fa("table"),
            shiny::br(),
            # add table
            shiny::tableOutput("tableDiag6")
          ),
          shiny::tabPanel(
            title = "Missing Data",
            icon = fontawesome::fa("person-circle-question"),
            shiny::br(),
            # add plot
            shiny::plotOutput("plotDiag1"),
            shiny::br(),
            # show output text of missing data
            shiny::htmlOutput("tableDiag1"),
          ),
          shiny::tabPanel(
            title = "Imputed Data",
            icon = fontawesome::fa("arrows-to-dot"),
            shiny::br(),
            # add plot
            shiny::plotOutput("plotDiag2")
          ),
          shiny::tabPanel(
            title = "Convergence",
            icon = fontawesome::fa("chart-line"),
            shiny::br(),
            # add plot
            shiny::plotOutput("plotDiag4")
          ),
        ),
      ),
    ),
    shiny::tabPanel(
      icon = list(fontawesome::fa("book-open")),
      title = "References",
      shiny::br(),
      # session info text output
      shiny::htmlOutput("gratrep")
    ),
    shiny::tabPanel(
      icon = list(fontawesome::fa("file-lines")),
      title = "Publications",
      shiny::br(),
      shiny::HTML(
        "Castro, J., Correia, L., Donato, B. de S., Arruda, B., Agulhari, F., Pellegrini, M. J., Belache, F. T. C., de Souza, C. P., Fernandez, J., Nogueira, L. A. C., Reis, F. J. J., Ferreira, A. de S., & Meziat-Filho, N. (2022). Cognitive functional therapy compared with core exercise and manual therapy in patients with chronic low back pain: randomised controlled trial. In Pain (Vol. 163, Issue 12, pp. 2430–2437). Ovid Technologies (Wolters Kluwer Health). <a href=\"https://doi.org/10.1097/j.pain.0000000000002644 \">https://doi.org/10.1097/j.pain.0000000000002644</a>"
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "Avila, L., da Silva, M. D., Neves, M. L., Abreu, A. R., Fiuza, C. R., Fukusawa, L., de Sá Ferreira, A., & Meziat-Filho, N. (2023). Effectiveness of Cognitive Functional Therapy Versus Core Exercises and Manual Therapy in Patients With Chronic Low Back Pain After Spinal Surgery: Randomized Controlled Trial. In Physical Therapy (Vol. 104, Issue 1). Oxford University Press (OUP). <a https://doi.org/10.1093/ptj/pzad105 \">https://doi.org/10.1093/ptj/pzad105</a>"
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "de Lira MR, Meziat-Filho N, Zuelli Martins Silva G, et al Efficacy of cognitive functional therapy for pain intensity and disability in patients with non-specific chronic low back pain: a randomised sham-controlled trial British Journal of Sports Medicine Published Online First: 06 March 2025. <a https://doi.org/10.1136/bjsports-2024-109012 \">https://doi.org/10.1136/bjsports-2024-109012</a>"
      ),
      shiny::br(),
    ),
    shiny::tabPanel(
      icon = list(fontawesome::fa("people-group")),
      title = "Credits",
      shiny::br(),
      shiny::HTML(
        "<b>Arthur de Sá Ferreira, DSc</b> (Developer)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0001-7014-2002"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0001-7014-2002
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Ney Meziat Filho, DSc</b> (Developer)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0003-2794-7299"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0003-2794-7299
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Fabiana Terra Cunha, DSc</b> (Contributor)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0002-2043-8494"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0002-2043-8494
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Jessica Fernandez, DSc</b> (Contributor)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0003-0047-9659"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0003-0047-9659
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Julia Castro, DSc</b> (Contributor)"),
      shiny::br(),
      shiny::HTML(
        '<a id="cy-effective-orcid-url" class="underline" 
         href="https://orcid.org/0000-0003-0511-5715"
         target="orcid.widget" rel="me noopener noreferrer" style="vertical-align: top">
         <img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" style="width: 1em; margin-inline-start: 0.5em" alt="ORCID iD icon"/>
         https://orcid.org/0000-0003-0511-5715
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<b>Centro Universitário Augusto Motta</b> (Affiliation) <br>
        Programa de Pós-graduação em Ciências da Reabilitação <br>
        Rio de Janeiro, RJ, Brazil"),
      shiny::br(),
      shiny::HTML(
        '<a
         href="https://ror.org/02ab1bc46"
         style="vertical-align: top">
         <img src="https://ror.org/assets/ror-logo-small-671ea83ad5060ad5c0c938809aab4731.png" style="width: 1em; margin-inline-start: 0.5em"/>
         https://ror.org/02ab1bc46
         </a>'
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>License</b>"),
      shiny::HTML(
        "This work is licensed under an <a rel=\"license\" data-spdx=\"Apache-2.0\" href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache License 2.0</a>."
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>Citation</b>"),
      shiny::HTML(
        "Ferreira, A. de S., & Meziat Filho, N. (2024). RCTapp Randomized Clinical Trial (1.0.0). Zenodo. <a href=\"https://doi.org/10.5281/zenodo.13848816\" target=\"_blank\">https://doi.org/10.5281/zenodo.13848816</a>"
      ),
      shiny::br(),
      shiny::br(),
    ),
  ),
)

# Define server script
server <- function(input, output, session) {
  
  # start introjs when button is pressed with custom options and events
  shiny::observeEvent(input$guide1,
                      rintrojs::introjs(session, options = list("nextLabel"="Next",
                                                                "prevLabel"="Previous",
                                                                "skipLabel"="Skip"))
  )
  
  # start introjs when button is pressed with custom options and events
  shiny::observeEvent(input$guide2,
                      rintrojs::introjs(session, options = list("nextLabel"="Next",
                                                                "prevLabel"="Previous",
                                                                "skipLabel"="Skip"))
  )
  
  # start introjs when button is pressed with custom options and events
  shiny::observeEvent(input$guide3,
                      rintrojs::introjs(session, options = list("nextLabel"="Next",
                                                                "prevLabel"="Previous",
                                                                "skipLabel"="Skip"))
  )
  
  # start introjs when button is pressed with custom options and events
  shiny::observeEvent(input$guide4,
                      rintrojs::introjs(session, options = list("nextLabel"="Next",
                                                                "prevLabel"="Previous",
                                                                "skipLabel"="Skip"))
  )
  
  # upload excel file ---------------------------------------------------------
  values <- shiny::reactiveValues(upload_state = NULL)
  
  shiny::observeEvent(input$InputFile, {
    values$upload_state <- 'uploaded'
  })
  
  shiny::observeEvent(input$restart, {
    values$upload_state <- 'restart'
  })
  
  # enable resamples if multiple imputation is differente from complete cases
  shiny::observeEvent(input$missing, {
    if (input$missing == "Multiple imputation") {
      shinyjs::enable("MICEresamples")
    } else {
      shinyjs::disable("MICEresamples")
    }
  })
  
  # create automatic endopoint names and values based on endpointN
  shiny::observeEvent(input$endpointN, {
    shiny::updateTextInput(session,
                           "endpointNames",
                           value = paste0("T", 1:input$endpointN, collapse = ","))
    shiny::updateTextInput(session,
                           "endpointValues",
                           value = paste0(0:(input$endpointN - 1), collapse = ","))
  })
  
  shiny::observeEvent(input$BGF, {
    shiny::req(rawdata())
    shiny::req(input[["BGF"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # update treatmentNames after number of levels from BGV
    shiny::updateTextInput(
      inputId = "treatmentNames",
      value = paste0(unique(rawdata[[input[["BGF"]]]]), collapse = ",")
    )
  })
  
  # observe BGF to select missing
  shiny::observeEvent(input$OV, {
    shiny::req(rawdata())
    shiny::req(input[["OV"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["COV"]], input[["OV"]]))]
    
    # update missing (if there are no missing data use only Complete Cases
    if (!any(is.na(rawdata))){
      shinyjs::disable(id = "missing")
      shiny::updateSelectInput(
        inputId = "missing",
        selected = "Complete cases"
      )
    } else {
      shinyjs::enable(id = "missing")
      shiny::updateSelectInput(
        inputId = "missing",
        selected = "Multiple imputation"
      )
    }
  })
  
  rawdata <- shiny::reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input[["InputFile"]][["datapath"]])
    } else if (values$upload_state == 'restart') {
      return(NULL)
    }
  })
  
  # delete all files if restart in WWW is pushed
  shiny::observeEvent(input$restart, {
    files <- list.files("www", pattern = ".docx", full.names = TRUE)
    unlink(files)
    files <- list.files("www", pattern = ".tiff", full.names = TRUE)
    unlink(files)
  })
  
  # show uploaded table ---------------------------------------------------------
  output[["rawtable"]] <- DT::renderDT({
    shiny::req(rawdata())
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # update list of between-subject variables from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "BGF",
      choices = colnames(rawdata),
      selected = NA
    )
    # update list of covariates from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "COV",
      choices = colnames(rawdata),
      selected = NA
    )
    # update list of baseline variables from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "BV",
      choices = colnames(rawdata),
      selected = NA
    )
    # update list of outcome variables from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "OV",
      choices = colnames(rawdata),
      selected = NA
    )
    # update list of outcome variables from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "OVRidit",
      choices = colnames(rawdata),
      selected = NA
    )
    
    DT::datatable(
      data = rawdata,
      extensions = c('ColReorder'),
      rownames = FALSE,
      options = list(
        searching = FALSE,
        colReorder = FALSE,
        pageLength = 10,
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 'tipr'
      ), selection = "none"
    )
  }, server = FALSE)
  
  # update list of control group after loading choices for between-groups variables
  shiny::observeEvent(input[["treatmentNames"]], {
    shinyWidgets::updateVirtualSelect(
      inputId = "controlgroup",
      choices = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]],
      selected = NA
    )
  })
  
  # change tab on runTable1 click
  shiny::observeEvent(input[["runTable1"]], {
    shiny::updateTabsetPanel(session = session,
                             inputId = "tabs",
                             selected = "Table 1")
  })
  
  # change tab on runTable2 click
  shiny::observeEvent(input[["runTable2"]], {
    shiny::updateTabsetPanel(session = session,
                             inputId = "tabs",
                             selected = "Table 2")
  })
  
  # change tab on runFigure 2 click
  shiny::observeEvent(input[["runFigure2"]], {
    shiny::updateTabsetPanel(session = session,
                             inputId = "tabs",
                             selected = "Figure 2")
  })
  
  # change tab on runTable 3 click
  shiny::observeEvent(input[["runTable3"]], {
    shiny::updateTabsetPanel(session = session,
                             inputId = "tabs",
                             selected = "Table 3")
  })
  
  # Report generation -----------------------------------------------------------
  output[["SAP"]] <- shiny::renderUI({
    shiny::req(rawdata())
    results <- sap(BGF = input[["BGF"]],
                   treatmentNames = input[["treatmentNames"]],
                   controlgroup = input[["controlgroup"]],
                   endpointNames = input[["endpointNames"]],
                   endpointValues = input[["endpointValues"]],
                   alpha = input[["alpha"]],
                   showPvalue = input[["showPvalue"]],
                   effectSize = input[["effectSize"]],
                   BV = input[["BV"]],
                   OV = input[["OV"]],
                   OutcomeName = input[["OutcomeName"]],
                   COV = input[["COV"]],
                   missing = input[["missing"]],
                   MICEresamples = input[["MICEresamples"]],
                   regressionDiag = input[["regressionDiag"]],
                   OVRidit = input[["OVRidit"]],
                   OutcomeNameRidit = input[["OutcomeNameRidit"]]
    )
    
    caption <- "Statistical analysis plan"
    
    # Define text styles for caption and footnotes
    caption_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    
    # create Word doc from results dataframe
    sect_properties <- officer::prop_section(
      page_size = officer::page_size(
        orient = "portrait",
        width = 8.3,
        height = 11.7
      ),
      type = "continuous",
      page_margins = officer::page_mar()
    )
    
    # Create Word document with formatted caption
    officer::read_docx() %>%
      officer::body_set_default_section(sect_properties) %>%
      officer::body_add_fpar(officer::fpar(officer::ftext(caption, prop = caption_style))) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$software)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$table1)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$table2)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$figure2)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$table3)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$p.value)) %>%
      officer::body_add_par(officer::run_linebreak()) %>%
      officer::body_add_par(htm2txt::htm2txt(results$e.size)) %>%
      print(target = file.path(dir.name, "SAP.docx"))
    
    shiny::HTML(results$report)
  })
  
  # download downloadSAP
  output[["downloadSAP"]] <- shiny::downloadHandler(
    filename = function() {
      paste0("SAP.docx")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "SAP.docx"), to = file)
    }
  )
  
  # run table 1 on runTable1 click ------------------------------------------------
  output[["table1"]] <- DT::renderDT({
    shiny::req(rawdata())
    shiny::req(input[["BV"]])
    shiny::req(input[["BGF"]])
    shiny::req(input[["treatmentNames"]])
    shiny::req(input[["maxlevels"]])
    shiny::req(input[["alpha"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BV"]], input[["BGF"]]))]
    
    # add column based on treatment group names per use input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    results <- TABLE.1(
      dataset = rawdata,
      variables = input[["BV"]],
      bw.factor = rawdata$TREATMENT,
      max.levels = as.numeric(input[["maxlevels"]]),
      alpha = as.numeric(input[["alpha"]]),
      n.digits = 2
    )
    
    # show p-value if checkbox is true
    if (!input[["showPvalue"]]) {
      results <- results[, -ncol(results)]
    }
    
    caption <- "Table 1: Between-group descriptive analysis."
    
    # Define text styles for caption and footnotes
    caption_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    footnote_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    
    # create Word doc from results dataframe
    sect_properties <- officer::prop_section(
      page_size = officer::page_size(
        orient = "portrait",
        width = 8.3,
        height = 11.7
      ),
      type = "continuous",
      page_margins = officer::page_mar()
    )
    
    FitFlextableToPage <- function(ft, pgwidth = 8.3 - 1) {
      ft_out <- ft %>% flextable::autofit()
      ft_out <-
        flextable::width(ft_out,
                         width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    
    # Generate and format the flextable
    my_summary_to_save <-
      results %>%
      as.data.frame(check.names = FALSE, row.names = NULL) %>%
      flextable::regulartable() %>%
      FitFlextableToPage() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 12, part = "all")
    
    # Create Word document with formatted caption, table, and footnote
    table_1 <-
      officer::read_docx() %>%
      officer::body_set_default_section(sect_properties) %>%
      officer::body_add_fpar(officer::fpar(officer::ftext(caption, prop = caption_style))) %>%  # Add caption
      flextable::body_add_flextable(my_summary_to_save) %>%
      officer::body_add_fpar(officer::fpar(
        officer::ftext("Mean (SD) or count (%)", prop = footnote_style)
      )) %>%  # Add footnote
      print(target = file.path(dir.name, "Table 1.docx"))
    
    # output results
    DT::datatable(
      class = "compact",
      data = results,
      caption = caption,
      extensions = c('ColReorder'),
      rownames = TRUE,
      options = list(
        searching = FALSE,
        colReorder = FALSE,
        pageLength = nrow(results),
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:ncol(results)),
          list(visible = FALSE, targets = 0)
        )
      ), selection = "none"
    ) %>%
      DT::formatStyle(columns = 1, fontWeight = "bold") %>%
      DT::formatStyle(columns = 2, fontStyle = "italic") %>%
      DT::formatStyle(columns = 3:ncol(results), textAlign = "right") %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        backgroundColor = 'White')
  }, server = FALSE)
  
  # Download Handler
  output$downloadTable1 <- shiny::downloadHandler(
    filename = function() {
      paste0("Table 1.docx")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Table 1.docx"), to = file)
    }
  )
  
  # run table 2 on runTable2 click ------------------------------------------------
  table2 <- shiny::reactive({
    shiny::req(rawdata())
    shiny::req(input[["BGF"]])
    shiny::req(input[["OV"]])
    shiny::req(input[["treatmentNames"]])
    shiny::req(input[["controlgroup"]])
    shiny::req(input[["endpointNames"]])
    shiny::req(input[["missing"]])
    shiny::req(input[["alpha"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["COV"]], input[["OV"]]))]
    
    # add column based on treatment group names per user input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    # check if data has baseline from checkbox
    if (input[["hasBaseline"]]) {
      results <- TABLE.2a(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        control.g = input[["controlgroup"]],
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    } else {
      results <- TABLE.2b(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        control.g = input[["controlgroup"]],
        # drop 1 (no baseline)
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]][-1],
        # drop 1 (no baseline)
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]][-1]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    }
    
    return(results)
  })
  
  regression <- shiny::reactive({
    shiny::req(rawdata())
    shiny::req(input[["BGF"]])
    shiny::req(input[["OV"]])
    shiny::req(input[["treatmentNames"]])
    shiny::req(input[["controlgroup"]])
    shiny::req(input[["OutcomeName"]])
    shiny::req(input[["endpointNames"]])
    shiny::req(input[["endpointValues"]])
    shiny::req(input[["missing"]])
    shiny::req(input[["MICEresamples"]])
    shiny::req(input[["alpha"]])
    shiny::req(input[["regressionDiag"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["COV"]], input[["OV"]]))]
    
    # add column based on treatment group names per user input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    if(input[["hasBaseline"]]) {
      results <- test.model.fit(
        dataset = rawdata,
        variables = input[["OV"]],
        ov.name <- input[["OutcomeName"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        p.digits = 3,
        diagnostics = input[["regressionDiag"]]
      )
    } else {
      results <- test.model.fit(
        dataset = rawdata,
        variables = input[["OV"]],
        ov.name <- input[["OutcomeName"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        # drop 1 (no baseline)
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]][-1],
        # drop 1 (no baseline)
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]][-1]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        p.digits = 3,
        diagnostics = input[["regressionDiag"]]
      )
    }
    
    return(results)
  })
  
  # show table 2 ------------------------------------------------
  output[["table2"]] <- DT::renderDT({
    shiny::req(table2())
    shiny::req(input[["OutcomeName"]])
    
    results.mix <- table2()$mix.mod.res %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$mix.mod.res))
    
    results.wt <- table2()$wt.diff %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$wt.diff))
    
    results.bw <- table2()$bw.diff %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$bw.diff))
    
    # convert table to multiline text
    results.model <- paste(table2()$f.test.res[, 1], table2()$f.test.res[, 2], sep = " ", collapse = ", ")
    print(results.model)
    
    results <- dplyr::bind_rows(results.mix, results.wt, results.bw) %>% as.data.frame(check.names = FALSE)
    
    # last column first, then the others
    results <- results[, c(ncol(results), 1:(ncol(results) - 1))]
    
    # remove names
    rownames(results) <- rep(NULL, nrow(results))
    
    # use outcome names
    results[, 1] <- gsub("Outcome", input[["OutcomeName"]], results[, 1])
    
    if(!sjmisc::is_empty(input[["COV"]])){
      caption <- paste0(
        "Table 2: Two-way linear mixed model analysis of ",
        input[["OutcomeName"]],
        " (adjusted by ",
        paste0(input[["COV"]], collapse = ", "),
        ")."
      )
    } else {
      caption <- paste0(
        "Table 2: Two-way linear mixed model analysis of ",
        input[["OutcomeName"]],
        "."
      )
    }
    
    # Define text styles for caption and footnotes
    caption_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    footnote_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    SMD.caption <- "SMD¹ = Standardized Mean Difference calculated from marginal estimates (Cohen's d)."
    
    # create Word doc from results dataframe
    sect_properties <- officer::prop_section(
      page_size = officer::page_size(
        orient = "landscape",
        width = 8.3,
        height = 11.7
      ),
      type = "continuous",
      page_margins = officer::page_mar()
    )
    
    FitFlextableToPage <- function(ft, pgwidth = 11.7 - 1) {
      ft_out <- ft %>% flextable::autofit()
      ft_out <-
        flextable::width(ft_out,
                         width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    
    # Generate and format the flextable
    my_summary_to_save <-
      results %>%
      as.data.frame(check.names = FALSE, row.names = NULL) %>%
      flextable::regulartable() %>%
      FitFlextableToPage() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 12, part = "all")
    
    # Create Word document with formatted caption, table, and footnote
    table_2 <-
      officer::read_docx() %>%
      officer::body_set_default_section(sect_properties) %>%
      officer::body_add_fpar(officer::fpar(officer::ftext(caption, prop = caption_style))) %>%  # Add caption
      flextable::body_add_flextable(my_summary_to_save) %>%
      officer::body_add_fpar(officer::fpar(
        officer::ftext(
          results.model,
          prop = footnote_style
        )
      )) %>%  # Add footnote
      officer::body_add_fpar(officer::fpar(
        officer::ftext(
          SMD.caption,
          prop = footnote_style
        )
      )) %>%  # Add footnote
      print(target = file.path(dir.name, "Table 2.docx"))
    
    # update text textTable2
    output[["textTable2"]] <- shiny::renderUI({
      shiny::HTML(paste(
        results.model,
        SMD.caption,
        sep = "<br>"
      ))
    })
    
    # output results
    DT::datatable(
      class = "compact",
      data = results,
      caption = caption,
      extensions = c('ColReorder'),
      rownames = FALSE,
      colnames = NULL,
      options = list(
        searching = FALSE,
        colReorder = FALSE,
        pageLength = nrow(results),
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = 1:(ncol(results)-1),
            defaultContent = "-",
            targets = "_all"
          )
        )
      ), selection = "none"
    ) %>%
      DT::formatStyle(columns = 3:ncol(results), textAlign = "right") %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        backgroundColor = 'White') %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        valueColumns = 1,
        backgroundColor = DT::styleEqual("Within-subjects", 'LightGray')) %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        valueColumns = 1,
        backgroundColor = DT::styleEqual("Between-subjects", 'WhiteSmoke'))
  }, server = FALSE)
  
  # Download table 2 ------------------------------------------------
  output$downloadTable2 <- shiny::downloadHandler(
    filename = function() {
      paste0("Table 2.docx")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Table 2.docx"), to = file)
    }
  )
  
  # show figure 2 ------------------------------------------------
  output[["plot"]] <- shiny::renderPlot({
    shiny::req(rawdata())
    shiny::req(input[["BGF"]])
    shiny::req(input[["OV"]])
    shiny::req(input[["treatmentNames"]])
    shiny::req(input[["controlgroup"]])
    shiny::req(input[["endpointNames"]])
    shiny::req(input[["endpointValues"]])
    shiny::req(input[["missing"]])
    shiny::req(input[["alpha"]])
    shiny::req(input[["OutcomeName"]])
    shiny::req(input[["legendOptions"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["COV"]], input[["OV"]]))]
    
    # add column based on treatment group names per user input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    # check if data has baseline from checkbox
    if (input[["hasBaseline"]]) {
      FIGURE.2(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        xlabs = strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]],
        ylab = input[["OutcomeName"]],
        legend.opt = input[["legendOptions"]],
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    } else {
      FIGURE.2(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = rawdata[input[["COV"]]],
        bw.factor = rawdata$TREATMENT,
        # drop 1 (no baseline)
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]][-1],
        # drop 1 (no baseline)
        wt.values = as.numeric(strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]][-1]),
        missing = input[["missing"]],
        m.imputations = as.numeric(input[["MICEresamples"]]),
        xlabs = strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]],
        ylab = input[["OutcomeName"]],
        legend.opt = input[["legendOptions"]],
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    }
    
    # save plot
    dev.copy(
      tiff,
      file = file.path(dir.name, "Figure 2.tiff"),
      width = 7,
      height = 5,
      units = "in",
      res = 300
    )
    dev.off()
  })
  
  # download Figure 2 ------------------------------------------------
  output$downloadFigure2 <- shiny::downloadHandler(
    filename = function() {
      paste0("Figure 2.tiff")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Figure 2.tiff"), to = file)
    }
  )
  
  # run table 3 on runTable3 click ------------------------------------------------
  table3 <- shiny::reactive({
    shiny::req(rawdata())
    shiny::req(input[["BGF"]])
    shiny::req(input[["OVRidit"]])
    shiny::req(input[["treatmentNames"]])
    shiny::req(input[["controlgroup"]])
    shiny::req(input[["endpointNames"]])
    shiny::req(input[["alpha"]])
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    # clean variable names
    colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
    # clean NA values
    rawdata <- as.NA(rawdata)
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["OVRidit"]]))]
    
    # add column based on treatment group names per user input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    # data will never have baseline
    results <- TABLE.3(
      dataset = rawdata,
      variables = input[["OVRidit"]],
      bw.factor = rawdata$TREATMENT,
      control.g = input[["controlgroup"]],
      # drop 1
      wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]][-1],
      alpha = as.numeric(input[["alpha"]]),
      n.digits = 2
    )
    return(results)
  })
  
  # show table 3 ------------------------------------------------
  output[["table3"]] <- DT::renderDT({
    shiny::req(table3())
    shiny::req(input[["OutcomeNameRidit"]])
    
    results <- table3()$ridit.results %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table3()$ridit.results))
    
    # last column first, then the others
    results <- results[, c(ncol(results), 1:(ncol(results) - 1))]
    
    # remove names
    rownames(results) <- rep(NULL, nrow(results))
    
    caption <- paste0("Table 3: Two-way cross-table analysis (", input[["OutcomeNameRidit"]], ").")
    
    # Define text styles for caption and footnotes
    caption_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    footnote_style <- officer::fp_text(font.size = 12, font.family = "Times New Roman")
    
    # create Word doc from results dataframe
    sect_properties <- officer::prop_section(
      page_size = officer::page_size(
        orient = "landscape",
        width = 8.3,
        height = 11.7
      ),
      type = "continuous",
      page_margins = officer::page_mar()
    )
    
    FitFlextableToPage <- function(ft, pgwidth = 11.7 - 1) {
      ft_out <- ft %>% flextable::autofit()
      ft_out <-
        flextable::width(ft_out,
                         width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
      return(ft_out)
    }
    
    # Generate and format the flextable
    my_summary_to_save <-
      results %>%
      as.data.frame(check.names = FALSE, row.names = NULL) %>%
      flextable::regulartable() %>%
      FitFlextableToPage() %>%
      flextable::font(fontname = "Times New Roman", part = "all") %>%
      flextable::fontsize(size = 12, part = "all")
    
    # Create Word document with formatted caption, table, and footnote
    table_3 <-
      officer::read_docx() %>%
      officer::body_set_default_section(sect_properties) %>%
      officer::body_add_fpar(officer::fpar(officer::ftext(caption, prop = caption_style))) %>%  # Add caption
      flextable::body_add_flextable(my_summary_to_save) %>%
      officer::body_add_fpar(officer::fpar(officer::ftext("Count (%)", prop = footnote_style))) %>%  # Add footnote
      print(target = file.path(dir.name, "Table 3.docx"))
    
    # output results
    DT::datatable(
      class = "compact",
      data = results,
      caption = caption,
      extensions = c('ColReorder'),
      rownames = FALSE,
      colnames = NULL,
      options = list(
        searching = FALSE,
        colReorder = FALSE,
        pageLength = nrow(results),
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(
            className = 'dt-center',
            targets = 1:(ncol(results)-1),
            defaultContent = "-",
            targets = "_all"
          )
        )
      ), selection = "none"
    ) %>%
      DT::formatStyle(columns = 1, fontWeight = "bold") %>%
      DT::formatStyle(columns = 3:ncol(results), textAlign = "right") %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        backgroundColor = 'White') %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        valueColumns = 1,
        backgroundColor = DT::styleEqual("Between-subjects", 'LightGray')) %>%
      DT::formatStyle(
        columns = 1:ncol(results),
        valueColumns = 1,
        backgroundColor = DT::styleEqual("Level", 'WhiteSmoke'))
  }, server = FALSE)
  
  # Download table 3 ------------------------------------------------
  output$downloadTable3 <- shiny::downloadHandler(
    filename = function() {
      paste0("Table 3.docx")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Table 3.docx"), to = file)
    }
  )
  
  # show text for tableDiag1
  output[["tableDiag1"]] <- shiny::renderUI({
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Little_Test_Res)
    
    shiny::HTML(regression()$Little_Test_Res)
  })
  
  # show plot regression diagnosis
  output[["plotDiag1"]] <- shiny::renderPlot({
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Upset_Plot)
    
    regression()$Upset_Plot
  })
  
  # show plot regression diagnosis
  output[["plotDiag2"]] <- shiny::renderPlot({
    shiny::req(input[["OutcomeName"]])
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Imp_Data)
    
    plot(
      regression()$Imp_Data
    )
  })
  
  # show plot regression diagnosis
  output[["plotDiag3"]] <- shiny::renderPlot({
    shiny::req(input[["OutcomeName"]])
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Scaled_Res)
    
    plot(
      regression()$Scaled_Res
    )
  })
  
  # show text for tableDiag3
  output[["tableDiag3"]] <- shiny::renderUI({
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Shapiro_Wilk_Res)
    
    shiny::HTML(regression()$Shapiro_Wilk_Res)
  })
  
  # show plot regression diagnosis
  output[["plotDiag4"]] <- shiny::renderPlot({
    shiny::req(input[["OutcomeName"]])
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Convergence)
    
    plot(
      regression()$Convergence
    )
  })
  
  # show plot regression diagnosis
  output[["plotDiag5"]] <- shiny::renderPlot({
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$Comp_Plus_Res)
    
    plot(
      regression()$Comp_Plus_Res,
      partial.residual = list(lty = "dashed"),
      main = "Component-plus-residual plot for the interaction effect",
      xlab = "Time (endpoints)",
      ylab = input[["OutcomeName"]]
    )
  })
  
  # show table with VIF values
  output[["tableDiag6"]] <- shiny::renderTable({
    shiny::req(input[["regressionDiag"]])
    shiny::req(regression()$VIF)
    
    results <- regression()$VIF
    # if results is a matrix
    if (is.matrix(results)) {
      results <- results %>%
        as.data.frame(check.names = FALSE) %>%
        dplyr::mutate(Variables = names(regression()$VIF))
      # add new column to results data.frame with rownames
      results <- results %>%
        dplyr::mutate('Model effects' = rownames(results)) %>%
        dplyr::select('Model effects', everything())
      # reorder rows
      row_1 <- which(results[,1] == "TIME_M:GROUP_M")
      row_2 <- which(results[,1] == "GROUP_M")
      row_3 <- which(results[,1] == "TIME_M")
      other_rows <- setdiff(1:nrow(results), c(row_1, row_2, row_3))
      results <- results[c(row_1, row_2, row_3, other_rows),]
      results
    } else {
      results <- results %>%
        as.data.frame(check.names = FALSE)
      # rownames to new column
      results <- results %>%
        dplyr::mutate('Model effects' = rownames(results)) %>%
        dplyr::select('Model effects', everything())
      colnames(results) <- c("Model effects", "VIF")
      # reorder rows
      row_1 <- which(results[,1] == "TIME_M:GROUP_M")
      row_2 <- which(results[,1] == "GROUP_M")
      row_3 <- which(results[,1] == "TIME_M")
      other_rows <- setdiff(1:nrow(results), c(row_1, row_2, row_3))
      results <- results[c(row_1, row_2, row_3, other_rows),]
      results
    }
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # output references ------------------------------------------------
  output$gratrep <- shiny::renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "www/grateful-report.html",
      width = "100%",
      height = 500
    )
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
