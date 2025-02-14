# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# create file with citations
grateful::cite_packages(output = "file", out.dir = file.path(getwd(), "www"), out.format = "html", pkgs = "All")

# copy favicon folder to the www dir
R.utils::copyDirectory(from = "favicon_io", to = file.path(dir.name, "favicon_io"))

# load libraries
library(dplyr)
library(htmltools)

# source all scripts
# source("RCT-packages.R", local = TRUE) # to install packages (not required for Shiny app)
source("RCT-NA.R", local = TRUE) # to convert specific values to NA
source("RCT-Table1.R", local = TRUE) # numeric and categorical variables, descriptive analysis, between-factor
source("RCT-Table2a.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITH baseline adjustment
source("RCT-Table2b.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITHOUT baseline adjustment
source("RCT-Figure2.R", local = TRUE) # numeric variables, plot of descriptive analysis (mean and CI)
source("RCT-Table3.R", local = TRUE) #  ordinal variables, ridit analysis, ONLY within-factor
source("RCT-Missingness.R", local = TRUE) # missing data analysis

# use this code to debug
# rsconnect::showLogs()

ui <- shiny::fluidPage(
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
    shiny::fluidRow(
      shiny::column(
        8,
        list(
          fontawesome::fa("users"),
          fontawesome::fa("shuffle"),
          fontawesome::fa("users"),
          shiny::HTML("<strong>RCTapp</strong>")
        ),
        style = "text-align:left;"
      ),
      shiny::column(
        4,
        tags$a(
          id = "restart",
          class = "btn btn-primary",
          href = "javascript:history.go(0)",
          shiny::HTML('<i class="fa fa-refresh fa-1x"></i>'),
          title = "restart",
          style = "color:white; border-color:white; border-radius:100%"
        ),
        style = "text-align:right;"
      )
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
      shiny::br(),
      # input Excel file data
      shiny::fileInput(
        inputId = "InputFile",
        label = NULL,
        multiple = FALSE,
        buttonLabel = list(fontawesome::fa("file-excel"), "Upload"),
        accept = c(".xlsx"),
        width = "100%",
      ),
      DT::DTOutput(outputId = "rawtable"),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("list-check"), "SAP"),
      shiny::h4("Statistical Analysis Plan", style = "text-align:center; font-weight:bold;"),
      # split panel into 4 columns
      shiny::fluidRow(
        shiny::column(
          3,
          # add title
          shiny::h4("Study design", style = "text-align:center"),
          # add horizontal line
          shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
          # add checkbox for between-subject factors (BGF)
          shinyWidgets::virtualSelectInput(
            inputId = "BGF",
            label = "Treatment",
            choices = NULL,
            selected = NA,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = FALSE,
            width = "100%"
          ),
          # show text input to change treatment group names
          shiny::textInput(
            inputId = "treatmentNames",
            label = "Treatment labels (csv)",
            value = "Control,Treatment",
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
          shiny::br(),
          # show text for endpoint names
          shiny::textInput(
            inputId = "endpointNames",
            label = "Endpoints (csv)",
            value = "Baseline,Follow-up",
            width = "100%"
          ),
          # show text for endpoint
          shiny::textInput(
            inputId = "endpointValues",
            label = "Endpoint times (csv)",
            value = "0,3",
            width = "100%"
          ),
          shiny::br(),
          # alpha level for statistical significance
          shiny::numericInput(
            inputId = "alpha",
            label = "Alpha level",
            value = 0.05,
            min = 0.001,
            max = 0.20,
            step = 0.01,
            width = "100%"
          ),
        ),
        shiny::column(
          3,
          # add title
          shiny::actionButton(
            inputId = "runTable1",
            icon = shiny::icon("play"),
            label = "Table 1",
            style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
          ),
          # add horizontal line
          shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
          # add checkbox for baseline variables (BV)
          shinyWidgets::virtualSelectInput(
            inputId = "BV",
            label = "Baseline variables",
            choices = NULL,
            selected = NA,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE,
            width = "100%"
          ),
          # show maxlevels for between-subject factors
          shiny::numericInput(
            inputId = "maxlevels",
            label = "Max levels (categorical variables)",
            value = 5,
            min = 1,
            max = 10,
            step = 1,
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
        shiny::column(
          3,
          # add title
          shiny::fluidRow(
            shiny::column(
              6,
              # add button to run analysis
              shiny::actionButton(
                inputId = "runTable2",
                icon = shiny::icon("play"),
                label = "Table 2",
                style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
              ),
            ),
            shiny::column(
              6,
              # add button to run analysis
              shiny::actionButton(
                inputId = "runFigure2",
                icon = shiny::icon("play"),
                label = "Figure 2",
                style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
              ),
            ),
          ),
          # add horizontal line
          shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
          # add checkbox for outcome variables (OV)
          shinyWidgets::virtualSelectInput(
            inputId = "OV",
            label = "Outcome variables",
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
          # show text input to change outcome name
          shiny::textInput(
            inputId = "OutcomeName",
            label = "Outcome label",
            value = "Outcome",
            width = "100%"
          ),
          # add checkbox for covariates (CV)
          shinyWidgets::virtualSelectInput(
            inputId = "CV",
            label = "Covariates",
            choices = NULL,
            selected = NA,
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = TRUE,
            width = "100%"
          ),
          # show options for missing data
          shinyWidgets::virtualSelectInput(
            inputId = "missing",
            label = "Missing data",
            choices = c("Complete cases", "Mean imputation", "Multiple imputation"),
            selected = "Complete cases",
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = FALSE,
            width = "100%"
          ),
          # number of resamples
          shiny::numericInput(
            inputId = "MICEresamples",
            label = "Resamples for multiple imputation",
            value = 50,
            min = 1,
            max = 100,
            step = 1,
            width = "100%"
          ),
          # show checkbox for show/hid legend
          shiny::checkboxInput(
            inputId = "hasLegend",
            label = "Show legend",
            value = TRUE,
            width = "100%"
          ),
          # options for legend
          shinyWidgets::virtualSelectInput(
            inputId = "legendOptions",
            label = "Legend position",
            choices = c("none", "top", "topleft", "topright", "bottom", "bottomleft", "bottomright", "left", "right", "center"),
            selected = "none",
            showValueAsTags = TRUE,
            search = TRUE,
            multiple = FALSE,
            width = "100%"
          ),
        ),
        # add column for buttons
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
      ),
    ),
    # tab for table 1 of results
    shiny::tabPanel(
      title = "Table 1",
      icon = fontawesome::fa("table"),
      shiny::br(),
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
    ),
    # tab for table 2 of results
    shiny::tabPanel(
      title = "Table 2",
      icon = fontawesome::fa("table"),
      shiny::br(),
      # show table of results
      DT::dataTableOutput("datatable2"),
      # print text:"SMD¹ = Standardized Mean Difference calculated from marginal estimates (Cohen's d)."
      shiny::tags$p("SMD¹ = Standardized Mean Difference calculated from marginal estimates (Cohen's d)."),
      shiny::br(),
      # download Word format
      shiny::downloadButton(
        outputId = "downloadTable2",
        label = "Download Table 2 (.DOCX)",
        style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
      ),
      shiny::br(),
    ),
    # tab for plot of results
    shiny::tabPanel(
      title = "Figure 2",
      icon = fontawesome::fa("chart-line"),
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
    ),
    # tab for table 3 of results
    shiny::tabPanel(
      title = "Table 3",
      icon = fontawesome::fa("table"),
      # show table of results
      DT::dataTableOutput("table3"),
    ),
    # tab for regression diagnosis
    shiny::tabPanel(
      title = "Assumptions",
      icon = fontawesome::fa("stethoscope"),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("circle-info")),
      shiny::br(),
      shiny::HTML("<b> Tutorial</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<p>1. In <b>Data</b>, Use the <b>Upload</b> button to load data in .XLSX format.</p>\
         <p>2. Click <b>SAP</b> to configure the <b>statistial analysis plan</b>:</p>\
         <p>2.1 <b>Study design</b></p>\
         <ul>\
         <li><i>Treatment</i></li>\
         <li><i>Control group</i></li>\
         <li><i>Endpoints</i></li>\
         <li><i>Type-I error (alpha value)</i></li>\
         </ul>\
         <p>2.2. <b>Table 1</b></p>\
         <ul>\
         <li><i>Baseline</i></li>\
         <li><i>Max levels for categorical variables</i></li>\
         <li><i>Show/Hide p-values</i></li>\
         </ul>\
         <p>2.3. <b>Table 2 & Figure 2</b></p>\
         <ul>\
         <li><i>Outcomes</i></li>\
         <li><i>Covariates</i></li>\
         <li><i>Missing data handling method</i></li>\
         <li><i>Resamples for multiple imputation, if any</i></li>\
         <li><i>Position of plot legend, if any</i></li>\
         </ul>\
         <p>2.4. <b>Table 3:</b></p>\
         <ul>\
         </ul>\
         <p>3. Click <i class='fa fa-refresh fa-1x'></i> <b>Restart</b> before running new analisys."
      ),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("book-open")),
      shiny::br(),
      shiny::HTML("<b> Session info</b>"),
      shiny::br(),
      shiny::br(),
      # session info text output
      shiny::htmlOutput("gratrep")
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("file-lines")),
      shiny::br(),
      shiny::HTML("<b> Publications</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "Castro, J., Correia, L., Donato, B. de S., Arruda, B., Agulhari, F., Pellegrini, M. J., Belache, F. T. C., de Souza, C. P., Fernandez, J., Nogueira, L. A. C., Reis, F. J. J., Ferreira, A. de S., & Meziat-Filho, N. (2022). Cognitive functional therapy compared with core exercise and manual therapy in patients with chronic low back pain: randomised controlled trial. In Pain (Vol. 163, Issue 12, pp. 2430–2437). Ovid Technologies (Wolters Kluwer Health). <a href=\"https://doi.org/10.1097/j.pain.0000000000002644 \">https://doi.org/10.1097/j.pain.0000000000002644</a>"
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "Avila, L., da Silva, M. D., Neves, M. L., Abreu, A. R., Fiuza, C. R., Fukusawa, L., de Sá Ferreira, A., & Meziat-Filho, N. (2023). Effectiveness of Cognitive Functional Therapy Versus Core Exercises and Manual Therapy in Patients With Chronic Low Back Pain After Spinal Surgery: Randomized Controlled Trial. In Physical Therapy (Vol. 104, Issue 1). Oxford University Press (OUP). <a https://doi.org/10.1093/ptj/pzad105 \">https://doi.org/10.1093/ptj/pzad105</a>"
      ),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("people-group")),
      shiny::br(),
      shiny::HTML("<b> Authors</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<a href=\"mailto:arthurde@souunisuam.com.br\">Arthur de Sá Ferreira, DSc</a>"
      ),
      shiny::HTML("<b> (Developer)</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<a href=\"mailto:ney.filho@souunisuam.com.br\">Ney Meziat Filho, DSc</a>"
      ),
      shiny::HTML("; "),
      shiny::HTML(
        "<a href=\"mailto:fabianaterracunha@gmail.com\">Fabiana Terra Cunha, DSc</a>"
      ),
      shiny::HTML("; "),
      shiny::HTML(
        "<a href=\"mailto:jessicafmg@gmail.com\">Jessica Fernandez, DSc</a>"
      ),
      shiny::HTML("; "),
      shiny::HTML(
        "<a href=\"mailto:julia.d.castro@hotmail.com\">Julia Castro, DSc</a>"
      ),
      shiny::HTML("<b> (Contributors)</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML(
        "<a href=\"https://www.unisuam.edu.br/programa-pos-graduacao-ciencias-da-reabilitacao\">PPGCR</a> | Programa de Pós-graduação em Ciências da Reabilitação, Centro Universitário Augusto Motta, Rio de Janeiro, RJ, Brazil"
      ),
      shiny::HTML("<b> (Affiliation)</b>"),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>License</b>"),
      shiny::HTML(
        "This work is licensed under an <a rel=\"license\" data-spdx=\"Apache-2.0\" href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache License 2.0</a>."
      ),
      shiny::br(),
      shiny::br(),
      shiny::HTML("<b>Cite as</b>"),
      shiny::HTML(
        "Ferreira, A. de S., & Meziat Filho, N. (2024). RCTapp Randomized Clinical Trial (1.0.0). Zenodo. <a href=\"https://doi.org/10.5281/zenodo.13848816\" target=\"_blank\">https://doi.org/10.5281/zenodo.13848816</a>"
      ),
    ),
  ),
)

# Define server script
server <- function(input, output, session) {
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
  
  # enable legend options if hasLegeng is checked
  shiny::observeEvent(input$hasLegend, {
    if (input$hasLegend == TRUE) {
      shinyjs::enable("legendOptions")
    } else {
      shinyjs::disable("legendOptions")
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
      inputId = "CV",
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
    
    DT::datatable(
      data = rawdata,
      extensions = c('ColReorder'),
      rownames = FALSE,
      options = list(
        searching = FALSE,
        colReorder = TRUE,
        pageLength = 10,
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 'tipr'
      )
    )
  }, server = FALSE)
  
  # show selected BGF in BGFtable
  output[["BGFtable"]] <- shiny::renderTable({
    shiny::req(input[["BGF"]])
    data.frame("Treatment" = input[["BGF"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # show selected control group in CGtable
  output[["CGtable"]] <- shiny::renderTable({
    shiny::req(input[["controlgroup"]])
    data.frame("Control group" = input[["controlgroup"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # show selected CV in CVtable
  output[["CVtable"]] <- shiny::renderTable({
    shiny::req(input[["CV"]])
    data.frame("Covariates" = input[["CV"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # show selected OV in OVtable
  output[["OVtable"]] <- shiny::renderTable({
    shiny::req(input[["OV"]])
    data.frame("Outcome data" = input[["OV"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # show selected endpoints
  output[["Endtable"]] <- shiny::renderTable({
    shiny::req(input[["endpointNames"]])
    data.frame("Endpoints" = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
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
  
  # enable plot legend when checked
  
  # run table 1 on runTable1 click ------------------------------------------------
  output[["table1"]] <- DT::renderDT({
    shiny::req(rawdata())
    shiny::req(input[["BV"]])
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
        flextable::width(ft_out, width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
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
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(caption, prop = caption_style)
        )
      ) %>%  # Add caption
      flextable::body_add_flextable(my_summary_to_save) %>%
      # define sec properties
      
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("Mean (SD) or count (%)", prop = footnote_style)
        )
      ) %>%  # Add footnote
      print(target = file.path(dir.name, "Table 1.docx"))
    
    # output results
    DT::datatable(
      data = results,
      caption = caption,
      extensions = c('ColReorder'),
      rownames = TRUE,
      options = list(
        searching = FALSE,
        colReorder = TRUE,
        pageLength = nrow(results),
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(
          list(className = 'dt-center', targets = 1:ncol(results)),
          list(visible = FALSE, targets = 0)
        )
      )
    ) %>%
      DT::formatStyle(columns = 1, fontWeight = "bold") %>%
      DT::formatStyle(columns = 2, fontStyle = "italic") %>%
      DT::formatStyle(columns = 3:ncol(results), textAlign = "right")
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
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["CV"]], input[["OV"]]))]
    
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
    if(input[["hasBaseline"]]){
      results <- TABLE.2a(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = input[["CV"]],
        bw.factor = rawdata$TREATMENT,
        control.g = input[["controlgroup"]],
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
        missing = tolower(gsub(" ", ".", input[["missing"]])),
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    } else {
      results <- TABLE.2b(
        dataset = rawdata,
        variables = input[["OV"]],
        covariate = input[["CV"]],
        bw.factor = rawdata$TREATMENT,
        control.g = input[["controlgroup"]],
        # drop 1 
        wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]][-1],
        missing = tolower(gsub(" ", ".", input[["missing"]])),
        m.imputations = as.numeric(input[["MICEresamples"]]),
        alpha = as.numeric(input[["alpha"]]),
        n.digits = 2
      )
    }
    return(results)
  })
  
  # show table 2
  output[["datatable2"]] <- DT::renderDT({
    shiny::req(table2())
    
    results.mix <- table2()$mix.mod.res %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$mix.mod.res))
    
    results.wt <- table2()$wt.diff %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$wt.diff))
    
    results.bw <- table2()$bw.diff %>%
      as.data.frame(check.names = FALSE) %>%
      dplyr::mutate(Variables = rownames(table2()$bw.diff))
    
    results <- dplyr::bind_rows(results.mix, results.wt, results.bw) %>% as.data.frame(check.names = FALSE)
    
    # last column first, then the others
    results <- results[, c(ncol(results), 1:(ncol(results) - 1))]
    
    # remove names
    rownames(results) <- rep(NULL, nrow(results))
    
    # use outcome names
    results[, 1] <- gsub("Outcome", input[["OutcomeName"]], results[, 1])
    
    caption <- "Table 2: Two-way linear mixed model analysis."
    
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
        flextable::width(ft_out, width = dim(ft_out)$widths * pgwidth / (flextable::flextable_dim(ft_out)$widths))
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
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext(caption, prop = caption_style)
        )
      ) %>%  # Add caption
      flextable::body_add_flextable(my_summary_to_save) %>%
      # define sec properties
      
      officer::body_add_fpar(
        officer::fpar(
          officer::ftext("SMD¹ = Standardized Mean Difference calculated from marginal estimates (Cohen's d).", prop = footnote_style)
        )
      ) %>%  # Add footnote
      print(target = file.path(dir.name, "Table 2.docx"))
    
    # output results
    DT::datatable(
      data = results,
      caption = caption,
      extensions = c('ColReorder'),
      rownames = FALSE,
      colnames = NULL,
      options = list(
        searching = FALSE,
        colReorder = TRUE,
        pageLength = nrow(results),
        width = "100%",
        fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}'),
        scrollX = TRUE,
        dom = 't',
        columnDefs = list(list(
          className = 'dt-center', targets = 1:ncol(results),
          defaultContent = "-",
          targets = "_all"
        ))
      )
    ) %>%
      DT::formatStyle(columns = 1, fontWeight = "bold") %>%
      DT::formatStyle(columns = 3:ncol(results), textAlign = "right")
  }, server = FALSE)
  
  # Download Handler
  output$downloadTable2 <- shiny::downloadHandler(
    filename = function() {
      paste0("Table 2.docx")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Table 2.docx"), to = file)
    }
  )
  
  # run Figure 2
  output[["plot"]] <- shiny::renderPlot({
    shiny::req(rawdata())
    shiny::req(input[["OV"]])
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
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BGF"]], input[["OV"]]))]
    
    # add column based on treatment group names per user input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(trimws(input[["treatmentNames"]], which = "both"), ",")[[1]]
        ))
    }
    
    FIGURE.2(
      dataset = rawdata,
      variables = input[["OV"]],
      covariate = input[["CV"]],
      bw.factor = rawdata$TREATMENT,
      wt.labels = strsplit(trimws(input[["endpointNames"]], which = "both"), ",")[[1]],
      missing = tolower(gsub(" ", ".", input[["missing"]])),
      m.imputations = as.numeric(input[["MICEresamples"]]),
      xlabs = strsplit(trimws(input[["endpointValues"]], which = "both"), ",")[[1]],
      ylab = input[["OutcomeName"]],
      legend.opt = input[["legendOptions"]],
      alpha = as.numeric(input[["alpha"]])
    )
    
    # save plot
    dev.copy(tiff, file = file.path(dir.name, "Figure 2.tiff"), width = 7, height = 5, units = "in", res = 300)
    dev.off()
  })
  
  # download Figure 2
  output$downloadFigure2 <- shiny::downloadHandler(
    filename = function() {
      paste0("Figure 2.tiff")
    },
    content = function(file) {
      file.copy(from = file.path(dir.name, "Figure 2.tiff"), to = file)
    }
  )
  
  # run table 3
  
  # output references
  output$gratrep <- shiny::renderUI({
    tags$iframe(seamless="seamless", 
                src = "www/grateful-report.html",
                width = "100%",
                height = 500)
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
