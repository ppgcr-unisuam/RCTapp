# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)) {
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# copy favicon folder to the www dir
R.utils::copyDirectory(from = "favicon_io", to = file.path(dir.name, "favicon_io"))

# load libraries
library(dplyr)
library(htmltools)

# source all scripts
# source("RCT-packages.R", local = TRUE) # to install packages
source("RCT-Figure1.R", local = TRUE) # numeric variables, plot of descriptive analysis (mean and CI)
source("RCT-Table1.R", local = TRUE) # numeric and ordinal variables, descriptive analysis, between-factor
source("RCT-Table2a.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITH baseline adjustment
source("RCT-Table2b.R", local = TRUE) # numeric variables, linear mixed model analysis, between- AND within-factor WITHOUT baseline adjustment
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
          shiny::HTML("<strong>RCTapp</strong>"),
          shiny::br(),
          "Randomized Clinical Trial"
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
      title = list(fontawesome::fa("right-to-bracket"), "Input"),
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
      shiny::h4("Statistical Analysis Plan", style = "text-align:center"),
      shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
      # split panel into 3 columns
      shiny::fluidRow(
        shiny::column(
          3,
          # add title
          shiny::h4("Table 1", style = "text-align:center"),
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
          # show maxlevels for between-subject factors
          shiny::numericInput(
            inputId = "maxlevels",
            label = "Max levels",
            value = 5,
            min = 1,
            max = 10,
            step = 1,
            width = "100%"
          ),
        ),
        shiny::column(
          6,
          # add title
          shiny::h4("Table 2", style = "text-align:center"),
          # add horizontal line
          shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
          # split into two columns
          shiny::fluidRow(
            shiny::column(
              6,
              # add checkbox for subject variables (ID)
              shinyWidgets::virtualSelectInput(
                inputId = "ID",
                label = "Subject ID",
                choices = NULL,
                selected = NA,
                showValueAsTags = TRUE,
                search = TRUE,
                multiple = FALSE,
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
              # add checkbox for outcome variables (OV)
              shinyWidgets::virtualSelectInput(
                inputId = "OV",
                label = "Outcome data",
                choices = NULL,
                selected = NA,
                showValueAsTags = TRUE,
                search = TRUE,
                multiple = TRUE,
                width = "100%"
              ),
              # show options for baseline data
              shinyWidgets::virtualSelectInput(
                inputId = "baselineVar",
                label = "Baseline data",
                choices = NULL,
                selected = NA,
                showValueAsTags = TRUE,
                search = TRUE,
                multiple = FALSE,
                width = "100%"
              ),
              # show options for missing data
              shinyWidgets::virtualSelectInput(
                inputId = "missing",
                label = "Missing data",
                choices = c(
                  "None",
                  "Complete cases",
                  "Mean imputation",
                  "Multiple imputation"
                ),
                selected = NA,
                showValueAsTags = TRUE,
                search = TRUE,
                multiple = FALSE,
                width = "100%"
              ),
            ),
            shiny::column(
              6,
              # show text input to change outcome name
              shiny::textInput(
                inputId = "OutcomeNames",
                label = "Outcome",
                value = "Outcome",
                width = "100%"
              ),
              # show text input to change treatment group names
              shiny::textInput(
                inputId = "treatmentNames",
                label = "Treatments (csv)",
                value = "Control, Treatment",
                width = "100%"
              ),
              # show text for endpoint names
              shiny::textInput(
                inputId = "endpointNames",
                label = "Endpoints (csv)",
                value = "Baseline, Follow-up",
                width = "100%"
              ),
              # show text for endpoint
              shiny::textInput(
                inputId = "endpointValues",
                label = "Endpoint times (csv)",
                value = "0, 3",
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
          ),
        ),
        # add column for buttons
        shiny::column(
          3,
          # add title
          shiny::h4("Run", style = "text-align:center"),
          # add horizontal line
          shiny::tags$hr(style = "border-color: #2C3E50; border-width: 2px;"),
          # add button to run analysis
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "runTable1",
            icon = shiny::icon("play"),
            label = "Table 1",
            style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
          ),
          shiny::br(),
          shiny::br(),
          # add button to run analysis
          shiny::actionButton(
            inputId = "runTable2",
            icon = shiny::icon("play"),
            label = "Table 2",
            style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
          ),
          shiny::br(),
          shiny::br(),
          # add button to run analysis
          shiny::actionButton(
            inputId = "runTable3",
            icon = shiny::icon("play"),
            label = "Table 3",
            style = "color: #FFFFFF; background-color: #2C3E50; border-color: #2C3E50; width: 100%;"
          ),
          shiny::br(),
          shiny::br(),
          # add button to run analysis
          shiny::actionButton(
            inputId = "runPlot",
            icon = shiny::icon("play"),
            label = "Plot",
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
    ),
    # tab for table 2 of results
    shiny::tabPanel(
      title = "Table 2",
      icon = fontawesome::fa("table"),
      shiny::br(),
      # show table of results
      DT::dataTableOutput("table2"),
    ),
    # tab for table 3 of results
    shiny::tabPanel(
      title = "Table 3",
      icon = fontawesome::fa("table"),
      # show table of results
      DT::dataTableOutput("table3"),
    ),
    # tab for plot of results
    shiny::tabPanel(title = list(fontawesome::fa("chart-line"), "Plot"), # show plot of results
                    shiny::plotOutput("plot"), ),
    shiny::tabPanel(
      title = list(fontawesome::fa("circle-info")),
      shiny::br(),
      shiny::HTML(
        "<p>1. Use the <b>Upload</b> button to load an Excel file (.xlsx).</p>\
         <p>2. Click <b>Plan</b> to configure the statistial analysis.</p>\
         <p>2.1 Select the following variables from the dataset:</p>\
         <ul>\
         <li><i>Baseline variables</i></li>\
         <li><i>Subject ID</i></li>\
         <li><i>Treatment</i></li>\
         <li><i>Control group</i></li>\
         <li><i>Covariates</i></li>\
         <li><i>Outcome data</i></li>\
         <li><i>Baseline data</i></li>\
         <li><i>Missing data handling method</i></li>\
         </ul>\
         <p>2.2. Type specific labels for the analysis:</p>\
         <ul>\
         <li><i>Treatments</i></li>\
         <li><i>Endpoints</i></li>\
         <li><i>Endopont values</i></li>\
         <li><i>Resamples</i></li>\
         <li><i>Alpha level</i></li>\
         </ul>\
         <p>3. Click <b>Preview</b> to visualize the data.</p>\
         <p>3.1. Click <b>Table 1</b> to visualize the results of the Table 1.</p>\
         <p>3.2. Click <b>Table 2</b> to visualize the results of the Table 2.</p>\
         <p>3.3. Click <b>Table 3</b> to visualize the results of the Table 3.</p>\
         <p>3.4. Click <b>Plot</b> to visualize the results of the Plot.</p>\
         <p>4. Click <b>restart</b> icon before running new analisys.",
      ),
    ),
    shiny::tabPanel(
      title = list(fontawesome::fa("people-group")),
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
      shiny::HTML("<b>Cite as</b>"),
      shiny::HTML(
        "Ferreira, A. de S., & Meziat Filho, N. (2024). RCTapp Randomized Clinical Trial (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.13848816"
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
  
  rawdata <- shiny::reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input[["InputFile"]][["datapath"]])
    } else if (values$upload_state == 'restart') {
      return(NULL)
    }
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
    
    # update list of subject variables from rawdata header
    shinyWidgets::updateVirtualSelect(
      inputId = "ID",
      choices = colnames(rawdata),
      selected = NA
    )
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
      rownames = FALSE,
      options = list(
        dom = 'tipr',
        searching = FALSE,
        pageLength = 10,
        width = "100%",
        scrollX = TRUE
      )
    )
  })
  
  # show selected ID in IDtable
  output[["IDtable"]] <- shiny::renderTable({
    shiny::req(input[["ID"]])
    data.frame("Subject ID" = input[["ID"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
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
  
  # show selected baseline variable
  output[["baselineVar"]] <- shiny::renderTable({
    shiny::req(input[["baselineVar"]])
    data.frame("Baseline" = input[["baselineVar"]], check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # show selected endpoints
  output[["Endtable"]] <- shiny::renderTable({
    shiny::req(input[["endpointNames"]])
    data.frame("Endpoints" = strsplit(input[["endpointNames"]], ", ")[[1]],
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = TRUE)
  
  # update list of control group after loading choices for between-groups variables
  shiny::observeEvent(input[["BGF"]], {
    shinyWidgets::updateVirtualSelect(
      inputId = "controlgroup",
      choices = strsplit(input[["treatmentNames"]], ", ")[[1]],
      selected = NA
    )
  })
  
  # update list of baseline data after loading choices for OV variables
  shiny::observeEvent(input[["OV"]], {
    shinyWidgets::updateVirtualSelect(inputId = "baselineVar",
                                      choices = input[["OV"]],
                                      selected = NA)
  })
  
  # change tab on runTable1 click
  shiny::observeEvent(input[["runTable1"]], {
    shiny::updateTabsetPanel(session = session,
                             inputId = "tabs",
                             selected = "Table 1")
  })
  
  # run table 1 on runTable1 click ------------------------------------------------
  output[["table1"]] <- DT::renderDT({
    shiny::req(rawdata())
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    
    # select columns from checked variables
    rawdata <- rawdata[, unique(c(input[["BV"]], input[["BGF"]]))]
    
    # add column based on treatment group names per use input
    if (!is.null(input[["BGF"]])) {
      rawdata <- rawdata %>%
        dplyr::mutate(TREATMENT = factor(
          x = rawdata[[input[["BGF"]]]],
          levels = unique(rawdata[[input[["BGF"]]]]),
          labels = strsplit(input[["treatmentNames"]], ", ")[[1]]
        ))
    }
    
    results <- TABLE.1(
      dataset = rawdata,
      variables = input[["BV"]],
      bw.factor = rawdata$TREATMENT,
      max.levels = as.numeric(input[["maxlevels"]]),
      alpha = input[["alpha"]],
      n.digits = 2
    )
    
    title <- "Table 1"
    
    # output results
    DT::datatable(
      data = results,
      caption = "Table 1: Between-group descriptive analysis [mean (SD) or count (%)].",
      extensions = c('Buttons', 'ColReorder', 'FixedHeader'),
      options = list(
        searching = FALSE,
        colReorder = TRUE,
        fixedHeader = TRUE,
        pageLength = nrow(results),
        width = "100%",
        scrollX = TRUE,
        dom = 'tB',
        buttons = list(
          list(
            extend = "copy",
            text = "Copiar",
            filename = title
          ),
          list(
            extend = "csv",
            text = "CSV",
            filename = title
          ),
          list(
            extend = "pdf",
            text = "PDF",
            title = title,
            filename = title
          )
        )
      )
    )
  })
  
  
  
  # run table 2
  # output[["table2"]] <- DT::renderDT({
  #   shiny::req(rawdata())
  #
  #   # read file
  #   rawdata <- readxl::read_xlsx(rawdata())
  #   # remove empty columns
  #   rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
  #   # remove empty rows
  #   rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
  #
  #   # select columns from checked variables
  #   rawdata <- rawdata[, unique(c(input[["ID"]], input[["BGF"]], input[["CV"]], input[["OV"]]))]
  #
  #   # add column based on treatment group names per use input
  #   if (!is.null(input[["BGF"]])) {
  #     rawdata <- rawdata %>%
  #       dplyr::mutate(TREATMENT = factor(
  #         x = rawdata[[input[["BGF"]]]],
  #         levels = unique(rawdata[[input[["BGF"]]]]),
  #         labels = strsplit(input[["treatmentNames"]], ", ")[[1]]
  #       ))
  #   }
  #
  #   TABLE.2a(
  #     dataset = rawdata,
  #     variables = input[["OV"]],
  #     covariate = as.data.frame(rawdata[, input[["CV"]]], check.names = FALSE),
  #     bw.factor = rawdata$TREATMENT,
  #     control.g = input[["controlgroup"]],
  #     wt.labels = input[["endpointNames"]],
  #     missing = tolower(gsub(" ", ".", input[["missing"]])),
  #     m.imputations = input[["MICEresamples"]],
  #     alpha = input[["alpha"]],
  #     n.digits = 2
  #   )
  # })
  
  # # download MP4 file generated by 8 output files ---------------------------------------------------------
  # output[["downloadMP4"]] <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       paste0("outputvideo.mp4")
  #     },
  #     content = function(file) {
  #       file.copy(from = file.path(dir.name, "outputvideo.mp4"),
  #                 to = file)
  #     }
  #   )
  #
  # # download TRAJECTORY CSV files ---------------------------------------------------------
  # output[["downloadPATH"]] <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       paste0("trajectory_measured.csv")
  #     },
  #     content = function(file) {
  #       file.copy(from = file.path(dir.name, "CSV", "trajectory_measured.csv"),
  #                 to = file)
  #     }
  #   )
  #
  # # restart button ---------------------------------------------------------
  # shinyjs::onclick("restart", {
  #
  #   # clean InputFile
  #   shinyjs::reset("InputFile")
  #
  #   # delete files
  #   unlink(
  #     list.files(
  #       path = dir.name,
  #       recursive = TRUE,
  #       include.dirs = TRUE,
  #       full.names = TRUE,
  #       pattern = "mp4"
  #     )
  #   )
  #   unlink(
  #     list.files(
  #       path = dir.name,
  #       recursive = TRUE,
  #       include.dirs = TRUE,
  #       full.names = TRUE,
  #       pattern = "png"
  #     )
  #   )
  #   unlink(
  #     list.files(
  #       path = dir.name,
  #       recursive = TRUE,
  #       include.dirs = TRUE,
  #       full.names = TRUE,
  #       pattern = "csv"
  #     )
  #   )
  # })
  # shiny::outputOptions(output, "downloadMP4", suspendWhenHidden = FALSE)
  # shiny::outputOptions(output, "downloadPATH", suspendWhenHidden = FALSE)
  # shiny::outputOptions(output, "downloadDISPL", suspendWhenHidden = FALSE)
  # shiny::outputOptions(output, "downloadCC", suspendWhenHidden = FALSE)
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
