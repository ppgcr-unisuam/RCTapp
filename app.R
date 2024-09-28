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
source("RCT-Figure1.R", local = TRUE)
source("RCT-Missingness.R", local = TRUE)
source("RCT-Table1.R", local = TRUE)
source("RCT-Table2a.R", local = TRUE)
source("RCT-Table2b.R", local = TRUE)
source("RCT-Table3.R", local = TRUE)
# source("TRIAL.R", local = TRUE)

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
      title = list(fontawesome::fa("list-check"), "Plan"),
      shiny::br(),
      # split panel into 4 columns
      shiny::fluidRow(
        shiny::column(
          3,
          # add checkbox for subject variables (ID)
          shinyWidgets::pickerInput(
            inputId = "ID",
            label = "Subject ID",
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 0"
            ),
            multiple = TRUE,
            width = "100%"
          ),
          # show dataframe of selected ID with renderTable
          shiny::tableOutput(outputId = "IDtable"),
          # add checkbox for between-subject factors (BGF)
          shinyWidgets::pickerInput(
            inputId = "BGF",
            label = "Between-subject factors",
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 0"
            ),
            multiple = TRUE,
            width = "100%"
          ),
          # show dataframe of selected BGF with renderTable
          shiny::tableOutput(outputId = "BGFtable"),
          # add checkbox for covariates (CV)
          shinyWidgets::pickerInput(
            inputId = "CV",
            label = "Covariates",
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 0"
            ),
            multiple = TRUE,
            width = "100%"
          ),
          # show dataframe of selected CV with renderTable
          shiny::tableOutput(outputId = "CVtable"),
          # add checkbox for outcome variables (OV)
          shinyWidgets::pickerInput(
            inputId = "OV",
            label = "Outcome variables",
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              actionsBox = TRUE,
              size = 10,
              selectedTextFormat = "count > 0"
            ),
            multiple = TRUE,
            width = "100%"
          ),
          # show dataframe of selected OV with renderTable
          shiny::tableOutput(outputId = "OVtable"),
        ),
        shiny::column(
          3,
          # show options for missing data
          shiny::radioButtons(
            inputId = "missing",
            label = "Missing data",
            choices = c("None", "Complete cases", "Mean imputation", "Multiple imputation"),
            selected = "none",
            inline = FALSE,
            width = "100%"
          ),
          # number of resamples
          shiny::numericInput(
            inputId = "M",
            label = "Resamples",
            value = 50,
            min = 1,
            max = 100,
            step = 1,
            width = "100%"
          ),
        ),
        shiny::column(
          6,
          # show dataframe of selected ID with renderTable
          DT::DTOutput(outputId = "datatable"),
          shiny::br(),
        ),
        # show button Analyze
        shiny::actionButton(
          inputId = "buttAnalyze",
          label = "Analyze",
          class = "btn-primary",
          style = "width:100%; border-color:white; border-radius: 10px",
          icon("play")
        ),
      ),
    ),
    # tab for table 1 of results
    shiny::tabPanel(
      title = list(fontawesome::fa("table"), "Table 1"),
      shiny::br(),
      # show table of results
      DT::dataTableOutput("table"),
    ),
    # tab for table 2a of results
    shiny::tabPanel(
      title = list(fontawesome::fa("table"), "Table 2a"),
      shiny::br(),
      # show table of results
      DT::dataTableOutput("table2a"),
    ),
    # tab for table 3 of results
    shiny::tabPanel(
      title = list(fontawesome::fa("table"), "Table 3"),
      shiny::br(),
      # show table of results
      DT::dataTableOutput("table3"),
    ),
    # tab for plot of results
    shiny::tabPanel(
      title = list(fontawesome::fa("chart-line"), "Plot"),
      shiny::br(),
      # show plot of results
      shiny::plotOutput("plot"),
    ),

    # shiny::tabPanel(
    #   title = list(fontawesome::fa("arrow-trend-up"), "Track"),
    #   shiny::br(),
    #   shiny::tabsetPanel(
    #     id = "tabTrack",
    #     type = "tabs",
    #     shiny::tabPanel(
    #       title = list(fontawesome::fa("sliders"), "Setup"),
    #       shiny::br(),
    #       # split two columns
    #       shiny::fluidRow(
    #         shiny::column(
    #           4,
    #           shiny::sliderInput(
    #             inputId = "KernelSizeTrack",
    #             label = "Object (px)",
    #             min = 1,
    #             max = 201,
    #             value = 50,
    #             step = 2,
    #             ticks = FALSE,
    #             width = "100%",
    #           ),
    #           shiny::sliderInput(
    #             inputId = "Overlap",
    #             label = "ROI (%)",
    #             min = 0,
    #             max = 100,
    #             value = 50,
    #             step = 5,
    #             ticks = FALSE,
    #             width = "100%",
    #           ),
    #           shiny::radioButtons(
    #             inputId = "FilterType",
    #             label = "Filter",
    #             choices = c("none", "mean", "median"),
    #             selected = "none",
    #             inline = TRUE,
    #             width = "100%"
    #           ),
    #           shiny::sliderInput(
    #             inputId = "FilterSize",
    #             label = "Size (px)",
    #             min = 1,
    #             max = 11,
    #             value = 1,
    #             step = 2,
    #             ticks = FALSE,
    #             width = "100%",
    #           ),
    #           shiny::sliderInput(
    #             inputId = "Jump",
    #             label = "Jump (frames)",
    #             min = 0,
    #             max = 5,
    #             value = 0,
    #             ticks = FALSE,
    #             width = "100%",
    #           ),
    #           shiny::br(),
    #           shiny::actionButton(
    #             inputId = "buttAnalyze",
    #             label = "Analyze",
    #             class = "btn-primary",
    #             style = "width:100%; border-color:white; border-radius: 10px",
    #             icon("play")
    #           ),
    #           shiny::br(),
    #           align = "center"
    #         ),
    #         shiny::column(
    #           8,
    #           shiny::tabPanel(
    #             title = list(fontawesome::fa("crop"), "ROI"),
    #             shiny::br(),
    #             shiny::plotOutput(
    #               outputId = "plotROI",
    #               width = "auto",
    #               click = "roi_click"
    #             ),
    #             align = "center"
    #           ),
    #         ),
    #       ),
    #     ),
    #     shiny::tabPanel(
    #       value = "track",
    #       title = list(fontawesome::fa("right-from-bracket"), "Output"),
    #       shiny::br(),
    #       shiny::downloadButton(
    #         outputId = "downloadMP4",
    #         label = "Video",
    #         class = "btn-primary",
    #         style = "width:100%; border-color:white; border-radius: 10px;",
    #       ),
    #       shiny::br(),
    #       shiny::br(),
    #       shiny::uiOutput(outputId = "videooutput"),
    #       align = "center"
    #     ),
    #     shiny::tabPanel(
    #       title = list(fontawesome::fa("chart-line"), "Plots"),
    #       shiny::br(),
    #       shiny::fluidPage(
    #         shiny::fluidRow(
    #           shiny::column(
    #             4,
    #             shiny::downloadButton(
    #               outputId = "downloadPATH",
    #               label = "Trajectory data",
    #               class = "btn-primary",
    #               style = "width:100%; border-color:white; border-radius: 10px;",
    #             ),
    #           ),
    #           shiny::column(
    #             4,
    #             shiny::downloadButton(
    #               outputId = "downloadDISPL",
    #               label = "Displacement data",
    #               class = "btn-primary",
    #               style = "width:100%; border-color:white; border-radius: 10px;",
    #             ),
    #           ),
    #           shiny::column(
    #             4,
    #             shiny::downloadButton(
    #               outputId = "downloadCC",
    #               label = "Cross-correlation data",
    #               class = "btn-primary",
    #               style = "width:100%; border-color:white; border-radius: 10px;",
    #             ),
    #             align = "center"
    #           ),
    #         ),
    #       ),
    #       shiny::br(),
    #       shiny::br(),
    #       shiny::plotOutput("plotTrack",  width = "100%"),
    #       align = "center"
    #     ),
    #     shiny::tabPanel(
    #       title = list(fontawesome::fa("table"), "Tables"),
    #       shiny::br(),
    #       DT::dataTableOutput("tableTrack", width = "100%"),
    #       align = "center"
    #     ),
    #   ),
    # ),
    shiny::tabPanel(
      title = list(fontawesome::fa("circle-info")),
      shiny::br(),
      shiny::HTML(
        "<p>1. Upload an Excel file (.xlsx) with the <b>Upload</b> button.</p>
            <p>2. Choose variables for analysis.</p>
            <p>3. Click <b>Plan</b> to configure the statistial analysis.</p>
            <p>4. Click <b>Analyze</b>. Wait until the red progress bar on the top stops blinking.</p>
            <p>5. Check <b>Table</b> tab to visualize the results in tabular format.</p>
            <p>6. Check <b>Plot</b> tab to visualize the results in image format</p>
            <p>7. Click <b>restart</b> icon before running new analisys.",
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
    shinyWidgets::updatePickerInput(inputId = "ID", choices = colnames(rawdata), )
    # update list of between-subject variables from rawdata header
    shinyWidgets::updatePickerInput(inputId = "BGF", choices = colnames(rawdata), )
    # update list of covariates from rawdata header
    shinyWidgets::updatePickerInput(inputId = "CV", choices = colnames(rawdata), )
    # update list of outcome variables from rawdata header
    shinyWidgets::updatePickerInput(inputId = "OV", choices = colnames(rawdata), )
    
    DT::datatable(
      data = rawdata,
      rownames = FALSE,
      options = list(
        dom = 'tipr',
        searching = FALSE,
        pageLength = 10,
        width = "100%",
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # show selected ID in IDtable
  output[["IDtable"]] <- shiny::renderTable({
    shiny::req(input[["ID"]])
    data.frame(input[["ID"]])
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = FALSE)
  
  # show selected BGF in BGFtable
  output[["BGFtable"]] <- shiny::renderTable({
    shiny::req(input[["BGF"]])
    data.frame(input[["BGF"]])
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = FALSE)
  
  # show selected CV in CVtable
  output[["CVtable"]] <- shiny::renderTable({
    shiny::req(input[["CV"]])
    data.frame(input[["CV"]])
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = FALSE)
  
  # show selected OV in OVtable
  output[["OVtable"]] <- shiny::renderTable({
    shiny::req(input[["OV"]])
    data.frame(input[["OV"]])
  }, striped = TRUE, bordered = TRUE, width = "100%", rownames = FALSE, colnames = FALSE)
  
  # show table with selected variables
  output[["datatable"]] <- DT::renderDT({
    shiny::req(rawdata())
    
    # read file
    rawdata <- readxl::read_xlsx(rawdata())
    # remove empty columns
    rawdata <- rawdata[, colSums(is.na(rawdata)) != nrow(rawdata)]
    # remove empty rows
    rawdata <- rawdata[rowSums(is.na(rawdata)) != ncol(rawdata), ]
    
    # select columns from checked variables
    rawdata <- rawdata[, c(input[["ID"]], input[["BGF"]], input[["CV"]], input[["OV"]])]
    
    # show datatable
    DT::datatable(
      data = rawdata,
      rownames = FALSE,
      options = list(
        dom = 'tipr',
        searching = FALSE,
        pageLength = 10,
        width = "100%",
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  
  
  # # plot single frame of video imgEdit ---------------------------------------------------------
  # output[["plotMeasure"]] <- shiny::renderPlot({
  #   shiny::req(Video())
  #   shiny::req(input$framesEdit)
  #   shiny::req(input$imgEdit)
  #   # Get video info such as width, height, format, duration and framerate
  #   info <- av::av_media_info(Video())
  #
  #   # show 1st frame
  #   img <-
  #     magick::image_read(
  #       list.files(
  #         path = file.path(dir.name, "1 edited"),
  #         full.names = TRUE,
  #         pattern = "png")[input$imgEdit]
  #     )
  #
  #   # color palette (grayscale)
  #   pal <- grDevices::gray(seq(
  #     from = 0,
  #     to = 1,
  #     length.out = 256
  #   ), alpha = NULL)
  #   par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
  #   plot(
  #     img,
  #     xlim = c(0, info$video$width),
  #     ylim = c(0, info$video$height),
  #     asp = 1,
  #     col = pal
  #   )
  #
  #   # draw free-hand object
  #   lines(
  #     x = vals$x,
  #     y = vals$y,
  #     col = "red",
  #     lty = "solid",
  #     lwd = 2
  #   )
  # }, height = function() {
  #   (session$clientData$output_plotMeasure_width) * (0.6584)
  # })
  #
  # # measurements of single frame of video imgEdit ---------------------------------------------------------
  # output[["tableMeasure"]] <- DT::renderDataTable({
  #   shiny::req(Video())
  #   shiny::req(input$framesEdit)
  #   shiny::req(input$imgEdit)
  #   shiny::req(input$imgScale)
  #
  #   # Get video info such as width, height, format, duration and framerate
  #   info <- av::av_media_info(Video())
  #
  #   # show 1st frame
  #   img <-
  #     magick::image_read(
  #       list.files(
  #         path = file.path(dir.name, "1 edited"),
  #         full.names = TRUE,
  #         pattern = "png")[input$imgEdit]
  #     )
  #
  #   # get separate channels
  #   img_object <- as.integer(img[[1]])
  #   img_object_R <- img_object[, , 1]
  #   img_object_G <- img_object[, , 2]
  #   img_object_B <- img_object[, , 3]
  #
  #   # flip image vertically
  #   img_object_R <- img_object_R[nrow(img_object_R):1, ]
  #   img_object_G <- img_object_G[nrow(img_object_G):1, ]
  #   img_object_B <- img_object_B[nrow(img_object_B):1, ]
  #
  #   # create closed polygon
  #   poly <- round(concaveman::concaveman(cbind(vals$x, vals$y), concavity = 1, length_threshold = 0), 0)
  #   poly <- poly[complete.cases(poly), ]
  #
  #   # subset the image using polygon coordinates
  #   img_object_R <- img_object_R[
  #     min(poly[, 1]):max(poly[, 1]),
  #     min(poly[, 2]):max(poly[, 2])
  #   ]
  #   img_object_G <- img_object_G[
  #     min(poly[, 1]):max(poly[, 1]),
  #     min(poly[, 2]):max(poly[, 2])
  #   ]
  #   img_object_B <- img_object_B[
  #     min(poly[, 1]):max(poly[, 1]),
  #     min(poly[, 2]):max(poly[, 2])
  #   ]
  #
  #   # custom functions
  #   source("f_meas.R", local = TRUE)
  #   data <- f_measurement(R = img_object_R, G = img_object_G, B = img_object_B, poly = poly)
  #   contour <- data$contour
  #
  #   # draw actual ROI object
  #   lines(
  #     x = contour$x,
  #     y = contour$y,
  #     col = "red",
  #     lty = "solid",
  #     lwd = 2
  #   )
  #
  #   # distance
  #   distancia <- 0
  #
  #   # cross-sectional area
  #   area <- distancia * distancia * pi
  #
  #   # show measurements
  #   df_meas <- data.frame(
  #     "File name" = input$InputFile[1],
  #     "Frames (n)" = info$video$frames,
  #     "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
  #     "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
  #     "Current frame (n)" = input$imgEdit[1],
  #     "Object size (px)" = max(poly[, 1] - min(poly[, 1])) * max(poly[, 2] - min(poly[, 2])),
  #     "Distance (mm)" = round(distancia, digits = 2),
  #     "Cross-sectional area (mm²)" = round(area, digits = 2),
  #     "Threshold (Otsu)" = data$threshold,
  #     "Echogenicity, B&W (%)" = round(data$ecogenicidade_bw, digits = 2),
  #     "Echogenicity, gray (%)" = round(data$ecogenicidade_gray, digits = 2)
  #   )
  #   df_meas <- t(df_meas)
  #
  #   labels <-
  #     c(
  #       "File name",
  #       "Frames (n)",
  #       "Start - End (frames)",
  #       "Video size (px)",
  #       "Current frame (n)",
  #       "Object size (px)",
  #       "Distance (mm)",
  #       "Cross-sectional area (mm²)",
  #       "Threshold (Otsu)",
  #       "Echogenicity, B&W (%)",
  #       "Echogenicity, gray (%)"
  #     )
  #   rownames(df_meas) <-
  #     labels
  #
  #   # show DT table with buttons
  #   DT::datatable(
  #     df_meas,
  #     rownames = TRUE,
  #     colnames = rep("", ncol(df_meas)),
  #     extensions = c("Buttons", "FixedColumns"),
  #     options = list(
  #       dom = "B",
  #       ordering = F,
  #       buttons = list(
  #         list(extend = "copy",
  #              text = "Copy"),
  #         list(extend = "csv",
  #              text = "CSV"),
  #         list(extend = "excel",
  #              text = "Excel"),
  #         list(extend = "pdf",
  #              text = "PDF")
  #       ),
  #       fixedColumns = TRUE,
  #       pageLength = length(labels),
  #       autoWidth = TRUE,
  #       columnDefs = list(list(className = 'dt-center', targets = "_all"))
  #     )
  #   )
  # })
  #
  # # show PNG file of 1st frame ---------------------------------------------------------
  # output[["plotROI"]] <- shiny::renderPlot({
  #   shiny::req(Video())
  #   shiny::req(input$framesEdit)
  #
  #   # Get video info such as width, height, format, duration and framerate
  #   info <- av::av_media_info(Video())
  #
  #   # show 1st frame
  #   img <-
  #     magick::image_read(
  #       list.files(
  #         path = file.path(dir.name, "1 edited"),
  #         full.names = TRUE,
  #         pattern = "png")[1]
  #       )
  #
  #   # color palette (grayscale)
  #   pal <- grDevices::gray(seq(
  #     from = 0,
  #     to = 1,
  #     length.out = 256
  #   ), alpha = NULL)
  #   par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
  #   plot(
  #     img,
  #     xlim = c(0, info$video$width),
  #     ylim = c(0, info$video$height),
  #     asp = 1,
  #     col = pal
  #   )
  #   # custom functions
  #   round_2_odd <- function(x) {
  #     2 * floor(x / 2) + 1
  #   }
  #
  #   # draw object rectangle
  #   rect(
  #     xleft = roi_coords$xy[2, 1] - floor(input$KernelSizeTrack / 2),
  #     ybottom = roi_coords$xy[2, 2] - floor(input$KernelSizeTrack / 2),
  #     xright = roi_coords$xy[2, 1] + floor(input$KernelSizeTrack / 2),
  #     ytop = roi_coords$xy[2, 2] + floor(input$KernelSizeTrack / 2),
  #     col = "transparent",
  #     border = "red",
  #     lty = "solid",
  #     lwd = 2
  #   )
  #
  #   roi <-
  #     round_2_odd(input$KernelSizeTrack * (1 + as.numeric(input$Overlap) / 100)) # odd numbers only
  #   # draw ROI rectangle
  #   rect(
  #     xleft = roi_coords$xy[2, 1] - floor(roi / 2),
  #     ybottom = roi_coords$xy[2, 2] - floor(roi / 2),
  #     xright = roi_coords$xy[2, 1] + floor(roi / 2),
  #     ytop = roi_coords$xy[2, 2] + floor(roi / 2),
  #     col = "transparent",
  #     border = "yellow",
  #     lty = "solid",
  #     lwd = 2
  #   )
  #   # show coordinates of ROI
  #   text(
  #     x = roi_coords$xy[2, 1],
  #     y = roi_coords$xy[2, 2],
  #     paste0(
  #       "x=",
  #       round(roi_coords$xy[2, 1], 0),
  #       "\n",
  #       "y=",
  #       round(roi_coords$xy[2, 2], 0)
  #     ),
  #     col = "red"
  #   )
  # }, height = function() {
  #   (session$clientData$output_plotROI_width) * (0.6584)
  # })
  #
  # # process, play and export video ---------------------------------------------------------
  # shiny::observeEvent(input[["buttAnalyze"]], {
  #   shiny::req(Video())
  #   # Get video info such as width, height, format, duration and framerate
  #   info <- av::av_media_info(Video())
  #
  #   # Capture the result of us_track function call
  #   us_track(
  #     center.ini <-
  #       list(x = roi_coords$xy[2, 1], y = roi_coords$xy[2, 2]),
  #     inputfile = file.path(dir.name, "editeOVideo.mp4"),
  #     filtertype = input$FilterType,
  #     filtersize = input$FilterSize,
  #     overlap = input$Overlap,
  #     jump = input$Jump,
  #     kernel = input$KernelSizeTrack
  #   )
  #
  #   # draw trajectory on images
  #   path <-
  #     read.csv(file.path(dir.name, "CSV", "trajectory_measured.csv"))
  #   for (i in 1:length(list.files(file.path(dir.name, "8 output")))) {
  #     # read image from file
  #     img <- png::readPNG(file.path(dir.name, "8 output", paste0("image_", sprintf("%06d", i), ".png")))
  #     img <- grDevices::as.raster(img[, , 1:3])
  #
  #     # create a ggplot object with the image in background and trajectory as data points
  #     par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
  #     ggplot2::ggplot() +
  #       ggplot2::annotation_raster(
  #         img,
  #         xmin = 0,
  #         xmax = info$video$width,
  #         ymin = 0,
  #         ymax = info$video$height,
  #         interpolate = TRUE
  #       ) +
  #       ggplot2::geom_point(
  #         data = path[1:i,],
  #         ggplot2::aes(x = X, y = Y),
  #         colour = "red",
  #         size = 100
  #       ) +
  #       ggplot2::scale_x_continuous(limits = c(0, info$video$width),
  #                                   expand = c(0, 0)) +
  #       ggplot2::scale_y_continuous(limits = c(0, info$video$height),
  #                                   expand = c(0, 0)) +
  #       ggplot2::coord_fixed() +
  #       ggplot2::theme_void()
  #
  #     # save ggplot as png
  #     ggplot2::ggsave(
  #       filename = file.path(dir.name, "8 output", paste0("image_", sprintf("%06d", i), ".png")),
  #       width = info$video$width,
  #       height = info$video$height,
  #       units = "px",
  #       dpi = 1,
  #       type = "cairo",
  #       limitsize = FALSE
  #     )
  #   }
  #
  #   # build mp4 video using av video package from out.dir files
  #   av::av_encode_video(
  #     input = list.files(
  #       file.path(dir.name, "8 output"),
  #       pattern = ".png",
  #       full.names = TRUE
  #     ),
  #     output = file.path(dir.name, "outputvideo.mp4"),
  #     framerate = info$video$framerate
  #   )
  # })
  #
  # # show MP4 video of output file
  # output[["videooutput"]] <- shiny::renderUI({
  #   shiny::req(Video())
  #   # Get video info such as width, height, format, duration and framerate
  #   info <- av::av_media_info(Video())
  #
  #   # show video
  #   tags$video(
  #     width = "90%",
  #     height = "90%",
  #     controls = "",
  #     tags$source(src = "outputvideo.mp4", type = "video/mp4")
  #   )
  # })
  #
  # df <-
  #   shiny::reactive(if (file.exists(file.path(
  #     dir.name, "CSV", "max_cross_correlation.csv"
  #   )) &
  #   file.exists(file.path(dir.name, "CSV", "displacement.csv"))) {
  #     info <- av::av_media_info(Video())
  #     data.frame(
  #       "File name" = input$InputFile[1],
  #       "Frames (n)" = info$video$frames,
  #       "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
  #       "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
  #       "X0 (px)" = round(roi_coords$xy[2, 1], 0),
  #       "Y0 (px)" = round(roi_coords$xy[2, 2], 0),
  #       "Object size (px)" = input$KernelSizeTrack,
  #       "Filter type" = input$FilterType,
  #       "Filter size (px)" = input$FilterSize,
  #       "Overlap (%)" = input$Overlap,
  #       "Jump (frames)" = input$Jump,
  #       "Displacement, total (px)" = round(sum(read.csv(
  #         file.path(dir.name, "CSV", "displacement.csv")
  #       )[[1]], na.rm = TRUE), 0),
  #       "Displacement, mean (px)" = round(mean(read.csv(
  #         file.path(dir.name, "CSV", "displacement.csv")
  #       )[[1]], na.rm = TRUE), 0),
  #       "Speed, mean (px/frame)" = round(mean(abs(
  #         diff(read.csv(
  #           file.path(dir.name, "CSV", "displacement.csv")
  #         )[[1]], differences = 1)
  #       ), na.rm = TRUE), 3),
  #       "Speed, max (px/frame)" = round(max(abs(
  #         diff(read.csv(
  #           file.path(dir.name, "CSV", "displacement.csv")
  #         )[[1]], differences = 1)
  #       ), na.rm = TRUE), 3),
  #       "Cross-correlation, max" = round(max(read.csv(
  #         file.path(dir.name, "CSV", "max_cross_correlation.csv")
  #       )[[1]], na.rm = TRUE), 3),
  #       "Cross-correlation, mean" = round(mean(read.csv(
  #         file.path(dir.name, "CSV", "max_cross_correlation.csv")
  #       )[[1]], na.rm = TRUE), 3),
  #       "Cross-correlation, min" = round(min(read.csv(
  #         file.path(dir.name, "CSV", "max_cross_correlation.csv")
  #       )[[1]], na.rm = TRUE), 3)
  #     )
  #   } else {
  #     info <- av::av_media_info(Video())
  #     data.frame(
  #       "File name" = input$InputFile[1],
  #       "Frames (n)" = info$video$frames,
  #       "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
  #       "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
  #       "X0 (px)" = round(roi_coords$xy[2, 1], 0),
  #       "Y0 (px)" = round(roi_coords$xy[2, 2], 0),
  #       "Object size (px)" = input$KernelSizeTrack,
  #       "Filter type" = input$FilterType,
  #       "Filter size (px)" = input$FilterSize,
  #       "Overlap (%)" = input$Overlap,
  #       "Jump (frames)" = input$Jump,
  #       "Displacement, total (px)" = NA,
  #       "Displacement, mean (px)" = NA,
  #       "Speed, mean (px/frame)" = NA,
  #       "Speed, max (px/frame)" = NA,
  #       "Cross-correlation, max" = NA,
  #       "Cross-correlation, mean" = NA,
  #       "Cross-correlation, min" = NA
  #     )
  #   })
  #
  # # plot results of th CSV files ---------------------------------------------------------
  # output[["plotTrack"]] <- shiny::renderImage({
  #   shiny::req(Video())
  #   # Get video info such as width, height, format, duration and framerate
  #   info <-
  #     av::av_media_info(file.path(dir.name, "outputvideo.mp4"))
  #
  #   source("f_plot.R", local = TRUE)
  #   img <- htmltools::capturePlot({
  #     plot.trajectory(res.dir = file.path(dir.name, "CSV"),
  #                     info = info)
  #   }, height = 500, width = 500)
  #   list(
  #     src = img,
  #     height = "auto",
  #     width = "auto",
  #     contentType = "image/png"
  #   )
  # }, deleteFile = TRUE)
  #
  # # show datatable of results ---------------------------------------------------------
  # output[["tableTrack"]] <- DT::renderDataTable({
  #   shiny::req(Video())
  #   shiny::req(df())
  #   df <- t(df())
  #   labels <-
  #     c(
  #       "File name",
  #       "Frames (n)",
  #       "Start - End (frames)",
  #       "Video size (px)",
  #       "X0 (px)",
  #       "Y0 (px)",
  #       "Object size (px)",
  #       "Filter type",
  #       "Filter size (px)",
  #       "Overlap (%)",
  #       "Jump (frames)",
  #       "Displacement, total (px)",
  #       "Displacement, mean (px)",
  #       "Speed, mean (px/frame)",
  #       "Speed, max (px/frame)",
  #       "Cross-correlation, max",
  #       "Cross-correlation, mean",
  #       "Cross-correlation, min"
  #     )
  #   rownames(df) <-
  #     labels
  #   # show DT table with buttons
  #   DT::datatable(
  #     df,
  #     rownames = TRUE,
  #     colnames = rep("", ncol(df)),
  #     extensions = c("Buttons", "FixedColumns"),
  #     options = list(
  #       dom = "B",
  #       ordering = F,
  #       buttons = list(
  #         list(extend = "copy",
  #              text = "Copy"),
  #         list(extend = "csv",
  #              text = "CSV"),
  #         list(extend = "excel",
  #              text = "Excel"),
  #         list(extend = "pdf",
  #              text = "PDF")
  #       ),
  #       fixedColumns = TRUE,
  #       pageLength = length(labels),
  #       autoWidth = TRUE,
  #       columnDefs = list(list(className = 'dt-center', targets = "_all"))
  #     )
  #   )
  # })
  #
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
  # # download DISPLACEMENT CSV files ---------------------------------------------------------
  # output[["downloadDISPL"]] <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       paste0("displacement.csv")
  #     },
  #     content = function(file) {
  #       file.copy(from = file.path(dir.name, "CSV", "displacement.csv"),
  #                 to = file)
  #     }
  #   )
  #
  # # download CC CSV files ---------------------------------------------------------
  # output[["downloadCC"]] <-
  #   shiny::downloadHandler(
  #     filename = function() {
  #       paste0("max_cross_correlation.csv")
  #     },
  #     content = function(file) {
  #       file.copy(from = file.path(dir.name, "CSV", "max_cross_correlation.csv"),
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
