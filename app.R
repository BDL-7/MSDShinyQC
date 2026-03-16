


# Source dependencies ----

source("appDirectory.R")
source(file.path(R_FILES_DIR, "packages.R"), local = TRUE)
source(file.path(R_FILES_DIR, "set_path.R"), local = TRUE)
source(file.path(R_FILES_DIR, "msdsigdata.R"), local = TRUE)


# UI ----

ui <- bs4DashPage(
  title = "MSD Shiny QC",
  dark  = FALSE,              # Don't need dark mode

  header = bs4DashNavbar(title = "MSD QC Dashboard"),

  sidebar = bs4DashSidebar(
    skin       = "light",
    status     = "primary",
    brandColor = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem("QC Report",    
                         tabName = "qc",     
                         icon = icon("flask")),
      bs4SidebarMenuItem("Append Files", 
                         tabName = "append", 
                         icon = icon("file-excel"))
    )
  ),

  body = bs4DashBody(
    useShinyjs(),

    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-footer, 
      .wrapper { background-color: #e6f7ff !important; }
      .card-header {
        background-color: #A1D6E2; color: #000000;
        font-weight: bold; font-size: 16px;
        border-top: 3px solid #000000; border-bottom: 3px solid #000000;
      }
      .btn-custom-run, .btn-custom-download {
        background: linear-gradient(135deg, #a8edea 35%, #f3f8f9 65%);
        color: #000000; border: 1.5px solid black; border-radius: 6px;
        box-shadow: 1px 1px 5px rgba(173,173,173,0.3);
      }
      .card-title { font-weight: bold; }
      table.dataTable td a { text-decoration: none; font-weight: 600; }
    "))),

    bs4TabItems(

      # QC tab ----
      bs4TabItem(
        tabName = "qc",
        fluidRow(

          bs4Card(
            title = "Upload Experiment Data", solidHeader = TRUE,
            collapsible = TRUE, width = 3,
            style = "background: linear-gradient(to right, #f0f9ff, #a8edea);",

            selectInput("team", "Choose Team:",
                        choices = c("HIR", "IVYBivalent", "Sam5k50k", "VRI"),
                        selected = "HIR"),

            virtualSelectInput("plex", "Plate Type", multiple = FALSE,
              choices = c(
                "SARS-CoV-2 Plate 2 (3-plex)",
                "Respiratory Plate 1 (6-plex)",
                "SARS-CoV-2 Plate 32 (10-plex, variant)",
                "SARS-CoV-2 Plate 38 (10-plex, variant)"
              )
            ),

            virtualSelectInput("abType", "Antibody", multiple = TRUE,
              choices = c("IgG", "IgA", "IgM"),
              position = "auto", dropboxWidth = "100%", zIndex = 1000
            ),

            div(style = "margin-top:20px;",
              fileInput("filesforRApp", NULL, accept = ".xlsx",
                        buttonLabel = "Browse File",
                        placeholder = "Upload one experiment XLSX file")
            ),

            actionButton("runRApp", "Run Program",
                         class = "btn btn-custom-run btn-block"),
            div(style = "margin-top:15px;"),
            uiOutput("download_ui"),
            div(style = "margin-top:15px;"),

            radioButtons("downloadRawfileType", "Download Raw Data As:",
                         choices = c(".xlsx", ".csv", ".RDS"),
                         selected = ".xlsx"),
            downloadButton("downloadData", "Download Raw Data",
                           class = "btn btn-custom-download btn-block")
          ),

          bs4Card(
            title = "Reports (View / Download)", solidHeader = TRUE,
            collapsible = TRUE, width = 9,
            style = "background: linear-gradient(to right, #f0f9ff, #a8edea);",
            DT::dataTableOutput("reportsTable")
          )
        ),

        fluidRow(
          bs4Card(
            title = "Merged Raw Data Table", solidHeader = TRUE,
            collapsible = TRUE, width = 12,
            style = "background: linear-gradient(to right, #f0f9ff, #a8edea);",
            withSpinner(dataTableOutput("table1"), type = 6)
          )
        )
      ),

      # Append tab ----
      bs4TabItem(
        tabName = "append",
        bs4Card(
          title = "Append Multiple Excel Files", status = "warning",
          solidHeader = TRUE, width = 12,
          fileInput("files", "Choose Excel Files", multiple = TRUE, accept = ".xlsx"),
          checkboxInput("removeDuplicates", "Remove duplicate rows",      value = TRUE),
          checkboxInput("addCutoffs",       "Add Signal and MSD Cutoffs", value = FALSE),
          actionButton("append", "Append & Preview", class = "btn btn-info"),
          br(), br(),
          downloadButton("multiexceldownload", "Download .xlsx File",
                         class = "btn btn-custom-download"),
          textOutput("appendStatus"),
          textOutput("errorMessage")
        ),
        bs4Card(
          title = "Preview Data", status = "secondary",
          collapsible = TRUE, width = 12,
          shinycssloaders::withSpinner(DT::dataTableOutput("appendData"), type = 6)
        )
      )

    )
  )
)


# Server ----

server <- function(input, output, session) {

  # Source helpers ----
  source(file.path(R_FILES_DIR, "helpers.R"),             local = TRUE)
  source(file.path(R_FILES_DIR, "validate_excel.R"),      local = TRUE)
  source(file.path(R_FILES_DIR, "process_excel_files.R"), local = TRUE)
  source(file.path(R_FILES_DIR, "shiny_quarto_safety.R"), local = TRUE)
  source(file.path(R_FILES_DIR, "run_qc.R"),              local = TRUE)


  # Reactives ----

  processed_data <- reactive({
    req(input$filesforRApp)
    vr <- validate_msd_file(input$filesforRApp$datapath, 
                            input$filesforRApp$name, 
                            mode = "qc")
    if (!show_validation_modal(vr, "QC File Validation")) return(NULL)

    result <- process_excel_files(as.character(input$filesforRApp$datapath))
    if (!is.list(result) || !"main_data" %in% names(result)) {
      stop("process_excel_files() did not return the expected structure.")
    }
    session$userData$s1_raw       <- result$s1_raw
    session$userData$s2_raw       <- result$s2_raw
    session$userData$s3_processed <- result$s3_processed
    result$main_data
  })

  msd_cutoff_data <- reactive({
    req(input$plex, input$abType)
    msd_cutoff_filter(msdcutoffs, input$plex, input$abType)
  })

  sig_cutoff_data <- reactive({
    req(input$plex, input$abType)
    sig_cutoff_filter(signalcutoffs, input$plex, input$abType)
  })

  merged_data <- reactive({
    processed_data() |>
      dplyr::left_join(msd_cutoff_data(), 
                       by = c("Plate Type", "Antibody", "Assay")) |>
      dplyr::left_join(sig_cutoff_data(), 
                       by = c("Plate Type", "Antibody", "Assay", "Sample")) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(),       
                      ~ifelse(is.na(.), ".", .)),
        dplyr::across(c("Barcode2", "Barcode3"), 
                      ~ifelse(. == "N/A", ".", .))
      )
  })


  # UI outputs ----

  output$download_ui <- renderUI({
    if (is.null(input$team) || identical(input$team, "HIR")) {
      tags$div(p("Run Program will generate reports."))
    } else {
      tags$div(
        p("After running, click below to download the QC Excel:"),
        downloadButton("downloadExcelQC", "Download QC Excel",
                       class = "btn btn-custom-download btn-block")
      )
    }
  })

  output$table1 <- renderDataTable({ req(merged_data()); merged_data() })


  # Run program ----

  observeEvent(input$runRApp, {
    req(merged_data())
    run_qc(session, input, output, 
           merged_data(), 
           RDS_DIR, REPORTS_DIR, R_FILES_DIR)
  })


  # Download handlers ----

  output$downloadData <- downloadHandler(
    filename = function() {
      req(merged_data(), input$downloadRawfileType, input$filesforRApp)
      ext <- switch(input$downloadRawfileType, 
                    ".xlsx" = ".xlsx", ".csv" = ".csv", ".RDS" = ".rds", ".xlsx")
      paste0(tools::file_path_sans_ext(input$filesforRApp$name), "_Raw_Data_",
             as.character(merged_data()$ExperimentDate[1]), ext)
    },
    content = function(file) {
      req(merged_data())
      dat <- merged_data()
      if      (identical(input$downloadRawfileType, ".xlsx")) openxlsx::write.xlsx(dat, file)
      else if (identical(input$downloadRawfileType, ".csv"))  utils::write.csv(dat, file, row.names = FALSE, na = "")
      else if (identical(input$downloadRawfileType, ".RDS"))  saveRDS(dat, file)
    }
  )

  output$downloadExcelQC <- downloadHandler(
    filename = function() paste0("QC_Results_", input$team, "_", Sys.Date(), ".xlsx"),
    content  = function(file) openxlsx::write.xlsx(merged_data(), file)
  )


  # Append tab ----

  appended_data <- reactiveVal(NULL)

  observeEvent(input$append, {
    req(input$files)
    output$errorMessage <- renderText("")
    output$appendStatus <- renderText("Validating files...")

    vr <- validate_msd_file(input$files$datapath, input$files$name, mode = "append")
    if (!show_validation_modal(vr, "Append File Validation")) {
      output$appendStatus <- renderText("Blocked: fix errors and re-upload.")
      return()
    }

    tryCatch({
      output$appendStatus <- renderText("Processing files...")
      data <- process_excel_files(input$files$datapath)$main_data

      if (isTRUE(input$removeDuplicates)) data <- dplyr::distinct(data)

      if (isTRUE(input$addCutoffs)) {
        data <- data |>
          dplyr::left_join(msdcutoffs,    
                 by = c("Plate Type", "Antibody", "Assay")) |>
          dplyr::left_join(signalcutoffs, 
                 by = c("Plate Type", "Antibody", "Assay", "Sample"))
      }

      appended_data(data)
      output$appendStatus <- renderText(paste("Success:", nrow(data), "rows appended."))

    }, error = function(e) {
      appended_data(NULL)
      output$errorMessage <- renderText(paste("Error:", e$message))
      output$appendStatus <- renderText("Failed.")
    })
  })

  output$appendData <- DT::renderDataTable(
    { req(appended_data()); appended_data() },
    options = list(pageLength = 10, scrollX = TRUE)
  )

  output$multiexceldownload <- downloadHandler(
    filename = function() paste0("Combined_experiments_", Sys.Date(), ".xlsx"),
    content  = function(file) { req(appended_data()); openxlsx::write.xlsx(appended_data(), file) }
  )

}


# Launch ----

if (!dir.exists("reports")) dir.create("reports", 
                                       recursive = TRUE, showWarnings = FALSE)
shinyApp(ui, server)
