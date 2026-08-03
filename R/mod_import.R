#' Module Importation & Inspection des Données
#' 
#' @param id Identifiant du module Shiny
#' @return Server return: list contenant `df_reactive` et `meta_reactive`

mod_import_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("cloud-upload-alt", class = "me-2 text-primary"),
        "Chargement des Données"
      ),
      width = 320,
      
      radioButtons(
        ns("data_source"), "Source des Données :",
        choices = c(
          "📁 Téléverser un fichier" = "file",
          "🧪 Données d'exemple (Iris)" = "example"
        ),
        selected = "file"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'file'", ns("data_source")),
        fileInput(
          ns("file"), 
          "Glisser-déposer ou Choisir :",
          accept = c(".xlsx", ".xls", ".csv", ".rds"),
          buttonLabel = "Parcourir...",
          placeholder = "Aucun fichier sélectionné"
        ),
        uiOutput(ns("excel_sheet_ui"))
      ),
      
      tags$hr(),
      tags$h6(icon("sliders-h", class = "me-1"), "Options de Nettoyage Initial"),
      
      checkboxInput(ns("clean_colnames"), "Nettoyer les noms de colonnes (snake_case)", value = TRUE),
      checkboxInput(ns("trim_whitespace"), "Supprimer les espaces superflus (trim)", value = TRUE),
      
      tags$hr(),
      actionButton(ns("reset_data"), "Réinitialiser", icon = icon("undo"), class = "btn-outline-danger btn-sm w-100")
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      # Cartes KPI
      uiOutput(ns("kpi_boxes_ui")),
      
      tags$div(class = "mb-3"),
      
      # Onglets de prévisualisation
      bslib::navset_card_tab(
        bslib::nav_panel(
          title = tags$span(icon("table"), " Aperçu de la Table"),
          uiOutput(ns("preview_table_container"))
        ),
        bslib::nav_panel(
          title = tags$span(icon("info-circle"), " Structure des Colonnes & Types"),
          tableOutput(ns("column_types_table"))
        ),
        bslib::nav_panel(
          title = tags$span(icon("exclamation-triangle"), " Synthèse des Manquants"),
          uiOutput(ns("missing_summary_ui"))
        )
      )
    )
  )
}

mod_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Excel sheet UI
    output$excel_sheet_ui <- renderUI({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext %in% c("xlsx", "xls")) {
        sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
        if (!is.null(sheets) && length(sheets) > 1) {
          selectInput(ns("excel_sheet"), "Sélectionner la Feuille Excel :", choices = sheets)
        }
      }
    })
    
    # 2. Reactive Data Loading
    raw_data <- reactive({
      if (input$data_source == "example") {
        return(iris)
      }
      
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      path <- input$file$datapath
      
      df <- switch(
        ext,
        csv = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
        xlsx = {
          sheet <- if (is.null(input$excel_sheet)) 1 else input$excel_sheet
          readxl::read_excel(path, sheet = sheet)
        },
        xls = {
          sheet <- if (is.null(input$excel_sheet)) 1 else input$excel_sheet
          readxl::read_excel(path, sheet = sheet)
        },
        rds = readRDS(path),
        stop("Format de fichier non supporté. Veuillez téléverser un fichier .xlsx, .xls, .csv ou .rds.")
      )
      
      return(as.data.frame(df))
    })
    
    # 3. Cleaned Data Reactive
    cleaned_data <- reactive({
      df <- raw_data()
      req(df)
      
      # Clean colnames if checked
      if (isTRUE(input$clean_colnames)) {
        names(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
        names(df) <- gsub("_+", "_", names(df))
        names(df) <- gsub("^_|_$", "", names(df))
        names(df) <- tolower(names(df))
      }
      
      # Trim whitespace if checked
      if (isTRUE(input$trim_whitespace)) {
        df <- as.data.frame(lapply(df, function(col) {
          if (is.character(col)) trimws(col) else col
        }), stringsAsFactors = FALSE)
      }
      
      df
    })
    
    # Reset button handler
    observeEvent(input$reset_data, {
      shinyjs::reset("file")
    })
    
    # 4. KPI Boxes Output
    output$kpi_boxes_ui <- renderUI({
      df <- tryCatch(cleaned_data(), error = function(e) NULL)
      if (is.null(df)) {
        return(
          bslib::layout_column_wrap(
            width = 1/3,
            bslib::value_box(
              title = "Fichier",
              value = "Aucun",
              showcase = icon("file-excel"),
              theme = "secondary"
            )
          )
        )
      }
      
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      n_na <- sum(is.na(df))
      pct_na <- round((n_na / (n_rows * n_cols)) * 100, 1)
      
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(
          title = "Lignes / Participants",
          value = format(n_rows, big.mark = " "),
          showcase = icon("users"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Variables / Colonnes",
          value = format(n_cols, big.mark = " "),
          showcase = icon("columns"),
          theme = "info"
        ),
        bslib::value_box(
          title = "Valeurs Manquantes (NA)",
          value = sprintf("%s (%s%%)", format(n_na, big.mark = " "), pct_na),
          showcase = icon("exclamation-circle"),
          theme = if (pct_na > 15) "warning" else "success"
        )
      )
    })
    
    # 5. Preview Table Container (Supporte DT si installé, sinon table standard)
    output$preview_table_container <- renderUI({
      if (requireNamespace("DT", quietly = TRUE)) {
        DT::DTOutput(ns("preview_dt"))
      } else {
        tableOutput(ns("preview_std"))
      }
    })
    
    output$preview_dt <- renderUI({
      req(requireNamespace("DT", quietly = TRUE))
      df <- cleaned_data()
      req(df)
      DT::renderDT(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        class = "cell-border stripe hover"
      )
    })
    
    output$preview_std <- renderTable({
      df <- cleaned_data()
      req(df)
      head(df, 15)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 6. Column Types Table
    output$column_types_table <- renderTable({
      df <- cleaned_data()
      req(df)
      
      data.frame(
        `Variable` = names(df),
        `Type R` = sapply(df, class),
        `Valeurs Manquantes` = sapply(df, function(x) sum(is.na(x))),
        `Exemple de Valeur` = sapply(df, function(x) paste(head(na.omit(x), 2), collapse = ", ")),
        check.names = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 7. Missing Values Summary
    output$missing_summary_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      
      na_counts <- sapply(df, function(x) sum(is.na(x)))
      na_pct <- round((na_counts / nrow(df)) * 100, 1)
      na_df <- data.frame(
        Variable = names(df),
        Manquants = na_counts,
        Pourcentage = paste0(na_pct, " %"),
        check.names = FALSE
      )
      na_df <- na_df[order(-na_counts), ]
      
      tags$div(
        tags$p("Analyse de la répartition des données manquantes par colonne :"),
        tableOutput(ns("na_table"))
      )
    })
    
    output$na_table <- renderTable({
      df <- cleaned_data()
      req(df)
      na_counts <- sapply(df, function(x) sum(is.na(x)))
      na_pct <- round((na_counts / nrow(df)) * 100, 1)
      na_df <- data.frame(
        `Nom de la Variable` = names(df),
        `Nombre de NA` = na_counts,
        `Proportion` = paste0(na_pct, " %"),
        check.names = FALSE
      )
      na_df[order(-na_counts), ]
    }, striped = TRUE, hover = TRUE)
    
    return(reactive({ cleaned_data() }))
  })
}
