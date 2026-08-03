#' Module Importation & Inspection des Données
#' 
#' @param id Identifiant du module Shiny
#' @return Server return: list contenant `df_reactive` et `meta_reactive`

mod_import_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        bsicons::bs_icon("cloud-arrow-up", class = "me-2 text-primary"),
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
      tags$h6(bsicons::bs_icon("sliders", class = "me-1"), "Options de Nettoyage Initial"),
      
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
          title = tags$span(bsicons::bs_icon("table"), " Aperçu de la Table"),
          DT::DTOutput(ns("preview_table"))
        ),
        bslib::nav_panel(
          title = tags$span(bsicons::bs_icon("info-circle"), " Structure des Colonnes & Types"),
          tableOutput(ns("column_types_table"))
        ),
        bslib::nav_panel(
          title = tags$span(bsicons::bs_icon("exclamation-triangle"), " Synthèse des Manquants"),
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
              showcase = bsicons::bs_icon("file-earmark-x"),
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
          showcase = bsicons::bs_icon("people-fill"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Variables / Colonnes",
          value = format(n_cols, big.mark = " "),
          showcase = bsicons::bs_icon("columns-gap"),
          theme = "info"
        ),
        bslib::value_box(
          title = "Valeurs Manquantes (NA)",
          value = sprintf("%s (%s%%)", format(n_na, big.mark = " "), pct_na),
          showcase = bsicons::bs_icon("exclamation-circle-fill"),
          theme = if (pct_na > 15) "warning" else "success"
        )
      )
    })
    
    # 5. DT Preview Table
    output$preview_table <- DT::renderDT({
      df <- cleaned_data()
      req(df)
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/French.json')
        ),
        class = "cell-border stripe hover"
      )
    })
    
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
