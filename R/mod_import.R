#' Module Importation, Labellisation & Nettoyage
#' 
#' @param id Identifiant du module Shiny
#' @return Server return: list contenant `df_reactive` et `labels_reactive`

mod_import_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("cloud-upload-alt", class = "me-2 text-primary"),
        "Chargement & Options"
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
        uiOutput(ns("file_input_container")),
        uiOutput(ns("excel_sheet_ui"))
      ),
      
      tags$hr(),
      tags$h6(icon("sliders-h", class = "me-1"), "Nettoyage des Noms"),
      checkboxInput(ns("clean_colnames"), "Nettoyer les noms de colonnes (snake_case)", value = TRUE),
      checkboxInput(ns("trim_whitespace"), "Supprimer les espaces superflus (trim)", value = TRUE),
      
      tags$hr(),
      tags$h6(icon("magic", class = "me-1"), "Nettoyage & Imputation Avancés"),
      selectInput(ns("adv_clean_col"), "Choisir une colonne :", choices = NULL),
      selectInput(ns("adv_clean_action"), "Action à appliquer :",
                  choices = c(
                    "Aucune" = "none",
                    "Nettoyer en Texte (clean_text)" = "clean_text",
                    "Nettoyer en Numérique (clean_numeric)" = "clean_numeric",
                    "Nettoyer en Binaire (clean_binary)" = "clean_binary",
                    "Imputer par le Mode (impute_mode)" = "impute_mode",
                    "Imputer par la Moyenne (impute_mean)" = "impute_mean",
                    "Imputer par la Médiane (impute_median)" = "impute_median"
                  )),
      actionButton(ns("apply_adv_clean"), "Appliquer l'Action", class = "btn btn-outline-primary btn-sm w-100 mb-2"),

      tags$div(
        class = "mt-2 p-2 border rounded bg-light",
        tags$strong("Imputation Multiple (MICE) :"),
        tags$p("Impute tout le jeu de données d'un coup.", class = "text-muted small mb-1"),
        actionButton(ns("run_mice"), "Lancer MICE", class = "btn btn-primary btn-sm w-100")
      ),

      tags$hr(),
      tags$h6(icon("download", class = "me-1"), "Exporter les données"),
      downloadButton(ns("download_cleaned_csv"), "Télécharger CSV Nettoyé", class = "btn-outline-primary btn-sm w-100 mb-2"),
      
      tags$hr(),
      actionButton(ns("reset_data"), "Réinitialiser", icon = icon("undo"), class = "btn-outline-danger btn-sm w-100")
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      # KPIs
      uiOutput(ns("kpi_boxes_ui")),
      tags$div(class = "mb-3"),
      
      # Tabs
      bslib::navset_card_tab(
        # Tab 1: Preview Table
        bslib::nav_panel(
          title = tags$span(icon("table"), " Aperçu de la Table"),
          uiOutput(ns("preview_table_container"))
        ),
        
        # Tab 2: Label Manager (Gestion des Libellés)
        bslib::nav_panel(
          title = tags$span(icon("tag"), " Éditeur de Libellés (Labels)"),
          tags$div(
            class = "p-3",
            tags$p(class = "text-muted", "Attribuez des libellés francophones lisibles à vos variables (ex: 'age' ➔ 'Âge du patient (années)'). Ces libellés seront intégrés automatiquement dans tous vos tableaux et rapports Word."),
            actionButton(ns("apply_labels"), "Enregistrer les Libellés", icon = icon("save"), class = "btn-success btn-sm mb-3"),
            uiOutput(ns("labels_editor_ui"))
          )
        ),
        
        # Tab 3: Outliers Diagnostic
        bslib::nav_panel(
          title = tags$span(icon("search"), " Valeurs Aberrantes (Outliers)"),
          tags$div(
            class = "p-3",
            selectInput(ns("outlier_var"), "Choisir une variable numérique :", choices = NULL),
            uiOutput(ns("outlier_report_ui"))
          )
        ),
        
        # Tab 4: Missing Values
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
    
    # Trigger to reset file input
    reset_trigger <- reactiveVal(0)

    # Dynamic fileInput container
    output$file_input_container <- renderUI({
      reset_trigger() # dependency
      fileInput(
        ns("file"),
        "Glisser-déposer ou Choisir :",
        accept = c(".xlsx", ".xls", ".csv", ".rds"),
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné"
      )
    })

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
    
    # 2. Raw Data
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
        stop("Format de fichier non supporté.")
      )
      as.data.frame(df)
    })
    
    # 3. Cleaned Data
    cleaned_data <- reactiveVal(NULL)
    
    # Observe Reset Event
    observeEvent(input$reset_data, {
      updateRadioButtons(session, "data_source", selected = "file")
      reset_trigger(reset_trigger() + 1)
      cleaned_data(NULL)
      showNotification("Données réinitialisées avec succès !", type = "message")
    })

    # Initialize or reset cleaned_data when raw_data or standard cleaning checkboxes change
    observeEvent({
      raw_data()
      input$clean_colnames
      input$trim_whitespace
    }, {
      df <- raw_data()
      req(df)
      
      if (isTRUE(input$clean_colnames)) {
        if (exists("clean_names", where = asNamespace("analytix"))) {
          df <- analytix::clean_names(df)
        } else {
          names(df) <- gsub("[^a-zA-Z0-9_]", "_", names(df))
          names(df) <- gsub("_+", "_", names(df))
          names(df) <- gsub("^_|_$", "", names(df))
          names(df) <- tolower(names(df))
        }
      }
      
      if (isTRUE(input$trim_whitespace)) {
        df <- as.data.frame(lapply(df, function(col) {
          if (is.character(col)) trimws(col) else col
        }), stringsAsFactors = FALSE)
      }
      
      cleaned_data(df)
    })
    
    # Update selectors whenever cleaned_data changes
    observe({
      df <- cleaned_data()
      req(df)
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "outlier_var", choices = num_cols)
      updateSelectInput(session, "adv_clean_col", choices = names(df))
    })

    # Advanced Nettoyage & Imputation Handlers
    observeEvent(input$apply_adv_clean, {
      df <- cleaned_data()
      req(df, input$adv_clean_col)
      action <- input$adv_clean_action
      col_name <- input$adv_clean_col

      if (action == "none") {
        return()
      }

      tryCatch({
        new_col <- df[[col_name]]
        if (action == "clean_text") {
          if (exists("clean_text", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_text(new_col)
          } else {
            new_col <- trimws(as.character(new_col))
            new_col[new_col %in% c("", "NA", "N/A", "<NA>", "NULL")] <- NA
          }
        } else if (action == "clean_numeric") {
          if (exists("clean_numeric", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_numeric(new_col)
          } else {
            new_col <- as.numeric(gsub(",", ".", as.character(new_col)))
          }
        } else if (action == "clean_binary") {
          if (exists("clean_binary", where = asNamespace("analytix"))) {
            new_col <- analytix::clean_binary(new_col)
          } else {
            new_col <- factor(ifelse(tolower(as.character(new_col)) %in% c("oui", "yes", "1", "true"), "Oui", "Non"), levels = c("Oui", "Non"))
          }
        } else if (action == "impute_mode") {
          if (exists("impute_mode", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mode(new_col)
          } else {
            ux <- unique(new_col[!is.na(new_col)])
            mode_val <- ux[which.max(tabulate(match(new_col, ux)))]
            new_col[is.na(new_col)] <- mode_val
          }
        } else if (action == "impute_mean") {
          if (exists("impute_mean", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mean(new_col, type = "mean")
          } else {
            new_col[is.na(new_col)] <- mean(new_col, na.rm = TRUE)
          }
        } else if (action == "impute_median") {
          if (exists("impute_mean", where = asNamespace("analytix"))) {
            new_col <- analytix::impute_mean(new_col, type = "median")
          } else {
            new_col[is.na(new_col)] <- median(new_col, na.rm = TRUE)
          }
        }

        df[[col_name]] <- new_col
        cleaned_data(df)
        showNotification(paste0("Action '", action, "' appliquée avec succès sur '", col_name, "' !"), type = "message")
      }, error = function(e) {
        showNotification(paste("Erreur de nettoyage :", e$message), type = "error")
      })
    })

    observeEvent(input$run_mice, {
      df <- cleaned_data()
      req(df)
      shiny::withProgress(message = "Imputation multiple (MICE)...", value = 0.5, {
        tryCatch({
          if (exists("impute_mice", where = asNamespace("analytix"))) {
            imp_df <- analytix::impute_mice(df)
            cleaned_data(imp_df)
            showNotification("Imputation MICE exécutée avec succès !", type = "message")
          } else {
            showNotification("MICE non disponible dans le package.", type = "warning")
          }
        }, error = function(e) {
          showNotification(paste("Erreur MICE :", e$message), type = "error")
        })
      })
    })
    
    # 4. Labels Editor UI
    output$labels_editor_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      
      vars <- names(df)
      inputs <- lapply(vars, function(v) {
        current_lbl <- attr(df[[v]], "label")
        if (is.null(current_lbl)) current_lbl <- v
        
        tags$div(
          class = "row align-items-center mb-2",
          tags$div(class = "col-md-4 fw-bold text-truncate", v),
          tags$div(
            class = "col-md-8",
            textInput(ns(paste0("lbl_", v)), label = NULL, value = current_lbl, placeholder = paste("Libellé pour", v))
          )
        )
      })
      do.call(tags$div, inputs)
    })
    
    # Apply labels event
    observeEvent(input$apply_labels, {
      df <- cleaned_data()
      req(df)
      
      lbl_vector <- c()
      for (v in names(df)) {
        val <- input[[paste0("lbl_", v)]]
        if (!is.null(val) && nchar(trimws(val)) > 0) {
          lbl_vector[v] <- trimws(val)
        }
      }
      
      if (length(lbl_vector) > 0) {
        if (exists("label_vars", where = asNamespace("analytix"))) {
          df <- analytix::label_vars(df, lbl_vector)
        } else {
          for (nm in names(lbl_vector)) {
            if (nm %in% names(df)) attr(df[[nm]], "label") <- lbl_vector[[nm]]
          }
        }
        cleaned_data(df)
        showNotification("Libellés mis à jour avec succès !", type = "message")
      }
    })
    
    # 5. KPI Boxes
    output$kpi_boxes_ui <- renderUI({
      df <- cleaned_data()
      if (is.null(df)) {
        return(
          bslib::layout_column_wrap(
            width = 1/3,
            bslib::value_box(title = "Fichier", value = "Aucun", showcase = icon("file-excel"), theme = "secondary")
          )
        )
      }
      n_rows <- nrow(df)
      n_cols <- ncol(df)
      n_na <- sum(is.na(df))
      pct_na <- round((n_na / (n_rows * n_cols)) * 100, 1)
      
      bslib::layout_column_wrap(
        width = 1/3,
        bslib::value_box(title = "Lignes / Participants", value = format(n_rows, big.mark = " "), showcase = icon("users"), theme = "primary"),
        bslib::value_box(title = "Variables / Colonnes", value = format(n_cols, big.mark = " "), showcase = icon("columns"), theme = "info"),
        bslib::value_box(title = "Valeurs Manquantes (NA)", value = sprintf("%s (%s%%)", format(n_na, big.mark = " "), pct_na), showcase = icon("exclamation-circle"), theme = if (pct_na > 15) "warning" else "success")
      )
    })
    
    # 6. Preview Table Container
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
      DT::renderDT(df, options = list(pageLength = 10, scrollX = TRUE), class = "cell-border stripe hover")
    })
    
    output$preview_std <- renderTable({
      df <- cleaned_data()
      req(df)
      head(df, 15)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 7. Outlier Report
    output$outlier_report_ui <- renderUI({
      df <- cleaned_data()
      req(df, input$outlier_var)
      var_nm <- input$outlier_var
      
      if (exists("detect_outliers", where = asNamespace("analytix"))) {
        sym_var <- rlang::sym(var_nm)
        res <- tryCatch(analytix::detect_outliers(df, var = !!sym_var), error = function(e) NULL)
        if (!is.null(res) && !is.null(res$summary)) {
          return(flextable::htmltools_value(res$summary))
        }
      }
      
      # Fallback outlier calculation
      vec <- df[[var_nm]]
      q1 <- quantile(vec, 0.25, na.rm = TRUE)
      q3 <- quantile(vec, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      outliers <- vec[vec < (q1 - 1.5 * iqr) | vec > (q3 + 1.5 * iqr)]
      outliers <- na.omit(outliers)
      
      data.frame(
        Indicateur = c("Variable", "Effectif total", "Nb d'Outliers (IQR x 1.5)", "Valeurs detectees"),
        Valeur = c(var_nm, length(vec), length(outliers), paste(head(outliers, 5), collapse = ", "))
      ) %>% tableOutput()
    })
    
    # 8. Missing Summary
    output$missing_summary_ui <- renderUI({
      df <- cleaned_data()
      req(df)
      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          bslib::card_header(tags$div(class = "fw-bold", icon("table", class = "me-2"), "Tableau des Valeurs Manquantes")),
          tableOutput(ns("missing_table"))
        ),
        bslib::card(
          bslib::card_header(tags$div(class = "fw-bold", icon("image", class = "me-2"), "Carte Visuelle des Manquants")),
          plotOutput(ns("missing_map_plot"), height = "450px")
        )
      )
    })

    output$missing_table <- renderTable({
      df <- cleaned_data()
      req(df)
      na_counts <- sapply(df, function(x) sum(is.na(x)))
      na_pct <- round((na_counts / nrow(df)) * 100, 1)
      na_df <- data.frame(`Variable` = names(df), `Nombre de NA` = na_counts, `Proportion` = paste0(na_pct, " %"), check.names = FALSE)
      na_df[order(-na_counts), ]
    }, striped = TRUE, hover = TRUE)

    output$missing_map_plot <- renderPlot({
      df <- cleaned_data()
      req(df)
      if (exists("plot_missing_map", where = asNamespace("analytix"))) {
        analytix::plot_missing_map(df)
      } else {
        ggplot2::ggplot() +
          ggplot2::labs(title = "Graphique non disponible") +
          ggplot2::theme_void()
      }
    })
    
    # Download cleaned CSV handler
    output$download_cleaned_csv <- downloadHandler(
      filename = function() { paste0("donnees_nettoyees_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(cleaned_data(), file, row.names = FALSE)
      }
    )
    
    return(cleaned_data)
  })
}
