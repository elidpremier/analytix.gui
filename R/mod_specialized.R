#' Module Analyses Spécialisées (Likert, Choix Multiples, Heatmap)
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

mod_specialized_ui <- function(id) {
  ns <- NS(id)
  
  bslib::navset_card_tab(
    # Tab 1: Likert Scale
    bslib::nav_panel(
      title = tags$span(icon("star"), " Échelles de Likert"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Configuration Likert",
          selectInput(ns("likert_var"), "Variable Likert :", choices = NULL),
          width = 300
        ),
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("table", class = "me-2"), " Résumé Échelle de Likert"),
              downloadButton(ns("dl_likert_word"), "Word (.docx)", class = "btn-outline-primary btn-sm")
            )
          ),
          uiOutput(ns("likert_ui")),
          tags$div(class = "my-2"),
          plotOutput(ns("likert_plot"), height = "400px")
        )
      )
    ),
    
    # Tab 2: Multiple Choice
    bslib::nav_panel(
      title = tags$span(icon("list-check"), " Choix Multiples"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Questions Multiples",
          selectInput(ns("multi_vars"), "Colonnes composant la question :", choices = NULL, multiple = TRUE),
          width = 300
        ),
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("table", class = "me-2"), " Analyse des Réponses Multiples"),
              downloadButton(ns("dl_multi_word"), "Word (.docx)", class = "btn-outline-primary btn-sm")
            )
          ),
          uiOutput(ns("multi_ui"))
        )
      )
    ),
    
    # Tab 3: Heatmap Correlation Matrix
    bslib::nav_panel(
      title = tags$span(icon("th"), " Heatmap & Corrélations"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Variables Numériques",
          selectInput(ns("heatmap_vars"), "Sélectionner les variables (min 2) :", choices = NULL, multiple = TRUE),
          width = 300
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("image", class = "me-2"), " Carte Thermique des Corrélations"),
                tags$div(
                  class = "btn-group btn-group-sm",
                  downloadButton(ns("dl_hm_png"), "PNG", class = "btn-outline-success btn-sm"),
                  downloadButton(ns("dl_hm_pdf"), "PDF", class = "btn-outline-danger btn-sm")
                )
              )
            ),
            plotOutput(ns("heatmap_plot"), height = "450px")
          ),
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Matrice de Corrélations (Coefficients)"),
                downloadButton(ns("dl_cor_word"), "Word (.docx)", class = "btn-outline-primary btn-sm")
              )
            ),
            uiOutput(ns("correlation_table_ui"))
          )
        )
      )
    ),

    # Tab 4: Diagnostic Performance
    bslib::nav_panel(
      title = tags$span(icon("stethoscope"), " Performance Diagnostique"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Indicateurs Diagnostiques",
          selectInput(ns("diag_actual"), "Standard de référence / Gold Standard :", choices = NULL),
          selectInput(ns("diag_predicted"), "Test diagnostique à évaluer :", choices = NULL),
          uiOutput(ns("diag_pos_ui")),
          width = 320
        ),
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("table", class = "me-2"), " Indicateurs diagnostiques (Sensibilité, Spécificité, VPP, VPN)"),
              tags$div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("dl_diag_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                downloadButton(ns("dl_diag_csv"), "CSV", class = "btn-outline-secondary btn-sm")
              )
            )
          ),
          uiOutput(ns("diagnostic_results_ui"))
        )
      )
    )
  )
}

mod_specialized_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "likert_var", choices = names(df))
      updateSelectInput(session, "multi_vars", choices = names(df))
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "heatmap_vars", choices = num_cols, selected = head(num_cols, 4))
      updateSelectInput(session, "diag_actual", choices = names(df))
      updateSelectInput(session, "diag_predicted", choices = names(df))
    })

    output$diag_pos_ui <- renderUI({
      df <- data_reactive()
      req(df, input$diag_actual)
      vals <- unique(df[[input$diag_actual]])
      vals <- vals[!is.na(vals)]
      selectInput(ns("diag_pos_val"), "Valeur du cas positif :", choices = vals)
    })
    
    # 1. Likert logic
    likert_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$likert_var), "Veuillez sélectionner une variable Likert.")
      )
      var_nm <- input$likert_var
      sym_v <- rlang::sym(var_nm)
      
      if (exists("descr_likert", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::descr_likert(df, var = !!sym_v), error = function(e) NULL)
        if (!is.null(res)) return(res)
      }
      
      # Fallback Likert summary
      tb <- table(df[[var_nm]], useNA = "no")
      data.frame(Modalite = names(tb), Effectif = as.numeric(tb), Pourcentage = paste0(round(prop.table(tb)*100, 1), "%"))
    })
    
    output$likert_ui <- renderUI({
      res <- likert_res()
      req(res)
      if (inherits(res, "flextable")) {
        flextable::htmltools_value(res)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_likert"))
      }
    })
    output$raw_likert <- renderTable({ likert_res() }, striped = TRUE, hover = TRUE)

    likert_plot_res <- reactive({
      df <- data_reactive()
      req(df, input$likert_var)

      vec <- df[[input$likert_var]]
      if (!is.numeric(vec)) {
        fact <- as.factor(vec)
        df_copy <- df
        df_copy[[input$likert_var]] <- as.numeric(fact)
        lvls <- levels(fact)
        n_lvls <- length(lvls)

        if (exists("plot_likert_divergent", where = asNamespace("analytix"))) {
          analytix::plot_likert_divergent(df_copy, cols = input$likert_var, n_levels = n_lvls, level_labels = lvls)
        } else {
          NULL
        }
      } else {
        n_lvls <- length(unique(na.omit(vec)))
        if (exists("plot_likert_divergent", where = asNamespace("analytix"))) {
          analytix::plot_likert_divergent(df, cols = input$likert_var, n_levels = max(c(5, n_lvls)))
        } else {
          NULL
        }
      }
    })

    output$likert_plot <- renderPlot({
      p <- likert_plot_res()
      shiny::validate(
        shiny::need(!is.null(p), "Le graphique de Likert divergent n'est pas disponible pour cette variable.")
      )
      p
    })
    
    # 2. Multi choice logic
    multi_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$multi_vars) && length(input$multi_vars) > 0, "Veuillez sélectionner des variables pour les choix multiples.")
      )
      vars <- input$multi_vars
      
      if (exists("descr_multi_choice", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::descr_multi_choice(df, vars = vars), error = function(e) NULL)
        if (!is.null(res)) return(res)
      }
      
      counts <- sapply(df[vars], function(x) sum(x == 1 | x == "Oui" | x == TRUE, na.rm = TRUE))
      data.frame(Option = vars, Citations = counts, `% Participants` = paste0(round(counts/nrow(df)*100, 1), "%"), check.names = FALSE)
    })
    
    output$multi_ui <- renderUI({
      res <- multi_res()
      req(res)
      if (inherits(res, "flextable")) {
        flextable::htmltools_value(res)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_multi"))
      }
    })
    output$raw_multi <- renderTable({ multi_res() }, striped = TRUE, hover = TRUE)
    
    # 3. Heatmap & Correlation logic
    current_hm_plot <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$heatmap_vars) && length(input$heatmap_vars) >= 2, "Veuillez sélectionner au moins 2 variables numériques pour la heatmap.")
      )
      vars <- input$heatmap_vars
      
      if (exists("plot_correlation", where = asNamespace("analytix"))) {
        p <- tryCatch(analytix::plot_correlation(df, cols = vars), error = function(e) NULL)
        if (!is.null(p) && inherits(p, "ggplot")) return(p)
      }

      if (exists("plot_heatmap_matrix", where = asNamespace("analytix"))) {
        p <- tryCatch(analytix::plot_heatmap_matrix(df, vars = vars), error = function(e) NULL)
        if (!is.null(p) && inherits(p, "ggplot")) return(p)
      }
      
      sub_df <- na.omit(df[vars])
      cm <- cor(sub_df)
      df_cm <- as.data.frame(as.table(cm))
      
      ggplot2::ggplot(df_cm, ggplot2::aes(x = Var1, y = Var2, fill = Freq)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient2(low = "#0284c7", high = "#e11d48", mid = "white", midpoint = 0, limit = c(-1,1)) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::labs(title = "Matrice de Corrélations", x = "", y = "")
    })
    
    output$heatmap_plot <- renderPlot({ current_hm_plot() })

    cor_table_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$heatmap_vars) && length(input$heatmap_vars) >= 2, "Veuillez sélectionner au moins 2 variables numériques.")
      )
      vars <- input$heatmap_vars

      if (exists("correlation_table", where = asNamespace("analytix"))) {
        res <- tryCatch(analytix::correlation_table(df, cols = vars), error = function(e) NULL)
        if (!is.null(res)) return(res)
      }

      cor_m <- cor(df[vars], use = "pairwise.complete.obs")
      as.data.frame(cor_m)
    })

    output$correlation_table_ui <- renderUI({
      res <- cor_table_res()
      req(res)
      if (inherits(res, "flextable")) {
        flextable::htmltools_value(res)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_cor_table"))
      }
    })
    output$raw_cor_table <- renderTable({ cor_table_res() }, rownames = TRUE, striped = TRUE, hover = TRUE)

    # 4. Diagnostic Performance logic
    diagnostic_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$diag_actual) && isTruthy(input$diag_predicted), "Veuillez sélectionner le Gold Standard et le Test à évaluer.")
      )

      act <- df[[input$diag_actual]]
      pred <- df[[input$diag_predicted]]
      pos_v <- input$diag_pos_val
      req(pos_v)

      if (exists("calc_sensitivity_specificity", where = asNamespace("analytix"))) {
        res <- tryCatch({
          analytix::calc_sensitivity_specificity(act, pred, positive_val = pos_v)
        }, error = function(e) {
          NULL
        })
        if (!is.null(res)) return(res)
      }

      tp <- sum(act == pos_v & pred == pos_v, na.rm = TRUE)
      fp <- sum(act != pos_v & pred == pos_v, na.rm = TRUE)
      fn <- sum(act == pos_v & pred != pos_v, na.rm = TRUE)
      tn <- sum(act != pos_v & pred != pos_v, na.rm = TRUE)
      data.frame(
        Indicateur = c("Vrais Positifs (VP)", "Faux Positifs (FP)", "Faux Négatifs (FN)", "Vrais Négatifs (VN)"),
        Valeur = c(tp, fp, fn, tn),
        stringsAsFactors = FALSE
      )
    })

    output$diagnostic_results_ui <- renderUI({
      res <- diagnostic_res()
      req(res)
      if (inherits(res, "flextable")) {
        flextable::htmltools_value(res)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_diagnostic_table"))
      }
    })
    output$raw_diagnostic_table <- renderTable({ diagnostic_res() }, striped = TRUE, hover = TRUE)
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    output$dl_likert_word <- downloadHandler(
      filename = function() { paste0("Likert_", input$likert_var, ".docx") },
      content = function(file) {
        doc <- officer::read_docx()
        res <- likert_res()
        if (inherits(res, "flextable")) doc <- flextable::body_add_flextable(doc, res)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )
    
    output$dl_multi_word <- downloadHandler(
      filename = function() { paste0("Choix_Multiples.docx") },
      content = function(file) {
        doc <- officer::read_docx()
        res <- multi_res()
        if (inherits(res, "flextable")) doc <- flextable::body_add_flextable(doc, res)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )
    
    output$dl_hm_png <- downloadHandler(
      filename = function() { "Heatmap_Correlations.png" },
      content = function(file) { ggplot2::ggsave(file, plot = current_hm_plot(), width = 8, height = 6, dpi = 300) }
    )
    
    output$dl_hm_pdf <- downloadHandler(
      filename = function() { "Heatmap_Correlations.pdf" },
      content = function(file) { ggplot2::ggsave(file, plot = current_hm_plot(), width = 8, height = 6) }
    )

    output$dl_cor_word <- downloadHandler(
      filename = function() { "Matrice_Correlations.docx" },
      content = function(file) {
        doc <- officer::read_docx()
        res <- cor_table_res()
        if (inherits(res, "flextable")) doc <- flextable::body_add_flextable(doc, res)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_diag_word <- downloadHandler(
      filename = function() { "Performance_Diagnostique.docx" },
      content = function(file) {
        doc <- officer::read_docx()
        res <- diagnostic_res()
        if (inherits(res, "flextable")) doc <- flextable::body_add_flextable(doc, res)
        else doc <- flextable::body_add_flextable(doc, flextable::flextable(as.data.frame(res)))
        print(doc, target = file)
      }
    )

    output$dl_diag_csv <- downloadHandler(
      filename = function() { "Performance_Diagnostique.csv" },
      content = function(file) {
        res <- diagnostic_res()
        if (inherits(res, "flextable")) {
          write.csv(res$body$dataset, file, row.names = FALSE)
        } else if (is.data.frame(res)) {
          write.csv(res, file, row.names = FALSE)
        }
      }
    )
    
    return(reactive({
      list(
        likert = likert_res(),
        multi = multi_res(),
        heatmap = current_hm_plot(),
        correlation_matrix = tryCatch(cor_table_res(), error = function(e) NULL),
        diagnostic = tryCatch(diagnostic_res(), error = function(e) NULL)
      )
    }))
  })
}
