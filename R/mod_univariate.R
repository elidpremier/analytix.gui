#' Module Analyse Univariée avec Exports Immédiats
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

mod_univariate_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("chart-bar", class = "me-2 text-primary"),
        "Configuration Univariée"
      ),
      width = 320,
      
      selectInput(
        ns("select_var"), 
        "Choisir la variable à analyser :",
        choices = NULL
      ),
      
      selectInput(
        ns("analysis_type"),
        "Type d'Analyse :",
        choices = c(
          "🤖 Détection Automatique" = "auto",
          "📊 Numérique Continue (Moyenne, Médiane, SD, IQR)" = "numeric",
          "🏷️ Catégorielle / Qualitative (Effectifs, %)" = "categorical",
          "✅ Binaire (Oui / Non, 1 / 0)" = "binary",
          "⭐ Échelle de Likert" = "likert"
        ),
        selected = "auto"
      ),
      
      tags$hr(),
      checkboxInput(ns("include_na"), "Inclure les valeurs manquantes (NA) dans les %", value = FALSE),
      
      conditionalPanel(
        condition = sprintf("input['%s'] == 'categorical' || input['%s'] == 'binary' || input['%s'] == 'auto'", ns("analysis_type"), ns("analysis_type"), ns("analysis_type")),
        tags$hr(),
        tags$h6(icon("chart-line", class = "me-1"), "Options de Prévalence"),
        uiOutput(ns("prev_cases_ui")),
        selectInput(ns("prev_method"), "Méthode d'IC :",
                    choices = c("Wilson" = "wilson", "Exact (Clopper-Pearson)" = "exact", "Asymptotic" = "asymptotic")),
        numericInput(ns("prev_conf"), "Niveau de confiance (%) :", value = 95, min = 50, max = 99)
      ),

      tags$hr(),
      tags$div(
        class = "alert alert-info py-2 px-3",
        style = "font-size: 0.85rem;",
        icon("info-circle", class = "me-1"),
        "Le package analytix adapte automatiquement les statistiques (Moyenne ± SD vs Médiane [IQR]) selon la distribution."
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::layout_column_wrap(
        width = 1/2,
        
        # Carte Tableau avec Export Immédiat
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("table", class = "me-2"), " Tableau Descriptif (analytix)"),
              tags$div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("dl_tab_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
                downloadButton(ns("dl_tab_csv"), "CSV", class = "btn-outline-secondary btn-sm")
              )
            )
          ),
          uiOutput(ns("univariate_table_ui"))
        ),
        
        # Carte Graphique avec Export Immédiat
        bslib::card(
          bslib::card_header(
            tags$div(
              class = "d-flex justify-content-between align-items-center w-100",
              tags$span(icon("image", class = "me-2"), " Graphique"),
              tags$div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("dl_plot_png"), "PNG", class = "btn-outline-success btn-sm"),
                downloadButton(ns("dl_plot_pdf"), "PDF", class = "btn-outline-danger btn-sm")
              )
            )
          ),
          plotOutput(ns("univariate_plot"), height = "420px")
        )
      ),

      tags$div(class = "mt-3"),
      uiOutput(ns("prevalence_card_ui"))
    )
  )
}

mod_univariate_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "select_var", choices = names(df))
    })

    output$prev_cases_ui <- renderUI({
      df <- data_reactive()
      req(df, input$select_var)
      vals <- unique(df[[input$select_var]])
      vals <- vals[!is.na(vals)]
      selectInput(ns("prev_case_val"), "Valeur d'intérêt (cas positif) :", choices = vals)
    })
    
    effective_type <- reactive({
      df <- data_reactive()
      req(df, input$select_var)
      
      var_name <- input$select_var
      col <- df[[var_name]]
      
      if (input$analysis_type != "auto") {
        return(input$analysis_type)
      }
      
      if (is.numeric(col)) {
        if (length(unique(na.omit(col))) <= 2) return("binary")
        return("numeric")
      } else if (is.factor(col) || is.character(col)) {
        if (length(unique(na.omit(col))) == 2) return("binary")
        return("categorical")
      } else {
        return("categorical")
      }
    })
    
    # 1. Compute summary table using analytix
    res_table <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$select_var), "Veuillez sélectionner une variable à analyser.")
      )
      
      var_name <- input$select_var
      type <- effective_type()
      na.rm <- !isTRUE(input$include_na)
      sym_var <- rlang::sym(var_name)
      
      tryCatch({
        if (type == "numeric" && exists("descr_numeric", where = asNamespace("analytix"))) {
          analytix::descr_numeric(df, var = !!sym_var)
        } else if (type == "categorical" && exists("descr_categorial", where = asNamespace("analytix"))) {
          analytix::descr_categorial(df, var = !!sym_var, na.rm = na.rm)
        } else if (type == "binary" && exists("descr_binary", where = asNamespace("analytix"))) {
          analytix::descr_binary(df, var = !!sym_var)
        } else if (type == "likert" && exists("descr_likert", where = asNamespace("analytix"))) {
          analytix::descr_likert(df, var = !!sym_var)
        } else {
          col <- df[[var_name]]
          if (is.numeric(col)) {
            data.frame(
              Statistique = c("Effectif (N)", "Moyenne", "Écart-Type", "Médiane", "IQR [Q1 - Q3]"),
              Valeur = c(
                sum(!is.na(col)),
                round(mean(col, na.rm = TRUE), 2),
                round(sd(col, na.rm = TRUE), 2),
                round(median(col, na.rm = TRUE), 2),
                sprintf("[%s - %s]", round(quantile(col, 0.25, na.rm = TRUE), 2), round(quantile(col, 0.75, na.rm = TRUE), 2))
              )
            )
          } else {
            tb <- table(col, useNA = if (na.rm) "no" else "ifany")
            data.frame(
              Modalité = names(tb),
              Effectif = as.numeric(tb),
              Pourcentage = paste0(round(prop.table(tb) * 100, 1), " %")
            )
          }
        }
      }, error = function(e) {
        data.frame(Erreur = paste("Erreur d'analyse :", e$message))
      })
    })
    
    output$univariate_table_ui <- renderUI({
      res <- res_table()
      req(res)
      if (inherits(res, "flextable")) {
        flextable::htmltools_value(res)
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_summary_table"))
      }
    })
    
    output$raw_summary_table <- renderTable({
      res <- res_table()
      req(is.data.frame(res))
      res
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 2. Prevalence logic
    prevalence_res <- reactive({
      df <- data_reactive()
      req(df, input$select_var)

      type <- effective_type()
      shiny::validate(
        shiny::need(type %in% c("categorical", "binary"), "Le calcul de prévalence est disponible uniquement pour les variables qualitatives/binaires.")
      )

      req(input$prev_case_val)
      var_sym <- rlang::sym(input$select_var)
      conf_val <- as.numeric(input$prev_conf) / 100

      tryCatch({
        if (exists("calc_prevalence", where = asNamespace("analytix"))) {
          analytix::calc_prevalence(df, var = !!var_sym, cases_val = input$prev_case_val,
                                    conf_level = conf_val, method = input$prev_method)
        } else {
          vec <- df[[input$select_var]]
          vec_clean <- vec[!is.na(vec)]
          total <- length(vec_clean)
          cases <- sum(vec_clean == input$prev_case_val)
          p <- cases / total
          pct <- p * 100
          data.frame(
            Variable = input$select_var,
            Cas = cases,
            Total = total,
            Proportion = p,
            Pourcentage = pct,
            IC_Inf = pct - 1.96 * sqrt(pct * (100 - pct) / total),
            IC_Sup = pct + 1.96 * sqrt(pct * (100 - pct) / total),
            Formate = sprintf("%d/%d (%.1f%%)", cases, total, pct),
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        data.frame(Erreur = paste("Erreur prévalence :", e$message), stringsAsFactors = FALSE)
      })
    })

    output$prevalence_card_ui <- renderUI({
      type <- tryCatch(effective_type(), error = function(e) NULL)
      if (is.null(type) || !type %in% c("categorical", "binary")) return(NULL)

      bslib::card(
        bslib::card_header(
          tags$div(
            class = "d-flex justify-content-between align-items-center w-100",
            tags$span(icon("chart-line", class = "me-2"), " Calcul de Prévalence / Proportion Scientifique"),
            tags$div(
              class = "btn-group btn-group-sm",
              downloadButton(ns("dl_prev_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
              downloadButton(ns("dl_prev_csv"), "CSV", class = "btn-outline-secondary btn-sm")
            )
          )
        ),
        uiOutput(ns("prevalence_table_ui"))
      )
    })

    output$prevalence_table_ui <- renderUI({
      res <- tryCatch(prevalence_res(), error = function(e) NULL)
      req(res)
      if (is.data.frame(res)) {
        if (exists("theme_analytique", where = asNamespace("analytix"))) {
          ft <- flextable::flextable(res)
          ft <- analytix::theme_analytique(ft)
          flextable::htmltools_value(ft)
        } else {
          tableOutput(ns("raw_prevalence_table"))
        }
      }
    })

    output$raw_prevalence_table <- renderTable({
      prevalence_res()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # 3. Render Plot
    current_plot <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$select_var), "Veuillez sélectionner une variable à analyser.")
      )
      
      var_name <- input$select_var
      type <- effective_type()
      sym_var <- rlang::sym(var_name)
      
      if (exists("plot_distribution", where = asNamespace("analytix"))) {
        p <- try(analytix::plot_distribution(df, var = !!sym_var), silent = TRUE)
        if (!inherits(p, "try-error") && inherits(p, "ggplot")) {
          return(p)
        }
      }
      
      col <- df[[var_name]]
      if (type == "numeric") {
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +
          ggplot2::geom_histogram(fill = "#0284c7", color = "white", bins = 20, alpha = 0.85) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(title = paste("Distribution de", var_name), x = var_name, y = "Fréquence")
      } else {
        ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[var_name]]), fill = factor(.data[[var_name]]))) +
          ggplot2::geom_bar(alpha = 0.85, show.legend = FALSE) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(title = paste("Répartition de", var_name), x = var_name, y = "Effectif") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }
    })
    
    output$univariate_plot <- renderPlot({
      current_plot()
    })
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    # Prevalence Word
    output$dl_prev_word <- downloadHandler(
      filename = function() { paste0("Prevalence_", input$select_var, ".docx") },
      content = function(file) {
        res <- prevalence_res()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Calcul de Prévalence :", input$select_var), style = "heading 1")
        if (is.data.frame(res)) {
          ft <- flextable::flextable(res)
          if (exists("theme_analytique", where = asNamespace("analytix"))) {
            ft <- analytix::theme_analytique(ft)
          } else {
            ft <- flextable::theme_vanilla(ft)
          }
          doc <- flextable::body_add_flextable(doc, ft)
        }
        print(doc, target = file)
      }
    )

    # Prevalence CSV
    output$dl_prev_csv <- downloadHandler(
      filename = function() { paste0("Prevalence_", input$select_var, ".csv") },
      content = function(file) {
        res <- prevalence_res()
        if (is.data.frame(res)) {
          write.csv(res, file, row.names = FALSE)
        }
      }
    )

    # Word Table
    output$dl_tab_word <- downloadHandler(
      filename = function() { paste0("Tableau_Univ_", input$select_var, ".docx") },
      content = function(file) {
        res <- res_table()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Tableau Univarié :", input$select_var), style = "heading 1")
        if (inherits(res, "flextable")) {
          doc <- flextable::body_add_flextable(doc, res)
        } else if (is.data.frame(res)) {
          ft <- flextable::theme_vanilla(flextable::flextable(res))
          doc <- flextable::body_add_flextable(doc, ft)
        }
        print(doc, target = file)
      }
    )
    
    # CSV Table
    output$dl_tab_csv <- downloadHandler(
      filename = function() { paste0("Tableau_Univ_", input$select_var, ".csv") },
      content = function(file) {
        res <- res_table()
        if (inherits(res, "flextable")) {
          write.csv(res$body$dataset, file, row.names = FALSE)
        } else if (is.data.frame(res)) {
          write.csv(res, file, row.names = FALSE)
        }
      }
    )
    
    # PNG Plot
    output$dl_plot_png <- downloadHandler(
      filename = function() { paste0("Graphique_Univ_", input$select_var, ".png") },
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(), width = 8, height = 6, dpi = 300)
      }
    )
    
    # PDF Plot
    output$dl_plot_pdf <- downloadHandler(
      filename = function() { paste0("Graphique_Univ_", input$select_var, ".pdf") },
      content = function(file) {
        ggplot2::ggsave(file, plot = current_plot(), width = 8, height = 6)
      }
    )
    
    return(reactive({
      list(
        var = input$select_var,
        type = effective_type(),
        table = res_table(),
        plot = current_plot(),
        prevalence = tryCatch(prevalence_res(), error = function(e) NULL)
      )
    }))
  })
}
