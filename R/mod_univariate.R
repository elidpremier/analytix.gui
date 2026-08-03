#' Module Analyse Univariée
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
      
      tags$hr(),
      tags$div(
        class = "alert alert-info py-2 px-3",
        style = "font-size: 0.85rem;",
        icon("info-circle", class = "me-1"),
        "Astuce : le package analytix adapte automatiquement les statistiques (Moyenne ± SD vs Médiane [IQR]) selon la distribution."
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::layout_column_wrap(
        width = 1/2,
        
        # Carte Tableau
        bslib::card(
          bslib::card_header(
            tags$div(
              icon("table", class = "me-2"), " Tableau Descriptif Normé (analytix)"
            )
          ),
          uiOutput(ns("univariate_table_ui"))
        ),
        
        # Carte Graphique
        bslib::card(
          bslib::card_header(
            tags$div(
              icon("image", class = "me-2"), " Visualisation Graphique"
            )
          ),
          plotOutput(ns("univariate_plot"), height = "420px")
        )
      )
    )
  )
}

mod_univariate_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Update choices on data change
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "select_var", choices = names(df))
    })
    
    # Determine type
    effective_type <- reactive({
      df <- data_reactive()
      req(df, input$select_var)
      
      var_name <- input$select_var
      col <- df[[var_name]]
      
      if (input$analysis_type != "auto") {
        return(input$analysis_type)
      }
      
      # Auto detection logic
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
      req(df, input$select_var)
      
      var_name <- input$select_var
      type <- effective_type()
      na.rm <- !isTRUE(input$include_na)
      
      tryCatch({
        if (type == "numeric" && exists("descr_numeric", where = asNamespace("analytix"))) {
          analytix::descr_numeric(df, var_name)
        } else if (type == "categorical" && exists("descr_categorial", where = asNamespace("analytix"))) {
          analytix::descr_categorial(df, var_name, na.rm = na.rm)
        } else if (type == "binary" && exists("descr_binary", where = asNamespace("analytix"))) {
          analytix::descr_binary(df, var_name)
        } else if (type == "likert" && exists("descr_likert", where = asNamespace("analytix"))) {
          analytix::descr_likert(df, var_name)
        } else {
          # Fallback standard summary dataframe if function not available
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
    
    # Render Table UI (Flextable HTML or table)
    output$univariate_table_ui <- renderUI({
      res <- res_table()
      req(res)
      
      if (inherits(res, "flextable")) {
        htmltools::HTML(flextable::htmltools_value(res))
      } else if (is.data.frame(res)) {
        tableOutput(ns("raw_summary_table"))
      }
    })
    
    output$raw_summary_table <- renderTable({
      res <- res_table()
      req(is.data.frame(res))
      res
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # 2. Render Plot
    output$univariate_plot <- renderPlot({
      df <- data_reactive()
      req(df, input$select_var)
      
      var_name <- input$select_var
      type <- effective_type()
      
      # Try analytix plot_distribution if available
      if (exists("plot_distribution", where = asNamespace("analytix"))) {
        p <- try(analytix::plot_distribution(df, var_name), silent = TRUE)
        if (!inherits(p, "try-error") && inherits(p, "ggplot")) {
          return(p)
        }
      }
      
      # Custom fallback ggplot2
      col <- df[[var_name]]
      if (type == "numeric") {
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_name]])) +
          ggplot2::geom_histogram(fill = "#0284c7", color = "white", bins = 20, alpha = 0.85) +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = paste("Distribution de", var_name),
            x = var_name, y = "Fréquence"
          )
      } else {
        ggplot2::ggplot(df, ggplot2::aes(x = factor(.data[[var_name]]), fill = factor(.data[[var_name]]))) +
          ggplot2::geom_bar(alpha = 0.85, show.legend = FALSE) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(
            title = paste("Répartition des modalités de", var_name),
            x = var_name, y = "Effectif"
          ) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      }
    })
    
    return(reactive({
      list(
        var = input$select_var,
        type = effective_type(),
        table = res_table()
      )
    }))
  })
}
