#' Module Analyse Univariée avec Exports Immédiats
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

# Helpers locally defined to avoid scoping errors
get_flextable <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (inherits(obj, "flextable")) {
    return(obj)
  }
  if (is.list(obj) && !is.null(obj$flextable) && inherits(obj$flextable, "flextable")) {
    return(obj$flextable)
  }
  return(NULL)
}

get_dataframe <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (is.data.frame(obj)) {
    return(obj)
  }
  if (is.list(obj) && !is.null(obj$data) && is.data.frame(obj$data)) {
    return(obj$data)
  }
  if (is.list(obj) && !is.null(obj$body$dataset) && is.data.frame(obj$body$dataset)) {
    return(obj$body$dataset)
  }
  return(NULL)
}

mod_univariate_ui <- function(id) {
  ns <- NS(id)
  
  bslib::navset_card_tab(
    # Tab 1: Analyse Individuelle par Variable
    bslib::nav_panel(
      title = tags$span(icon("chart-bar"), " Analyse par Variable"),
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
          tags$h6(icon("sliders-h", class = "me-1"), "Options de Tableaux"),
          numericInput(ns("digits"), "Nombre de décimales :", value = 2, min = 0, max = 6),
          checkboxInput(ns("show_valid"), "Afficher l'effectif valide", value = FALSE),
          checkboxInput(ns("show_skewness"), "Afficher l'asymétrie (skewness)", value = FALSE),
          textInput(ns("header_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7"),

          tags$hr(),
          tags$h6(icon("image", class = "me-1"), "Options Graphiques"),
          selectInput(
            ns("plot_type"),
            "Type de graphique :",
            choices = c(
              "🤖 Détection Automatique" = "auto",
              "📊 Histogramme (Numérique)" = "histogram",
              "📈 Courbe de Densité (Numérique)" = "density",
              "📦 Boîte à Moustaches (Boxplot)" = "boxplot",
              "📶 Diagramme en Barres (Qualitatif)" = "bar",
              "🍕 Camembert" = "pie"
            ),
            selected = "auto"
          ),
          textInput(ns("plot_title"), "Titre personnalisé :", value = ""),
          textInput(ns("plot_color"), "Couleur principale (Hex) :", value = "#3366CC"),
          selectInput(
            ns("plot_theme"),
            "Thème du graphique :",
            choices = c(
              "Minimaliste" = "minimal",
              "Classique" = "classic",
              "Clair" = "light"
            ),
            selected = "minimal"
          ),
          checkboxInput(ns("plot_horiz"), "Orientation Horizontale (Barres)", value = FALSE),
          checkboxInput(ns("plot_labels"), "Afficher les étiquettes (%)", value = TRUE),

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
    ),
    
    # Tab 2: Tableau Descriptif Global
    bslib::nav_panel(
      title = tags$span(icon("table"), " Tableau Descriptif Global"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = tags$div(
            icon("table", class = "me-2 text-primary"),
            "Options Tableau Global"
          ),
          width = 320,
          selectInput(
            ns("global_vars"),
            "Sélectionner les variables à inclure :",
            choices = NULL,
            multiple = TRUE
          ),
          checkboxInput(ns("global_integer_cat"), "Traiter entiers avec peu de modalités comme qualitatifs", value = TRUE),
          checkboxInput(ns("global_include_na"), "Inclure les valeurs manquantes (NA) dans les %", value = FALSE),
          numericInput(ns("global_digits"), "Nombre de décimales :", value = 2, min = 0, max = 6),
          textInput(ns("global_header_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7"),
          tags$hr(),
          tags$div(
            class = "btn-group btn-group-sm w-100",
            downloadButton(ns("dl_global_word"), "Word (.docx)", class = "btn-success btn-sm w-50"),
            downloadButton(ns("dl_global_csv"), "CSV complet", class = "btn-secondary btn-sm w-50")
          )
        ),
        
        tags$div(
          class = "container-fluid p-0",
          bslib::card(
            bslib::card_header(
              tags$div(
                class = "d-flex justify-content-between align-items-center w-100",
                tags$span(icon("table", class = "me-2"), " Synthèse Descriptive Globale"),
                tags$span(class = "badge bg-primary", "analytix::analyse_descriptive_multiple")
              )
            ),
            uiOutput(ns("global_descriptive_ui"))
          )
        )
      )
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
      updateSelectInput(session, "global_vars", choices = names(df), selected = names(df))
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
      include_na <- isTRUE(input$include_na)
      digits <- as.integer(input$digits)
      show_valid <- isTRUE(input$show_valid)
      show_skewness <- isTRUE(input$show_skewness)
      color <- input$header_color
      if (is.null(color) || nchar(trimws(color)) == 0) color <- "#0284c7"

      sym_var <- rlang::sym(var_name)
      
      tryCatch({
        if (type == "numeric" && exists("descr_numeric", where = asNamespace("analytix"))) {
          analytix::descr_numeric(df, var = !!sym_var, digits = digits, show_valid = show_valid, show_skewness = show_skewness, color = color)
        } else if (type == "categorical" && exists("descr_categorial", where = asNamespace("analytix"))) {
          analytix::descr_categorial(df, var = !!sym_var, include_na = include_na, digits = digits, color = color)
        } else if (type == "binary" && exists("descr_binary", where = asNamespace("analytix"))) {
          analytix::descr_binary(df, var = !!sym_var, digits = digits, color = color)
        } else if (type == "likert" && exists("descr_likert", where = asNamespace("analytix"))) {
          analytix::descr_likert(df, var = !!sym_var, color = color)
        } else {
          col <- df[[var_name]]
          if (is.numeric(col)) {
            data.frame(
              Statistique = c("Effectif (N)", "Moyenne", "Écart-Type", "Médiane", "IQR [Q1 - Q3]"),
              Valeur = c(
                sum(!is.na(col)),
                round(mean(col, na.rm = TRUE), digits),
                round(sd(col, na.rm = TRUE), digits),
                round(median(col, na.rm = TRUE), digits),
                sprintf("[%s - %s]", round(quantile(col, 0.25, na.rm = TRUE), digits), round(quantile(col, 0.75, na.rm = TRUE), digits))
              )
            )
          } else {
            tb <- table(col, useNA = if (include_na) "always" else "no")
            data.frame(
              Modalité = names(tb),
              Effectif = as.numeric(tb),
              Pourcentage = paste0(round(prop.table(tb) * 100, digits), " %")
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
      ft <- get_flextable(res)
      if (!is.null(ft)) {
        flextable::htmltools_value(ft)
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
      sym_var <- rlang::sym(var_name)
      col_data <- df[[var_name]]
      
      # Determine requested plot type
      requested_type <- input$plot_type
      if (requested_type == "auto") {
        if (is.numeric(col_data)) {
          unique_vals <- length(unique(stats::na.omit(col_data)))
          requested_type <- if (unique_vals <= 15) "bar" else "histogram"
        } else {
          requested_type <- "bar"
        }
      }
      
      # Graph Title
      custom_title <- input$plot_title
      if (is.null(custom_title) || nchar(trimws(custom_title)) == 0) {
        custom_title <- paste("Distribution de :", var_name)
      }

      # Plot Color
      plot_color <- input$plot_color
      if (is.null(plot_color) || nchar(trimws(plot_color)) == 0) {
        plot_color <- "#3366CC"
      }

      # Plot Theme
      plot_theme_name <- input$plot_theme
      thm <- switch(plot_theme_name,
                    "minimal" = ggplot2::theme_minimal(base_size = 14),
                    "classic" = ggplot2::theme_classic(base_size = 14),
                    "light" = ggplot2::theme_light(base_size = 14),
                    ggplot2::theme_minimal(base_size = 14))

      p <- NULL

      # 1. Pie Chart
      if (requested_type == "pie") {
        if (exists("plot_pie_chart", where = asNamespace("analytix"))) {
          p <- tryCatch({
            analytix::plot_pie_chart(df, x = !!sym_var, title = custom_title)
          }, error = function(e) NULL)
        }

        if (is.null(p)) {
          # Fallback pie chart
          t_val <- table(col_data, useNA = "no")
          df_plot <- data.frame(modalite = names(t_val), effectif = as.numeric(t_val), stringsAsFactors = FALSE)
          n_total <- sum(df_plot$effectif)
          df_plot$pct <- round(100 * df_plot$effectif / n_total, 1)
          df_plot$label <- ifelse(df_plot$pct >= 5, paste0(df_plot$modalite, "\n", format(df_plot$pct, nsmall = 1, decimal.mark = ","), "%"), "")

          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = "", y = effectif, fill = modalite)) +
            ggplot2::geom_col(width = 1, color = "white", linewidth = 0.5) +
            ggplot2::geom_text(
              ggplot2::aes(label = label),
              position = ggplot2::position_stack(vjust = 0.5),
              size = 3.8, color = "black", fontface = "bold"
            ) +
            ggplot2::coord_polar(theta = "y") +
            ggplot2::labs(title = custom_title, fill = var_name) +
            ggplot2::theme_void(base_size = 14) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
              legend.position = "right"
            ) +
            ggplot2::scale_fill_brewer(palette = "Set2")
        }
      }
      # 2. Bar chart / bar plot
      else if (requested_type == "bar") {
        if (exists("plot_barplot", where = asNamespace("analytix"))) {
          p <- tryCatch({
            # Try package barplot
            analytix::plot_barplot(df, x = !!sym_var, title = custom_title, col = plot_color, horiz = isTRUE(input$plot_horiz), show_labels = isTRUE(input$plot_labels))
          }, error = function(e) NULL)
        }

        if (is.null(p)) {
          t_val <- table(col_data, useNA = "no")
          df_plot <- data.frame(modalite = names(t_val), effectif = as.numeric(t_val), stringsAsFactors = FALSE)
          n_total <- sum(df_plot$effectif)
          df_plot$pct <- round(100 * df_plot$effectif / n_total, 1)
          df_plot$etiquette <- paste0(format(df_plot$pct, nsmall = 1, decimal.mark = ","), "%")

          if (isTRUE(input$plot_horiz)) {
            df_plot <- df_plot[order(df_plot$effectif), ]
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = effectif, y = stats::reorder(modalite, effectif))) +
              ggplot2::geom_col(fill = plot_color, width = 0.7) +
              ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
              ggplot2::labs(title = custom_title, subtitle = paste0("N = ", n_total), x = "Effectif", y = NULL)
            if (isTRUE(input$plot_labels)) {
              p <- p + ggplot2::geom_text(ggplot2::aes(label = etiquette), hjust = -0.2, size = 3.5, fontface = "bold")
            }
          } else {
            df_plot <- df_plot[order(df_plot$effectif, decreasing = TRUE), ]
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = stats::reorder(modalite, -effectif), y = effectif)) +
              ggplot2::geom_col(fill = plot_color, width = 0.7) +
              ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
              ggplot2::labs(title = custom_title, subtitle = paste0("N = ", n_total), x = NULL, y = "Effectif")
            if (isTRUE(input$plot_labels)) {
              p <- p + ggplot2::geom_text(ggplot2::aes(label = etiquette), vjust = -0.5, size = 3.5, fontface = "bold")
            }
          }
          p <- p + thm + ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
        }
      }
      # 3. Numeric plot types (Histogram, Density, Boxplot)
      else {
        if (exists("plot_distribution", where = asNamespace("analytix"))) {
          p <- tryCatch({
            analytix::plot_distribution(df, var = !!sym_var, type = requested_type, fill = plot_color, theme = plot_theme_name)
          }, error = function(e) NULL)
        }

        if (is.null(p) || inherits(p, "try-error")) {
          # Manual Fallback
          p <- ggplot2::ggplot(data.frame(val = col_data), ggplot2::aes(x = val))

          if (requested_type == "histogram") {
            p <- p + ggplot2::geom_histogram(fill = plot_color, color = "white", bins = 30, alpha = 0.8) +
              ggplot2::labs(y = "Effectif")
          } else if (requested_type == "density") {
            p <- p + ggplot2::geom_density(fill = plot_color, alpha = 0.5) +
              ggplot2::labs(y = "Densité")
          } else if (requested_type == "boxplot") {
            p <- ggplot2::ggplot(data.frame(val = col_data), ggplot2::aes(y = val)) +
              ggplot2::geom_boxplot(fill = plot_color, alpha = 0.8) +
              ggplot2::labs(x = "")
          }

          p <- p + thm +
            ggplot2::labs(title = custom_title, x = var_name) +
            ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
        } else {
          # Apply the custom title if retrieved from plot_distribution
          p <- p + ggplot2::labs(title = custom_title)
        }
      }

      return(p)
    })
    
    output$univariate_plot <- renderPlot({
      current_plot()
    })
    
    # --- Tableau Descriptif Global Server Logic ---
    global_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$global_vars), "Veuillez sélectionner au moins une variable.")
      )

      vars <- input$global_vars
      integer_as_cat <- isTRUE(input$global_integer_cat)
      include_na <- isTRUE(input$global_include_na)
      digits <- as.integer(input$global_digits)
      color <- input$global_header_color
      if (is.null(color) || nchar(trimws(color)) == 0) color <- "#0284c7"

      tryCatch({
        if (exists("analyse_descriptive_multiple", where = asNamespace("analytix"))) {
          analytix::analyse_descriptive_multiple(
            data = df,
            vars = vars,
            integer_as_category = integer_as_cat,
            digits = digits,
            color = color,
            include_na = include_na,
            na_rm = !include_na
          )
        } else {
          results <- list()
          for (v in vars) {
            x <- df[[v]]
            if (is.numeric(x)) {
              results[[v]] <- analytix::descr_numeric(df, var = !!rlang::sym(v), digits = digits, color = color)
            } else {
              results[[v]] <- analytix::descr_categorial(df, var = !!rlang::sym(v), digits = digits, color = color, include_na = include_na)
            }
          }
          results
        }
      }, error = function(e) {
        showNotification(paste("Erreur lors de la description globale :", e$message), type = "error")
        list()
      })
    })

    output$global_descriptive_ui <- renderUI({
      res_list <- global_res()
      shiny::validate(
        shiny::need(!is.null(res_list) && length(res_list) > 0, "Aucun tableau descriptif généré. Veuillez sélectionner des variables.")
      )

      tag_list <- lapply(names(res_list), function(var_name) {
        obj <- res_list[[var_name]]
        ft <- get_flextable(obj)

        tags$div(
          class = "mb-4 p-3 border rounded bg-white",
          tags$h5(class = "fw-bold text-slate-800 mb-2", paste("Description de :", var_name)),
          if (!is.null(ft)) {
            flextable::htmltools_value(ft)
          } else {
            tags$p(class = "text-danger", "Impossible de générer le tableau.")
          }
        )
      })
      do.call(tagList, tag_list)
    })

    # Download Global Word
    output$dl_global_word <- downloadHandler(
      filename = function() { paste0("Tableau_Descriptif_Global_", Sys.Date(), ".docx") },
      content = function(file) {
        res_list <- global_res()
        req(res_list)

        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, "Tableau Descriptif Global", style = "heading 1")

        for (var_name in names(res_list)) {
          obj <- res_list[[var_name]]
          ft <- get_flextable(obj)
          doc <- officer::body_add_par(doc, paste("Description de :", var_name), style = "heading 2")
          if (!is.null(ft)) {
            doc <- flextable::body_add_flextable(doc, ft)
          } else {
            doc <- officer::body_add_par(doc, "Tableau non disponible", style = "Normal")
          }
          doc <- officer::body_add_par(doc, "", style = "Normal")
        }
        print(doc, target = file)
      }
    )

    # Download Global CSV
    output$dl_global_csv <- downloadHandler(
      filename = function() { paste0("Tableau_Descriptif_Global_", Sys.Date(), ".csv") },
      content = function(file) {
        res_list <- global_res()
        req(res_list)

        df_list <- lapply(names(res_list), function(var_name) {
          obj <- res_list[[var_name]]
          df <- get_dataframe(obj)
          if (!is.null(df) && nrow(df) > 0) {
            df$Variable_Origine <- var_name
          }
          df
        })
        combined <- dplyr::bind_rows(df_list)
        write.csv(combined, file, row.names = FALSE)
      }
    )

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
        ft <- get_flextable(res)
        if (!is.null(ft)) {
          doc <- flextable::body_add_flextable(doc, ft)
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
        df <- get_dataframe(res)
        if (!is.null(df)) {
          write.csv(df, file, row.names = FALSE)
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
        prevalence = tryCatch(prevalence_res(), error = function(e) NULL),
        global_descriptive = tryCatch(global_res(), error = function(e) NULL)
      )
    }))
  })
}
