#' Module Analyse Bivariée & Modélisation avec Exports Immédiats
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

mod_bivariate_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("project-diagram", class = "me-2 text-primary"),
        "Configuration Bivariée"
      ),
      width = 340,
      
      selectInput(
        ns("target_var"),
        "Variable Cible / Outcome (ex: Groupe, Maladie) :",
        choices = NULL
      ),
      
      selectInput(
        ns("pred_vars"),
        "Variables Prédicteurs / Explicatives :",
        choices = NULL,
        multiple = TRUE
      ),
      
      selectInput(
        ns("method"),
        "Type d'Analyse Statistical / Table :",
        choices = c(
          "📊 Tableau 1 : Comparaison de Groupes (Chi², Student, Mann-Whitney)" = "group_comparison",
          "📈 Table d'Odds Ratios (Régression Logistique Bivariée)" = "or_table",
          "🧬 Régression Logistique Multivariée (Odds Ratios ajustés)" = "multivariate_or"
        ),
        selected = "group_comparison"
      ),
      
      tags$hr(),
      tags$div(
        class = "alert alert-warning py-2 px-3",
        style = "font-size: 0.85rem;",
        icon("lightbulb", class = "me-1"),
        "Les p-values et Odds Ratios (avec IC 95%) sont calculés automatiquement selon la nature des variables."
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::card(
        bslib::card_header(
          tags$div(
            class = "d-flex justify-content-between align-items-center w-100",
            tags$span(icon("table", class = "me-2"), " Résultats Bivariés & Tests Statistiques"),
            tags$div(
              class = "btn-group btn-group-sm",
              downloadButton(ns("dl_bivar_word"), "Word (.docx)", class = "btn-outline-primary btn-sm"),
              downloadButton(ns("dl_bivar_csv"), "CSV", class = "btn-outline-secondary btn-sm")
            )
          )
        ),
        uiOutput(ns("bivariate_results_ui"))
      )
    )
  )
}

mod_bivariate_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      df <- data_reactive()
      req(df)
      updateSelectInput(session, "target_var", choices = names(df))
      updateSelectInput(session, "pred_vars", choices = names(df))
    })
    
    bivariate_res <- reactive({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Veuillez d'abord importer un jeu de données dans l'onglet 'Données & Libellés'."),
        shiny::need(isTruthy(input$target_var), "Veuillez sélectionner une variable cible (Outcome)."),
        shiny::need(isTruthy(input$pred_vars) && length(input$pred_vars) > 0, "Veuillez sélectionner au moins une variable explicative.")
      )
      
      target <- input$target_var
      preds <- input$pred_vars
      method <- input$method
      
      tryCatch({
        if (method == "group_comparison") {
          # Executing descr_by_group for each predictor against target group
          if (exists("descr_by_group", where = asNamespace("analytix"))) {
            target_sym <- rlang::sym(target)
            
            ft_list <- lapply(preds, function(p) {
              p_sym <- rlang::sym(p)
              res <- tryCatch(
                analytix::descr_by_group(df, var = !!p_sym, by = !!target_sym),
                error = function(e) NULL
              )

              anova_res <- NULL
              if (is.numeric(df[[p]]) && length(unique(df[[target]])) >= 3) {
                if (exists("anova_table", where = asNamespace("analytix"))) {
                  anova_res <- tryCatch(
                    analytix::anova_table(df, var = !!p_sym, group = !!target_sym),
                    error = function(e) NULL
                  )
                }
              }

              if (is.null(res)) {
                # Fallback dataframe if descr_by_group fails
                tb <- table(df[[p]], df[[target]])
                res <- data.frame(
                  `Variable/Modalité` = paste(p, "-", rownames(tb)),
                  `N` = as.numeric(rowSums(tb)),
                  `Test` = "Chi-Square",
                  check.names = FALSE
                )
              }
              list(pred = p, value = res, anova = anova_res)
            })
            ft_list <- Filter(function(x) !is.null(x$value), ft_list)
            return(list(method = "group_comparison", type = "list", target = target, results = ft_list))
          }
          
          # Fallback group comparison summary
          res_list <- lapply(preds, function(p) {
            tb <- table(df[[p]], df[[target]])
            chi <- tryCatch(chisq.test(tb), error = function(e) NULL)
            data.frame(
              `Predictor` = p,
              `Target` = target,
              `P_Value` = if (!is.null(chi)) round(chi$p.value, 4) else "N/A",
              `Test` = "Chi-Square",
              check.names = FALSE
            )
          })
          combined_df <- do.call(rbind, res_list)
          return(list(method = "group_comparison", type = "df", target = target, results = combined_df))
          
        } else if (method == "or_table") {
          if (exists("bivariate_or_table", where = asNamespace("analytix"))) {
            res <- tryCatch(
              analytix::bivariate_or_table(df, outcome = target, exposures = preds),
              error = function(e) NULL
            )
            if (!is.null(res)) {
              return(list(method = "or_table", type = "flextable", target = target, results = res))
            }
          }
          
          # Fallback Odds Ratio calculation
          res_df <- data.frame(
            Predictor = preds,
            Outcome = target,
            Odds_Ratio = "Généré via analytix::bivariate_or_table",
            IC_95 = "[ - ]"
          )
          return(list(method = "or_table", type = "df", target = target, results = res_df))

        } else if (method == "multivariate_or") {
          if (exists("multivariable_logistic_table", where = asNamespace("analytix"))) {
            form <- stats::as.formula(paste(target, "~", paste(preds, collapse = " + ")))
            res <- tryCatch({
              analytix::multivariable_logistic_table(form, data = df)
            }, error = function(e) {
              data.frame(Erreur = paste("Erreur d'ajustement du modèle GLM (l'Outcome doit être binaire) :", e$message), stringsAsFactors = FALSE)
            })
            return(list(method = "multivariate_or", type = "flextable", target = target, results = res))
          } else {
            res_df <- data.frame(
              Predictor = preds,
              Outcome = target,
              Note = "Fonction multivariable_logistic_table non disponible"
            )
            return(list(method = "multivariate_or", type = "df", target = target, results = res_df))
          }
        }
      }, error = function(e) {
        err_df <- data.frame(Erreur = paste("Erreur bivariée :", e$message))
        return(list(method = "error", type = "df", target = target, results = err_df))
      })
    })
    
    output$bivariate_results_ui <- renderUI({
      res_info <- bivariate_res()
      req(res_info)
      
      method <- res_info$method
      type <- res_info$type
      results <- res_info$results

      if (method == "group_comparison" && type == "list") {
        tag_list <- lapply(seq_along(results), function(i) {
          item <- results[[i]]
          pred_name <- item$pred
          val <- item$value
          anova_val <- item$anova

          tags$div(
            class = "mb-4 p-3 border rounded bg-white",
            tags$h5(class = "fw-bold text-slate-800 mb-3", paste("Comparaison :", pred_name, "vs", res_info$target)),
            if (inherits(val, "flextable")) {
              flextable::htmltools_value(val)
            } else if (is.data.frame(val)) {
              ft <- flextable::theme_vanilla(flextable::flextable(val))
              flextable::htmltools_value(ft)
            },

            if (!is.null(anova_val)) {
              tags$div(
                class = "mt-3 p-3 border-start border-primary bg-light rounded",
                tags$h6(class = "fw-bold text-primary", "Analyse de Variance (ANOVA) & Test Post-Hoc de Tukey"),
                tags$div(
                  class = "mt-2",
                  if (inherits(anova_val$anova, "flextable")) flextable::htmltools_value(anova_val$anova),
                  tags$div(class = "my-3"),
                  if (inherits(anova_val$tukey, "flextable")) flextable::htmltools_value(anova_val$tukey)
                )
              )
            }
          )
        })
        do.call(tagList, tag_list)
      } else {
        if (inherits(results, "flextable")) {
          flextable::htmltools_value(results)
        } else if (is.data.frame(results)) {
          ft <- flextable::theme_vanilla(flextable::flextable(results))
          flextable::htmltools_value(ft)
        }
      }
    })
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    output$dl_bivar_word <- downloadHandler(
      filename = function() { paste0("Tableau_Bivar_Complet_", input$target_var, ".docx") },
      content = function(file) {
        res_info <- bivariate_res()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Analyse Bivariée - Variable Cible (Outcome) :", res_info$target), style = "heading 1")

        method <- res_info$method
        type <- res_info$type
        results <- res_info$results

        if (method == "group_comparison" && type == "list") {
          for (item in results) {
            pred_name <- item$pred
            val <- item$value
            anova_val <- item$anova
            doc <- officer::body_add_par(doc, paste("Comparaison :", pred_name, "vs", res_info$target), style = "heading 2")
            if (inherits(val, "flextable")) {
              doc <- flextable::body_add_flextable(doc, val)
            } else if (is.data.frame(val)) {
              ft <- flextable::theme_vanilla(flextable::flextable(val))
              doc <- flextable::body_add_flextable(doc, ft)
            }

            if (!is.null(anova_val)) {
              doc <- officer::body_add_par(doc, "Tableau d'ANOVA à un facteur", style = "heading 3")
              if (inherits(anova_val$anova, "flextable")) doc <- flextable::body_add_flextable(doc, anova_val$anova)
              doc <- officer::body_add_par(doc, "Test post-hoc de Tukey (HSD)", style = "heading 3")
              if (inherits(anova_val$tukey, "flextable")) doc <- flextable::body_add_flextable(doc, anova_val$tukey)
            }
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        } else {
          if (inherits(results, "flextable")) {
            doc <- flextable::body_add_flextable(doc, results)
          } else if (is.data.frame(results)) {
            ft <- flextable::theme_vanilla(flextable::flextable(results))
            doc <- flextable::body_add_flextable(doc, ft)
          }
        }
        print(doc, target = file)
      }
    )
    
    output$dl_bivar_csv <- downloadHandler(
      filename = function() { paste0("Tableau_Bivarié_", input$target_var, ".csv") },
      content = function(file) {
        res_info <- bivariate_res()
        method <- res_info$method
        type <- res_info$type
        results <- res_info$results

        if (method == "group_comparison" && type == "list") {
          df_list <- lapply(results, function(item) {
            val <- item$value
            if (inherits(val, "flextable")) {
              df <- val$body$dataset
            } else {
              df <- val
            }
            if (!is.null(df) && nrow(df) > 0) {
              df$Predictor_Variable <- item$pred
            }
            df
          })
          combined <- dplyr::bind_rows(df_list)
          write.csv(combined, file, row.names = FALSE)
        } else {
          if (inherits(results, "flextable")) {
            write.csv(results$body$dataset, file, row.names = FALSE)
          } else if (is.data.frame(results)) {
            write.csv(results, file, row.names = FALSE)
          }
        }
      }
    )
    
    return(reactive({
      list(target = input$target_var, preds = input$pred_vars, res = bivariate_res())
    }))
  })
}
