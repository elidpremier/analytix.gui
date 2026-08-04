#' Module Analyse Bivariée & Modélisation avec Exports Immédiats
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
      
      # Dynamic positive/interest value for the outcome
      uiOutput(ns("outcome_pos_ui")),

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
      tags$h6(icon("sliders-h", class = "me-1"), "Options d'Analyse"),
      numericInput(ns("bivar_digits"), "Nombre de décimales :", value = 2, min = 0, max = 6),
      numericInput(ns("bivar_conf"), "Niveau de confiance (%) :", value = 95, min = 50, max = 99),
      textInput(ns("bivar_header_color"), "Couleur de l'en-tête (Hex) :", value = "#0284c7"),

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

    # Render dynamic Outcome interest value selector
    output$outcome_pos_ui <- renderUI({
      df <- data_reactive()
      req(df, input$target_var)

      vals <- unique(df[[input$target_var]])
      vals <- vals[!is.na(vals)]

      # For bivarié, having a select input lets the user specify what is the "positive/interest" outcome value
      selectInput(
        ns("outcome_pos_val"),
        "Valeur de référence / Événement positif :",
        choices = vals,
        selected = if (length(vals) > 0) vals[1] else NULL
      )
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
      
      # Additional settings
      digits <- as.integer(input$bivar_digits)
      conf_level <- as.numeric(input$bivar_conf) / 100
      header_color <- input$bivar_header_color
      if (is.null(header_color) || nchar(trimws(header_color)) == 0) header_color <- "#0284c7"

      outcome_pos <- input$outcome_pos_val

      tryCatch({
        if (method == "group_comparison") {
          # Executing descr_by_group for each predictor against target group
          if (exists("descr_by_group", where = asNamespace("analytix"))) {
            target_sym <- rlang::sym(target)
            
            ft_list <- lapply(preds, function(p) {
              p_sym <- rlang::sym(p)
              res <- tryCatch(
                analytix::descr_by_group(df, var = !!p_sym, by = !!target_sym, digits = digits, color = header_color),
                error = function(e) {
                  # Retry without digits or color if signature differs
                  tryCatch(
                    analytix::descr_by_group(df, var = !!p_sym, by = !!target_sym),
                    error = function(e2) NULL
                  )
                }
              )

              anova_res <- NULL
              if (is.numeric(df[[p]]) && length(unique(df[[target]])) >= 3) {
                if (exists("anova_table", where = asNamespace("analytix"))) {
                  anova_res <- tryCatch(
                    analytix::anova_table(df, var = !!p_sym, group = !!target_sym, digits = digits, color = header_color),
                    error = function(e) {
                      tryCatch(
                        analytix::anova_table(df, var = !!p_sym, group = !!target_sym),
                        error = function(e2) NULL
                      )
                    }
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
              `P_Value` = if (!is.null(chi)) round(chi$p.value, digits) else "N/A",
              `Test` = "Chi-Square",
              check.names = FALSE
            )
          })
          combined_df <- do.call(rbind, res_list)
          return(list(method = "group_comparison", type = "df", target = target, results = combined_df))
          
        } else if (method == "or_table") {
          req(outcome_pos)

          if (exists("bivariate_or_table", where = asNamespace("analytix"))) {
            res <- tryCatch(
              analytix::bivariate_or_table(
                df,
                outcome = target,
                exposures = preds,
                outcome_positive_val = outcome_pos,
                conf_level = conf_level,
                digits = digits,
                color = header_color
              ),
              error = function(e) {
                # Retry with subset of arguments if custom signature varies
                analytix::bivariate_or_table(df, outcome = target, exposures = preds, outcome_positive_val = outcome_pos)
              }
            )
            if (!is.null(res)) {
              return(list(method = "or_table", type = "flextable", target = target, results = res))
            }
          }
          
          # Fallback Odds Ratio calculation if analytix function isn't available or fails
          rows_list <- list()
          df_mod <- df
          df_mod$outcome_bin <- ifelse(df_mod[[target]] == outcome_pos, 1, 0)

          for (exp_var in preds) {
            exp_vec <- df_mod[[exp_var]]
            formula_obj <- stats::as.formula(paste("outcome_bin ~ factor(", exp_var, ")"))
            fit <- tryCatch(stats::glm(formula_obj, data = df_mod, family = stats::binomial()), error = function(e) NULL)

            if (is.null(fit)) next

            co <- summary(fit)$coefficients
            ci <- tryCatch(suppressMessages(stats::confint(fit, level = conf_level)), error = function(e) {
              # Fallback CI using Standard Error
              ci_est <- coef(fit)
              se <- sqrt(diag(vcov(fit)))
              z <- stats::qnorm(1 - (1 - conf_level)/2)
              cbind(ci_est - z * se, ci_est + z * se)
            })

            levels_val <- levels(factor(exp_vec[!is.na(exp_vec)]))
            for (i in seq_along(levels_val)) {
              mod <- levels_val[i]
              sub_df <- df_mod[df_mod[[exp_var]] == mod & !is.na(df_mod[[exp_var]]) & !is.na(df_mod$outcome_bin), ]
              n_mod <- nrow(sub_df)
              n_pos <- sum(sub_df$outcome_bin == 1)
              pct_pos <- (n_pos / n_mod) * 100

              n_pct_str <- sprintf("%d/%d (%.1f%%)", n_pos, n_mod, pct_pos)

              if (i == 1) {
                or_str <- "1.00 (Réf.)"
                p_str <- "-"
              } else {
                coef_row_name <- paste0("factor(", exp_var, ")", mod)
                if (coef_row_name %in% rownames(co)) {
                  or_val <- exp(co[coef_row_name, "Estimate"])
                  p_val <- co[coef_row_name, "Pr(>|z|)"]

                  ci_low <- exp(ci[coef_row_name, 1])
                  ci_high <- exp(ci[coef_row_name, 2])

                  or_fmt <- format(round(or_val, digits), nsmall = digits)
                  low_fmt <- format(round(ci_low, digits), nsmall = digits)
                  high_fmt <- format(round(ci_high, digits), nsmall = digits)

                  or_str <- sprintf("%s [%s - %s]", or_fmt, low_fmt, high_fmt)
                  p_str <- if (p_val < 0.001) "< 0,001" else format(round(p_val, 3))
                } else {
                  or_str <- "-"
                  p_str <- "-"
                }
              }

              rows_list[[length(rows_list) + 1]] <- data.frame(
                Variable = exp_var,
                Modalite = mod,
                Effectif_Pct = n_pct_str,
                OR_IC95 = or_str,
                P_value = p_str,
                stringsAsFactors = FALSE
              )
            }
          }

          if (length(rows_list) > 0) {
            out_df <- dplyr::bind_rows(rows_list)
            dup_idx <- duplicated(out_df$Variable)
            out_df$Variable[dup_idx] <- ""
            names(out_df) <- c("Variable", "Modalité", paste0("Effectif (", outcome_pos, ")"), "OR brut [IC95%]", "p-value")

            ft <- flextable::flextable(out_df)
            if (exists("theme_analytique", where = asNamespace("analytix"))) {
              ft <- analytix::theme_analytique(ft, color = header_color)
            } else {
              ft <- flextable::theme_vanilla(ft)
            }
            ft <- flextable::set_caption(ft, paste("Association bivariée avec", target, "(Événement :", outcome_pos, ")"))
            return(list(method = "or_table", type = "flextable", target = target, results = ft))
          }

          res_df <- data.frame(
            Predictor = preds,
            Outcome = target,
            Odds_Ratio = "Calcul non disponible",
            IC_95 = "[ - ]"
          )
          return(list(method = "or_table", type = "df", target = target, results = res_df))

        } else if (method == "multivariate_or") {
          if (exists("multivariable_logistic_table", where = asNamespace("analytix"))) {
            form <- stats::as.formula(paste(target, "~", paste(preds, collapse = " + ")))
            # Create binomial GLM
            fit_multi <- tryCatch({
              # Make sure target is numeric 0/1 for binomial glm
              df_multi <- df
              if (!is.null(outcome_pos)) {
                df_multi[[target]] <- ifelse(df_multi[[target]] == outcome_pos, 1, 0)
              }
              stats::glm(form, data = df_multi, family = stats::binomial())
            }, error = function(e) NULL)

            if (!is.null(fit_multi)) {
              res <- tryCatch({
                analytix::multivariable_logistic_table(fit_multi, digits = digits, color = header_color)
              }, error = function(e) {
                tryCatch({
                  analytix::multivariable_logistic_table(fit_multi)
                }, error = function(e2) {
                  # Fallback to multivariable_logistic_table with formula signature
                  analytix::multivariable_logistic_table(form, data = df)
                })
              })
              return(list(method = "multivariate_or", type = "flextable", target = target, results = res))
            }
          }

          # Fallback Multivariable GLM Table
          df_multi <- df
          df_multi$outcome_bin <- ifelse(df_multi[[target]] == outcome_pos, 1, 0)
          form <- stats::as.formula(paste("outcome_bin ~", paste(preds, collapse = " + ")))
          fit <- tryCatch(stats::glm(form, data = df_multi, family = stats::binomial()), error = function(e) NULL)

          if (!is.null(fit)) {
            co <- summary(fit)$coefficients
            ci <- tryCatch(suppressMessages(stats::confint(fit, level = conf_level)), error = function(e) {
              ci_est <- coef(fit)
              se <- sqrt(diag(vcov(fit)))
              z <- stats::qnorm(1 - (1 - conf_level)/2)
              cbind(ci_est - z * se, ci_est + z * se)
            })

            res_rows <- list()
            for (rn in rownames(co)) {
              if (rn == "(Intercept)") next
              or_val <- exp(co[rn, "Estimate"])
              p_val <- co[rn, "Pr(>|z|)"]
              ci_low <- exp(ci[rn, 1])
              ci_high <- exp(ci[rn, 2])

              res_rows[[length(res_rows) + 1]] <- data.frame(
                Indicateur = rn,
                `OR ajusté` = format(round(or_val, digits), nsmall = digits),
                `IC 95%` = sprintf("[%s - %s]", format(round(ci_low, digits), nsmall = digits), format(round(ci_high, digits), nsmall = digits)),
                `p-value` = if (p_val < 0.001) "< 0,001" else format(round(p_val, 3)),
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
            }
            if (length(res_rows) > 0) {
              ft <- flextable::flextable(dplyr::bind_rows(res_rows))
              ft <- flextable::theme_vanilla(ft)
              ft <- flextable::set_caption(ft, paste("Régression Logistique Multivariée - Outcome :", target))
              return(list(method = "multivariate_or", type = "flextable", target = target, results = ft))
            }
          }

          res_df <- data.frame(
            Predictor = preds,
            Outcome = target,
            Note = "Régression multivariée non disponible"
          )
          return(list(method = "multivariate_or", type = "df", target = target, results = res_df))
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

          ft_val <- get_flextable(val)
          ft_anova <- if (!is.null(anova_val)) get_flextable(anova_val$anova) else NULL
          ft_tukey <- if (!is.null(anova_val)) get_flextable(anova_val$tukey) else NULL

          tags$div(
            class = "mb-4 p-3 border rounded bg-white",
            tags$h5(class = "fw-bold text-slate-800 mb-3", paste("Comparaison :", pred_name, "vs", res_info$target)),
            if (!is.null(ft_val)) {
              flextable::htmltools_value(ft_val)
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
                  if (!is.null(ft_anova)) flextable::htmltools_value(ft_anova),
                  tags$div(class = "my-3"),
                  if (!is.null(ft_tukey)) flextable::htmltools_value(ft_tukey)
                )
              )
            }
          )
        })
        do.call(tagList, tag_list)
      } else {
        ft_results <- get_flextable(results)
        if (!is.null(ft_results)) {
          flextable::htmltools_value(ft_results)
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

            ft_val <- get_flextable(val)
            if (!is.null(ft_val)) {
              doc <- flextable::body_add_flextable(doc, ft_val)
            } else if (is.data.frame(val)) {
              ft <- flextable::theme_vanilla(flextable::flextable(val))
              doc <- flextable::body_add_flextable(doc, ft)
            }

            if (!is.null(anova_val)) {
              doc <- officer::body_add_par(doc, "Tableau d'ANOVA à un facteur", style = "heading 3")
              ft_anova <- get_flextable(anova_val$anova)
              if (!is.null(ft_anova)) doc <- flextable::body_add_flextable(doc, ft_anova)
              doc <- officer::body_add_par(doc, "Test post-hoc de Tukey (HSD)", style = "heading 3")
              ft_tukey <- get_flextable(anova_val$tukey)
              if (!is.null(ft_tukey)) doc <- flextable::body_add_flextable(doc, ft_tukey)
            }
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
        } else {
          ft_results <- get_flextable(results)
          if (!is.null(ft_results)) {
            doc <- flextable::body_add_flextable(doc, ft_results)
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
            df <- get_dataframe(val)
            if (is.null(df)) df <- val

            if (!is.null(df) && nrow(df) > 0) {
              df$Predictor_Variable <- item$pred
            }
            df
          })
          combined <- dplyr::bind_rows(df_list)
          write.csv(combined, file, row.names = FALSE)
        } else {
          df_results <- get_dataframe(results)
          if (!is.null(df_results)) {
            write.csv(df_results, file, row.names = FALSE)
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
