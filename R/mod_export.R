#' Module Exportation du Rapport Word Global
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame
#' @param univar_reactive Reactive returning univariate results
#' @param bivar_reactive Reactive returning bivariate results
#' @param spec_reactive Reactive returning specialized results

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

mod_export_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        icon("file-word", class = "me-2 text-primary"),
        "Paramètres du Rapport"
      ),
      width = 340,
      
      textInput(ns("report_title"), "Titre du Rapport :", value = "Rapport d'Analyse Statistique"),
      textInput(ns("report_subtitle"), "Sous-titre / Projet :", value = "Étude Épidémiologique & Descriptive"),
      textInput(ns("report_author"), "Auteur / Investigateur :", value = "Dr / Chercheur"),
      textInput(ns("report_institution"), "Organisme / Client :", value = "Redaklab / Hôpital"),
      
      tags$hr(),
      tags$h6("Sections à Inclure :"),
      checkboxInput(ns("inc_summary"), " Synthèse du Jeu de Données", value = TRUE),
      checkboxInput(ns("inc_missing"), " Rapport sur les Données Manquantes", value = TRUE),
      checkboxInput(ns("inc_global_univar"), " Tableau Descriptif Global", value = TRUE),
      checkboxInput(ns("inc_univar"), " Analyses Univariées (par variable)", value = TRUE),
      checkboxInput(ns("inc_bivar"), " Analyses Bivariées", value = TRUE),
      checkboxInput(ns("inc_multiv"), " Régression Logistique Multivariée", value = TRUE),
      checkboxInput(ns("inc_spec"), " Analyses Spécialisées (Likert, Multi-choix)", value = TRUE),
      checkboxInput(ns("inc_cor"), " Matrice de Corrélations complète", value = TRUE),
      checkboxInput(ns("inc_diag"), " Performance Diagnostique", value = TRUE),
      
      tags$hr(),
      downloadButton(
        ns("download_word"),
        "Télécharger le Rapport Word (.docx)",
        class = "btn-success btn-lg w-100"
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::card(
        bslib::card_header(
          tags$div(
            icon("eye", class = "me-2"), " Aperçu de la Structure du Document Word Global"
          )
        ),
        tags$div(
          class = "p-3",
          tags$h4(uiOutput(ns("prev_title"))),
          tags$p(class = "text-muted", uiOutput(ns("prev_subtitle"))),
          tags$p(tags$strong("Auteur : "), uiOutput(ns("prev_author"))),
          tags$hr(),
          tags$h5("Sommaire des Tableaux & Figures qui seront générés :"),
          tags$ul(
            tags$li("Section 1 : Caractéristiques de la population d'étude"),
            tags$li("Section 2 : Diagnostic d'exhaustivité et valeurs manquantes (avec Carte visuelle)"),
            tags$li("Section 3 : Tableaux de fréquences, statistiques descriptives univariées et calcul de Prévalence"),
            tags$li("Section 4 : Comparaisons bivariées (ANOVA + Tukey) et Modélisations (Régression logistique bivariée / multivariée)"),
            tags$li("Section 5 : Échelles de Likert, réponses multiples, Matrice de corrélations et Performance diagnostique")
          ),
          tags$div(
            class = "alert alert-success mt-4",
            icon("check-circle", class = "me-2"),
            "Le document produit est un fichier Microsoft Word (.docx) natif, entièrement modifiable et conforme aux standards de publication scientifique."
          )
        )
      )
    )
  )
}

mod_export_server <- function(id, data_reactive, univar_reactive, bivar_reactive, spec_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$prev_title <- renderText({
      df <- data_reactive()
      shiny::validate(
        shiny::need(!is.null(df) && nrow(df) > 0, "Attention : Aucun jeu de données chargé. Veuillez d'abord charger vos données dans l'onglet 'Données & Libellés' pour générer le rapport.")
      )
      input$report_title
    })
    output$prev_subtitle <- renderText({
      req(data_reactive())
      input$report_subtitle
    })
    output$prev_author <- renderText({
      req(data_reactive())
      paste(input$report_author, "-", input$report_institution)
    })
    
    output$download_word <- downloadHandler(
      filename = function() {
        paste0("Rapport_Analytix_Complet_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        df <- data_reactive()
        if (is.null(df)) {
          showNotification("Impossible de générer le rapport : Aucun jeu de données chargé.", type = "error")
          return(NULL)
        }
        shiny::withProgress(message = 'Génération du rapport Word global...', value = 0, {
          
          shiny::incProgress(0.1, detail = "Initialisation du document...")
          
          doc <- officer::read_docx()
          
          # Title section
          # Use "heading 1" and "heading 2" instead of "Title" / "Subtitle" styles which may not be present in the default blank document template of officer.
          doc <- officer::body_add_par(doc, input$report_title, style = "heading 1")
          doc <- officer::body_add_par(doc, input$report_subtitle, style = "heading 2")
          doc <- officer::body_add_par(doc, paste("Auteur :", input$report_author, "| Organisme :", input$report_institution, "| Date :", Sys.Date()), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          shiny::incProgress(0.2, detail = "Ajout de la synthèse des données...")
          
          # 1. Summary Section
          if (isTRUE(input$inc_summary) && !is.null(df)) {
            doc <- officer::body_add_par(doc, "1. Aperçu du Jeu de Données", style = "heading 1")
            meta_df <- data.frame(
              Indicateur = c("Nombre total d'observations", "Nombre de variables", "Taux global d'exhaustivité"),
              Valeur = c(nrow(df), ncol(df), paste0(round((1 - sum(is.na(df))/(nrow(df)*ncol(df)))*100, 1), "%"))
            )
            ft_meta <- flextable::theme_vanilla(flextable::flextable(meta_df))
            doc <- flextable::body_add_flextable(doc, ft_meta)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          
          # 1.5. Missing Values Section
          if (isTRUE(input$inc_missing) && !is.null(df)) {
            doc <- officer::body_add_par(doc, "1.2. Diagnostic des Valeurs Manquantes", style = "heading 2")

            na_counts <- sapply(df, function(x) sum(is.na(x)))
            na_pct <- round((na_counts / nrow(df)) * 100, 1)
            na_df <- data.frame(
              `Variable` = names(df),
              `Nombre de NA` = na_counts,
              `Proportion` = paste0(na_pct, " %"),
              check.names = FALSE
            )
            na_df_sorted <- na_df[order(-na_counts), ]

            ft_na <- flextable::theme_vanilla(flextable::flextable(na_df_sorted))
            doc <- flextable::body_add_flextable(doc, ft_na)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }

          shiny::incProgress(0.3, detail = "Ajout du Tableau Descriptif Global...")

          # 1.8. Global Descriptive Table Section
          if (isTRUE(input$inc_global_univar)) {
            u_res <- tryCatch(univar_reactive(), error = function(e) NULL)
            if (!is.null(u_res) && !is.null(u_res$global_descriptive)) {
              doc <- officer::body_add_par(doc, "1.3. Tableau Descriptif Global", style = "heading 2")
              for (var_name in names(u_res$global_descriptive)) {
                obj <- u_res$global_descriptive[[var_name]]
                ft <- get_flextable(obj)
                doc <- officer::body_add_par(doc, paste("Description de :", var_name), style = "heading 3")
                if (!is.null(ft)) {
                  doc <- flextable::body_add_flextable(doc, ft)
                } else {
                  doc <- officer::body_add_par(doc, "Tableau non disponible", style = "Normal")
                }
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          shiny::incProgress(0.5, detail = "Ajout des tableaux univariés...")
          
          # 2. Univariate Section
          if (isTRUE(input$inc_univar)) {
            u_res <- tryCatch(univar_reactive(), error = function(e) NULL)
            if (!is.null(u_res) && !is.null(u_res$table)) {
              doc <- officer::body_add_par(doc, "2. Analyse Univariée", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Variable analysée :", u_res$var), style = "heading 2")
              
              ft <- get_flextable(u_res$table)
              if (!is.null(ft)) {
                doc <- flextable::body_add_flextable(doc, ft)
              } else {
                df_u <- get_dataframe(u_res$table)
                if (!is.null(df_u)) {
                  ft_u <- flextable::theme_vanilla(flextable::flextable(df_u))
                  doc <- flextable::body_add_flextable(doc, ft_u)
                } else if (is.data.frame(u_res$table)) {
                  ft_u <- flextable::theme_vanilla(flextable::flextable(u_res$table))
                  doc <- flextable::body_add_flextable(doc, ft_u)
                }
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          shiny::incProgress(0.7, detail = "Ajout des tableaux bivariés...")
          
          # 3. Bivariate Section
          if (isTRUE(input$inc_bivar)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$res) && b_res$res$method != "multivariate_or") {
              doc <- officer::body_add_par(doc, "3. Analyse Bivariée & Modélisation", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Outcome :", b_res$target), style = "heading 2")
              
              res_info <- b_res$res
              method <- res_info$method
              type <- res_info$type
              results <- res_info$results

              if (method == "group_comparison" && type == "list") {
                for (item in results) {
                  pred_name <- item$pred
                  val <- item$value
                  anova_val <- item$anova
                  doc <- officer::body_add_par(doc, paste("Comparaison :", pred_name, "vs", b_res$target), style = "heading 3")

                  ft_val <- get_flextable(val)
                  if (!is.null(ft_val)) {
                    doc <- flextable::body_add_flextable(doc, ft_val)
                  } else {
                    df_val <- get_dataframe(val)
                    if (!is.null(df_val)) {
                      ft <- flextable::theme_vanilla(flextable::flextable(df_val))
                      doc <- flextable::body_add_flextable(doc, ft)
                    } else if (is.data.frame(val)) {
                      ft <- flextable::theme_vanilla(flextable::flextable(val))
                      doc <- flextable::body_add_flextable(doc, ft)
                    }
                  }

                  if (!is.null(anova_val)) {
                    doc <- officer::body_add_par(doc, "Tableau d'ANOVA à un facteur", style = "heading 4")
                    ft_anova <- get_flextable(anova_val$anova)
                    if (!is.null(ft_anova)) doc <- flextable::body_add_flextable(doc, ft_anova)

                    doc <- officer::body_add_par(doc, "Test post-hoc de Tukey (HSD)", style = "heading 4")
                    ft_tukey <- get_flextable(anova_val$tukey)
                    if (!is.null(ft_tukey)) doc <- flextable::body_add_flextable(doc, ft_tukey)
                  }
                  doc <- officer::body_add_par(doc, "", style = "Normal")
                }
              } else {
                ft_res <- get_flextable(results)
                if (!is.null(ft_res)) {
                  doc <- flextable::body_add_flextable(doc, ft_res)
                } else {
                  df_res <- get_dataframe(results)
                  if (!is.null(df_res)) {
                    ft_b <- flextable::theme_vanilla(flextable::flextable(df_res))
                    doc <- flextable::body_add_flextable(doc, ft_b)
                  } else if (is.data.frame(results)) {
                    ft_b <- flextable::theme_vanilla(flextable::flextable(results))
                    doc <- flextable::body_add_flextable(doc, ft_b)
                  }
                }
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }
          
          # 3.5. Multivariable Regression Section
          if (isTRUE(input$inc_multiv)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$res) && b_res$res$method == "multivariate_or") {
              doc <- officer::body_add_par(doc, "3.2. Modélisation par Régression Logistique Multivariée", style = "heading 2")
              res_info <- b_res$res
              results <- res_info$results

              ft_res <- get_flextable(results)
              if (!is.null(ft_res)) {
                doc <- flextable::body_add_flextable(doc, ft_res)
              } else {
                df_res <- get_dataframe(results)
                if (!is.null(df_res)) {
                  ft_mult <- flextable::theme_vanilla(flextable::flextable(df_res))
                  doc <- flextable::body_add_flextable(doc, ft_mult)
                } else if (is.data.frame(results)) {
                  ft_mult <- flextable::theme_vanilla(flextable::flextable(results))
                  doc <- flextable::body_add_flextable(doc, ft_mult)
                }
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }

          shiny::incProgress(0.9, detail = "Ajout des analyses spécialisées...")

          # 4. Specialized Section
          if (isTRUE(input$inc_spec)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res)) {
              doc <- officer::body_add_par(doc, "4. Analyses Spécialisées", style = "heading 1")
              if (!is.null(s_res$likert)) {
                doc <- officer::body_add_par(doc, "Échelle de Likert", style = "heading 2")
                ft_likert <- get_flextable(s_res$likert)
                if (!is.null(ft_likert)) {
                  doc <- flextable::body_add_flextable(doc, ft_likert)
                } else {
                  df_likert <- get_dataframe(s_res$likert)
                  if (!is.null(df_likert)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(df_likert)))
                  else if (is.data.frame(s_res$likert)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(s_res$likert)))
                }
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
              if (!is.null(s_res$multi)) {
                doc <- officer::body_add_par(doc, "Analyse des Réponses Multiples", style = "heading 2")
                ft_multi <- get_flextable(s_res$multi)
                if (!is.null(ft_multi)) {
                  doc <- flextable::body_add_flextable(doc, ft_multi)
                } else {
                  df_multi <- get_dataframe(s_res$multi)
                  if (!is.null(df_multi)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(df_multi)))
                  else if (is.data.frame(s_res$multi)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(s_res$multi)))
                }
                doc <- officer::body_add_par(doc, "", style = "Normal")
              }
            }
          }

          # 4.2. Correlation Matrix Section
          if (isTRUE(input$inc_cor)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res) && !is.null(s_res$correlation_matrix)) {
              doc <- officer::body_add_par(doc, "4.2. Matrice de Corrélations complète", style = "heading 2")
              res <- s_res$correlation_matrix
              ft_cor <- get_flextable(res)
              if (!is.null(ft_cor)) {
                doc <- flextable::body_add_flextable(doc, ft_cor)
              } else {
                df_cor <- get_dataframe(res)
                if (!is.null(df_cor)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(df_cor)))
                else if (is.data.frame(res)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(res)))
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }

          # 4.3. Diagnostic Performance Section
          if (isTRUE(input$inc_diag)) {
            s_res <- tryCatch(spec_reactive(), error = function(e) NULL)
            if (!is.null(s_res) && !is.null(s_res$diagnostic)) {
              doc <- officer::body_add_par(doc, "4.3. Performance Diagnostique du Test", style = "heading 2")
              res <- s_res$diagnostic
              ft_diag <- get_flextable(res)
              if (!is.null(ft_diag)) {
                doc <- flextable::body_add_flextable(doc, ft_diag)
              } else {
                df_diag <- get_dataframe(res)
                if (!is.null(df_diag)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(df_diag)))
                else if (is.data.frame(res)) doc <- flextable::body_add_flextable(doc, flextable::theme_vanilla(flextable::flextable(res)))
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          shiny::incProgress(1.0, detail = "Finalisation...")
          print(doc, target = file)
        })
      }
    )
  })
}
