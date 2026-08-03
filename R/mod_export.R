#' Module Exportation du Rapport Word
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame
#' @param univar_reactive Reactive returning univariate results
#' @param bivar_reactive Reactive returning bivariate results

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
      checkboxInput(ns("inc_univar"), " Analyses Univariées", value = TRUE),
      checkboxInput(ns("inc_bivar"), " Analyses Bivariées", value = TRUE),
      
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
            icon("eye", class = "me-2"), " Aperçu de la Structure du Document Word"
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
            tags$li("Section 2 : Diagnostic d'exhaustivité des données"),
            tags$li("Section 3 : Tableaux de fréquences et statistiques descriptives (normes francophones)"),
            tags$li("Section 4 : Comparaisons bivariées et tests d'hypothèses")
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

mod_export_server <- function(id, data_reactive, univar_reactive, bivar_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$prev_title <- renderText({ input$report_title })
    output$prev_subtitle <- renderText({ input$report_subtitle })
    output$prev_author <- renderText({ paste(input$report_author, "-", input$report_institution) })
    
    output$download_word <- downloadHandler(
      filename = function() {
        paste0("Rapport_Analytix_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        shiny::withProgress(message = 'Génération du rapport Word en cours...', value = 0, {
          
          df <- data_reactive()
          shiny::incProgress(0.2, detail = "Initialisation du document...")
          
          # Initialize officer doc
          doc <- officer::read_docx()
          
          # Title section
          doc <- officer::body_add_par(doc, input$report_title, style = "Title")
          doc <- officer::body_add_par(doc, input$report_subtitle, style = "Subtitle")
          doc <- officer::body_add_par(doc, paste("Auteur :", input$report_author, "| Date :", Sys.Date()), style = "Normal")
          doc <- officer::body_add_par(doc, "", style = "Normal")
          
          shiny::incProgress(0.4, detail = "Ajout de la synthèse des données...")
          
          # 1. Summary Section
          if (isTRUE(input$inc_summary) && !is.null(df)) {
            doc <- officer::body_add_par(doc, "1. Aperçu du Jeu de Données", style = "heading 1")
            meta_df <- data.frame(
              Indicateur = c("Nombre total d'observations", "Nombre de variables", "Taux global d'exhaustivité"),
              Valeur = c(nrow(df), ncol(df), paste0(round((1 - sum(is.na(df))/(nrow(df)*ncol(df)))*100, 1), "%"))
            )
            ft_meta <- flextable::flextable(meta_df)
            ft_meta <- flextable::theme_vanilla(ft_meta)
            doc <- flextable::body_add_flextable(doc, ft_meta)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          
          shiny::incProgress(0.6, detail = "Ajout des tableaux univariés...")
          
          # 2. Univariate Section
          if (isTRUE(input$inc_univar)) {
            u_res <- tryCatch(univar_reactive(), error = function(e) NULL)
            if (!is.null(u_res) && !is.null(u_res$table)) {
              doc <- officer::body_add_par(doc, "2. Analyse Univariée", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Variable analysée :", u_res$var), style = "heading 2")
              
              if (inherits(u_res$table, "flextable")) {
                doc <- flextable::body_add_flextable(doc, u_res$table)
              } else if (is.data.frame(u_res$table)) {
                ft_u <- flextable::flextable(u_res$table)
                ft_u <- flextable::theme_vanilla(ft_u)
                doc <- flextable::body_add_flextable(doc, ft_u)
              }
              doc <- officer::body_add_par(doc, "", style = "Normal")
            }
          }
          
          shiny::incProgress(0.8, detail = "Ajout des tableaux bivariés...")
          
          # 3. Bivariate Section
          if (isTRUE(input$inc_bivar)) {
            b_res <- tryCatch(bivar_reactive(), error = function(e) NULL)
            if (!is.null(b_res) && !is.null(b_res$res)) {
              doc <- officer::body_add_par(doc, "3. Analyse Bivariée & Modélisation", style = "heading 1")
              doc <- officer::body_add_par(doc, paste("Outcome :", b_res$target), style = "heading 2")
              
              if (inherits(b_res$res, "flextable")) {
                doc <- flextable::body_add_flextable(doc, b_res$res)
              } else if (is.data.frame(b_res$res)) {
                ft_b <- flextable::flextable(b_res$res)
                ft_b <- flextable::theme_vanilla(ft_b)
                doc <- flextable::body_add_flextable(doc, ft_b)
              }
            }
          }
          
          shiny::incProgress(1.0, detail = "Finalisation...")
          print(doc, target = file)
        })
      }
    )
  })
}
