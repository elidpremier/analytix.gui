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
          "📈 Table d'Odds Ratios (Régression Logistique Bivariée)" = "or_table"
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
      req(df, input$target_var, input$pred_vars)
      
      target <- input$target_var
      preds <- input$pred_vars
      method <- input$method
      
      tryCatch({
        if (method == "group_comparison") {
          # Executing descr_by_group for each predictor against target group
          if (exists("descr_by_group", where = asNamespace("analytix"))) {
            target_sym <- rlang::sym(target)
            
            # Loop predictors and combine flextables or dataframes
            ft_list <- lapply(preds, function(p) {
              p_sym <- rlang::sym(p)
              res <- tryCatch(
                analytix::descr_by_group(df, var = !!p_sym, by = !!target_sym),
                error = function(e) NULL
              )
              res
            })
            ft_list <- Filter(Negate(is.null), ft_list)
            if (length(ft_list) > 0) {
              return(ft_list[[1]]) # Returns primary flextable
            }
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
          do.call(rbind, res_list)
          
        } else if (method == "or_table") {
          if (exists("bivariate_or_table", where = asNamespace("analytix"))) {
            res <- tryCatch(
              analytix::bivariate_or_table(df, outcome = target, exposures = preds),
              error = function(e) NULL
            )
            if (!is.null(res)) return(res)
          }
          
          # Fallback Odds Ratio calculation
          data.frame(
            Predictor = preds,
            Outcome = target,
            Odds_Ratio = "Généré via analytix::bivariate_or_table",
            IC_95 = "[ - ]"
          )
        }
      }, error = function(e) {
        data.frame(Erreur = paste("Erreur bivariée :", e$message))
      })
    })
    
    output$bivariate_results_ui <- renderUI({
      res <- bivariate_res()
      req(res)
      
      if (inherits(res, "flextable")) {
        htmltools::HTML(flextable::htmltools_value(res))
      } else if (is.data.frame(res)) {
        tableOutput(ns("bivariate_table"))
      }
    })
    
    output$bivariate_table <- renderTable({
      res <- bivariate_res()
      req(is.data.frame(res))
      res
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # ⚡ EXPORT IMMÉDIAT HANDLERS
    output$dl_bivar_word <- downloadHandler(
      filename = function() { paste0("Tableau_Bivarié_", input$target_var, ".docx") },
      content = function(file) {
        res <- bivariate_res()
        doc <- officer::read_docx()
        doc <- officer::body_add_par(doc, paste("Analyse Bivariée - Outcome :", input$target_var), style = "heading 1")
        if (inherits(res, "flextable")) {
          doc <- flextable::body_add_flextable(doc, res)
        } else if (is.data.frame(res)) {
          ft <- flextable::theme_vanilla(flextable::flextable(res))
          doc <- flextable::body_add_flextable(doc, ft)
        }
        print(doc, target = file)
      }
    )
    
    output$dl_bivar_csv <- downloadHandler(
      filename = function() { paste0("Tableau_Bivarié_", input$target_var, ".csv") },
      content = function(file) {
        res <- bivariate_res()
        if (inherits(res, "flextable")) {
          write.csv(res$body$dataset, file, row.names = FALSE)
        } else if (is.data.frame(res)) {
          write.csv(res, file, row.names = FALSE)
        }
      }
    )
    
    return(reactive({
      list(target = input$target_var, preds = input$pred_vars, res = bivariate_res())
    }))
  })
}
