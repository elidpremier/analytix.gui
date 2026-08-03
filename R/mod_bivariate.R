#' Module Analyse Bivariée & Modélisation
#' 
#' @param id Identifiant du module Shiny
#' @param data_reactive Reactive returning the cleaned data.frame

mod_bivariate_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = tags$div(
        bsicons::bs_icon("diagram-3", class = "me-2 text-primary"),
        "Configuration Bivariée"
      ),
      width = 340,
      
      selectInput(
        ns("target_var"),
        "Variable Cible / Outcome (ex: Maladie Oui/Non ou Groupe) :",
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
        bsicons::bs_icon("lightbulb-fill", class = "me-1"),
        "Les p-values et Odds Ratios (avec IC 95%) sont calculés automatiquement selon la nature des variables."
      )
    ),
    
    tags$div(
      class = "container-fluid p-0",
      
      bslib::card(
        bslib::card_header(
          tags$div(
            bsicons::bs_icon("table"), " Résultats Bivariés & Tests Statistiques (analytix)"
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
    
    # Compute bivariate analysis
    bivariate_res <- reactive({
      df <- data_reactive()
      req(df, input$target_var, input$pred_vars)
      
      target <- input$target_var
      preds <- input$pred_vars
      method <- input$method
      
      tryCatch({
        if (method == "group_comparison") {
          if (exists("descr_by_group", where = asNamespace("analytix"))) {
            analytix::descr_by_group(df, target_var = target, vars = preds)
          } else {
            # Fallback simple table if package function missing
            res_list <- lapply(preds, function(p) {
              tb <- table(df[[p]], df[[target]])
              chi <- tryCatch(chisq.test(tb), error = function(e) NULL)
              data.frame(
                Predictor = p,
                p_value = if (!is.null(chi)) round(chi$p.value, 4) else "N/A"
              )
            })
            do.call(rbind, res_list)
          }
        } else if (method == "or_table") {
          if (exists("bivariate_or_table", where = asNamespace("analytix"))) {
            analytix::bivariate_or_table(df, outcome = target, predictors = preds)
          } else if (exists("cross_multi", where = asNamespace("analytix"))) {
            analytix::cross_multi(df, target_var = target, vars = preds)
          } else {
            data.frame(Message = "Calcul des Odds Ratios via analytix::bivariate_or_table")
          }
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
    
    return(reactive({
      list(
        target = input$target_var,
        preds = input$pred_vars,
        res = bivariate_res()
      )
    }))
  })
}
