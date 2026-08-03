# ==============================================================================
# Analytix-GUI : Application Web No-Code (Shiny / Bslib)
# Développé pour les cliniciens, étudiants et clients Redaklab
# Moteur d'analyse : Package R analytix
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)
library(readxl)

# Helper pour les icônes
safe_icon <- function(name, class = "", fa_fallback = "circle") {
  if (requireNamespace("bsicons", quietly = TRUE)) {
    return(bsicons::bs_icon(name, class = class))
  } else {
    return(shiny::icon(fa_fallback, class = class))
  }
}

# --- Chargement de analytix ---
if (requireNamespace("analytix", quietly = TRUE)) {
  library(analytix)
} else if (file.exists("../analytix/DESCRIPTION")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all("../analytix")
  } else {
    sapply(list.files("../analytix/R", pattern = "\\.R$", full.names = TRUE), source)
  }
}

# --- Source des modules ---
source("R/mod_import.R")
source("R/mod_univariate.R")
source("R/mod_bivariate.R")
source("R/mod_specialized.R")
source("R/mod_export.R")

# --- UI de l'application ---
ui <- bslib::page_navbar(
  id = "navbar",
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    safe_icon("chart-line-fill", class = "text-info fs-4", fa_fallback = "chart-line"),
    tags$span("Analytix", style = "font-weight: 700; font-size: 1.25rem;"),
    tags$span("GUI", class = "badge-gui")
  ),
  
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "zephyr",
    primary = "#0284c7",
    secondary = "#475569",
    success = "#059669",
    info = "#0284c7",
    warning = "#d97706",
    danger = "#dc2626",
    font_scale = 0.95
  ),
  
  header = tags$head(
    includeCSS("www/style.css")
  ),
  
  # --- Onglet 0 : Accueil ---
  bslib::nav_panel(
    title = "Accueil & Guide",
    icon = safe_icon("house-door-fill", fa_fallback = "home"),
    
    tags$div(
      class = "container py-4",
      
      # Hero Banner
      tags$div(
        class = "hero-card",
        tags$div(
          class = "row align-items-center",
          tags$div(
            class = "col-lg-8",
            tags$h1("Interface d'Analyse Statistique No-Code"),
            tags$p("Transformez vos données brutes Excel en rapports scientifiques au format Word prêts pour publication sans écrire la moindre ligne de code R."),
            tags$div(
              class = "mt-4 d-flex gap-3",
              actionButton("btn_start", "Commencer l'Analyse", class = "btn btn-light btn-lg text-primary font-weight-bold", icon = icon("rocket")),
              actionButton("btn_demo", "Tester avec l'exemple", class = "btn btn-outline-light btn-lg", icon = icon("flask"))
            )
          ),
          tags$div(
            class = "col-lg-4 text-center d-none d-lg-block",
            safe_icon("file-earmark-medical", class = "text-white-50", fa_fallback = "notes-medical")
          )
        )
      ),
      
      # Step Workflow Cards
      tags$h3(class = "fw-bold mb-4 text-slate-800", "Comment ça marche ? (En 3 étapes simples)"),
      
      bslib::layout_column_wrap(
        width = 1/3,
        
        tags$div(
          class = "feature-step",
          tags$div(class = "feature-step-num", "1"),
          tags$h5(class = "fw-bold", "Importez & Labellisez"),
          tags$p(class = "text-muted", "Glissez-déposez votre fichier Excel (.xlsx, .xls) ou CSV. Éditez les libellés francophones de vos variables.")
        ),
        
        tags$div(
          class = "feature-step",
          tags$div(class = "feature-step-num", "2"),
          tags$h5(class = "fw-bold", "Explorez & Exportez à l'instant"),
          tags$p(class = "text-muted", "Obtenez instantanément des tableaux descriptifs, des tests bivariés et des graphiques avec leurs boutons de téléchargement direct.")
        ),
        
        tags$div(
          class = "feature-step",
          tags$div(class = "feature-step-num", "3"),
          tags$h5(class = "fw-bold", "Exportez le Rapport Word Global"),
          tags$p(class = "text-muted", "Téléchargez votre document Word (.docx) récapitulatif complet prêt pour publication scientifique.")
        )
      )
    )
  ),
  
  # --- Onglet 1 : Données & Libellés ---
  bslib::nav_panel(
    title = "1. Données & Libellés",
    icon = safe_icon("file-earmark-spreadsheet-fill", fa_fallback = "table"),
    mod_import_ui("import_module")
  ),
  
  # --- Onglet 2 : Univarié ---
  bslib::nav_panel(
    title = "2. Univarié",
    icon = safe_icon("pie-chart-fill", fa_fallback = "chart-pie"),
    mod_univariate_ui("univariate_module")
  ),
  
  # --- Onglet 3 : Bivarié ---
  bslib::nav_panel(
    title = "3. Bivarié & Tests",
    icon = safe_icon("diagram-3-fill", fa_fallback = "project-diagram"),
    mod_bivariate_ui("bivariate_module")
  ),
  
  # --- Onglet 4 : Analyses Spécialisées ---
  bslib::nav_panel(
    title = "4. Spécialisées",
    icon = safe_icon("star-fill", fa_fallback = "star"),
    mod_specialized_ui("specialized_module")
  ),
  
  # --- Onglet 5 : Rapport Word Global ---
  bslib::nav_panel(
    title = "5. Rapport Word Global",
    icon = safe_icon("file-word-fill", fa_fallback = "file-word"),
    mod_export_ui("export_module")
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://github.com/elidopremier/analytix.gui",
      target = "_blank",
      class = "nav-link text-white-50",
      icon("github", class = "me-1"), "GitHub"
    )
  )
)

# --- SERVER de l'application ---
server <- function(input, output, session) {
  
  # Shortcuts
  observeEvent(input$btn_start, {
    bslib::nav_select(id = "navbar", selected = "1. Données & Libellés")
  })
  observeEvent(input$btn_demo, {
    bslib::nav_select(id = "navbar", selected = "1. Données & Libellés")
  })
  
  # 1. Import & Libellés
  cleaned_data <- mod_import_server("import_module")
  
  # 2. Univarié
  univariate_res <- mod_univariate_server("univariate_module", data_reactive = cleaned_data)
  
  # 3. Bivarié
  bivariate_res <- mod_bivariate_server("bivariate_module", data_reactive = cleaned_data)
  
  # 4. Spécialisées
  specialized_res <- mod_specialized_server("specialized_module", data_reactive = cleaned_data)
  
  # 5. Export Word Global
  mod_export_server(
    "export_module",
    data_reactive = cleaned_data,
    univar_reactive = univariate_res,
    bivar_reactive = bivariate_res,
    spec_reactive = specialized_res
  )
}

# Launch App
shinyApp(ui = ui, server = server)
