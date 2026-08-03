# ==============================================================================
# Analytix-GUI : Application Web No-Code (Shiny / Bslib)
# Développé pour les cliniciens, étudiants et clients Redaklab
# Moteur d'analyse : Package R analytix
# ==============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(flextable)
library(officer)
library(DT)

# --- Chargement de analytix (local dev ou package installé) ---
if (requireNamespace("analytix", quietly = TRUE)) {
  library(analytix)
} else if (file.exists("../analytix/DESCRIPTION")) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all("../analytix")
  } else {
    sapply(list.files("../analytix/R", pattern = "\\.R$", full.names = TRUE), source)
  }
}

# --- Source des modules de l'application ---
source("R/mod_import.R")
source("R/mod_univariate.R")
source("R/mod_bivariate.R")
source("R/mod_export.R")

# --- UI de l'application ---
ui <- bslib::page_navbar(
  title = tags$div(
    class = "d-flex align-items-center gap-2",
    bsicons::bs_icon("bar-chart-line-fill", class = "text-info fs-4"),
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
    includeCSS("www/style.css"),
    tags$link(rel = "shortcut icon", href = "favicon.ico")
  ),
  
  # --- Onglet 0 : Accueil ---
  bslib::nav_panel(
    title = "Accueil & Guide",
    icon = bsicons::bs_icon("house-door-fill"),
    
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
            bsicons::bs_icon("file-earmark-medical", size = "8x", class = "text-white-50")
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
          tags$h5(class = "fw-bold", "Importez vos données"),
          tags$p(class = "text-muted", "Glissez-déposez votre fichier Excel (.xlsx, .xls) ou CSV. L'application vérifie l'intégrité et détecte automatiquement les types de variables.")
        ),
        
        tags$div(
          class = "feature-step",
          tags$div(class = "feature-step-num", "2"),
          tags$h5(class = "fw-bold", "Explorez & Analysez"),
          tags$p(class = "text-muted", "Sélectionnez vos variables cibles et explicatives. Obtenez instantanément des tableaux descriptifs, tests statistiques (Chi², Student, Mann-Whitney) et graphiques.")
        ),
        
        tags$div(
          class = "feature-step",
          tags$div(class = "feature-step-num", "3"),
          tags$h5(class = "fw-bold", "Exportez le Rapport Word"),
          tags$p(class = "text-muted", "Téléchargez votre document Word (.docx) entièrement formaté selon les normes des revues médicales et scientifiques francophones.")
        )
      ),
      
      tags$div(class = "my-5"),
      
      # Public Target Section
      bslib::card(
        bslib::card_header(tags$div(bsicons::bs_icon("heart-pulse-fill", class = "me-2 text-danger"), "Conçu pour vos besoins")),
        tags$div(
          class = "row p-3",
          tags$div(
            class = "col-md-4",
            tags$h6(class = "fw-bold text-primary", "🏥 Cliniciens & Chercheurs"),
            tags$p(class = "text-muted small", "Gagnez un temps précieux dans l'analyse de vos registres et essais cliniques.")
          ),
          tags$div(
            class = "col-md-4",
            tags$h6(class = "fw-bold text-primary", "🎓 Étudiants & Thésards"),
            tags$p(class = "text-muted small", "Réalisez vos analyses de mémoire et de thèse en toute autonomie.")
          ),
          tags$div(
            class = "col-md-4",
            tags$h6(class = "fw-bold text-primary", "💼 Clients Redaklab"),
            tags$p(class = "text-muted small", "Bénéficiez de la puissance de calcul d'analytix dans un cadre sécurisé et intuitif.")
          )
        )
      )
    )
  ),
  
  # --- Onglet 1 : Données ---
  bslib::nav_panel(
    title = "1. Données",
    icon = bsicons::bs_icon("file-earmark-spreadsheet-fill"),
    mod_import_ui("import_module")
  ),
  
  # --- Onglet 2 : Univarié ---
  bslib::nav_panel(
    title = "2. Univarié",
    icon = bsicons::bs_icon("pie-chart-fill"),
    mod_univariate_ui("univariate_module")
  ),
  
  # --- Onglet 3 : Bivarié ---
  bslib::nav_panel(
    title = "3. Bivarié & Tests",
    icon = bsicons::bs_icon("diagram-3-fill"),
    mod_bivariate_ui("bivariate_module")
  ),
  
  # --- Onglet 4 : Rapport Word ---
  bslib::nav_panel(
    title = "4. Rapport Word",
    icon = bsicons::bs_icon("file-word-fill"),
    mod_export_ui("export_module")
  ),
  
  nav_spacer(),
  
  nav_item(
    tags$a(
      href = "https://github.com/elidopremier/analytix.gui",
      target = "_blank",
      class = "nav-link text-white-50",
      bsicons::bs_icon("github", class = "me-1"), "GitHub"
    )
  )
)

# --- SERVER de l'application ---
server <- function(input, output, session) {
  
  # Navigation shortcut buttons
  observeEvent(input$btn_start, {
    bslib::nav_select(id = "navbar", selected = "1. Données")
  })
  
  observeEvent(input$btn_demo, {
    bslib::nav_select(id = "navbar", selected = "1. Données")
  })
  
  # 1. Module Importation
  cleaned_data <- mod_import_server("import_module")
  
  # 2. Module Univarié
  univariate_res <- mod_univariate_server("univariate_module", data_reactive = cleaned_data)
  
  # 3. Module Bivarié
  bivariate_res <- mod_bivariate_server("bivariate_module", data_reactive = cleaned_data)
  
  # 4. Module Export Word
  mod_export_server(
    "export_module",
    data_reactive = cleaned_data,
    univar_reactive = univariate_res,
    bivar_reactive = bivariate_res
  )
}

# --- Lancement de l'application ---
shinyApp(ui = ui, server = server)
