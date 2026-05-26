# ==========================================
# INTERFACE UTILISATEUR
# ==========================================

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "journal"),
  title = "Collège National de Biochimie-Biologie Moléculaire Médicale - Carte des thématiques de recherche",
  sidebar = sidebar(
    title = "Biochimie",
    radioButtons("mode", "Mode de recherche :",
                 choices = list("Par ville" = "liste", "Par expertise" = "expert")),
    
    conditionalPanel(
      condition = "input.mode == 'liste'",
      selectInput("select_ville", "Choisir une ville :", 
                  choices = c("Sélectionner..." = "", sort(unique(villes_coords$ville))))
    ),
    
    conditionalPanel(
      condition = "input.mode == 'expert'",
      selectInput("select_expert", "Domaine d'expertise :", 
                  choices = c("Sélectionner..." = "", sort(unique(full_data$Expertise))))
    )
  ),
  
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Carte interactive"), 
      leafletOutput("map", height = 600)
    ),
    card(
      card_header("Détails par expertise"), 
      style = "height: 660px;", 
      card_body(
        uiOutput("info_panel"),
        fillable = TRUE
      )
    ),
    card(
      card_header("Description & Contact"),
      style = "height: 180px;", 
      card_body(
        uiOutput("description_contact_panel"),
        fillable = TRUE
      )
    ),
    col_widths = c(5, 7, 12)
  )
)