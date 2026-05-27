# ==========================================
# Dépendances
# ==========================================

library(bslib)
library(dplyr)
library(leaflet)
library(readxl)
library(shiny)


# ==========================================
# Global
# ==========================================

villes_coords <- data.frame(
  ville = c("Amiens", "Angers", "Besançon", "Bordeaux", "Brest", "Caen", "Clermont-Ferrand", 
            "Dijon", "Grenoble", "Lille", "Limoges", "Lyon", "Marseille", "Metz", 
            "Montpellier", "Nancy", "Nantes", "Nice", "Nîmes", "Orléans", "Paris", 
            "Poitiers", "Reims", "Rennes", "Rouen", "Saint-Étienne", "Strasbourg", "Toulouse", "Tours"),
  lat = c(49.8942, 47.4784, 47.2378, 44.8378, 48.3903, 49.1828, 45.7772, 47.3220, 45.1885, 
          50.6292, 45.8336, 45.7640, 43.2965, 49.1193, 43.6108, 48.6921, 47.2184, 43.7102, 
          43.8367, 47.9030, 48.8566, 46.5802, 49.2583, 48.1173, 49.4432, 45.4397, 48.5734, 43.6047, 47.3941),
  lng = c(2.2957, -0.5632, 6.0244, -0.5792, -4.4861, -0.3707, 3.0870, 5.0415, 5.7245, 3.0573, 
          1.2611, 4.8357, 5.3698, 6.1757, 3.8767, 6.1844, -1.5536, 7.2620, 4.3601, 1.9083, 
          2.3522, 0.3404, 4.0317, -1.6778, 1.0999, 4.3872, 7.7521, 1.4442, 0.6848)
)

data_info <- read_excel("expertises.xlsx")
full_data <- inner_join(data_info, villes_coords, by = c("Villes" = "ville"))


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


# ==========================================
# SERVEUR
# ==========================================

server <- function(input, output, session) {
  
  current_selection <- reactiveVal("")
  selected_row_index <- reactiveVal(NULL)
  
  observeEvent(input$mode, { 
    current_selection("")
    selected_row_index(NULL)
    updateSelectInput(session, "select_ville", selected = "")
    updateSelectInput(session, "select_expert", selected = "")
  })
  
  observeEvent(current_selection(), { selected_row_index(NULL) })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      dragging = FALSE,           
      zoomControl = FALSE,        
      scrollWheelZoom = FALSE,    
      doubleClickZoom = FALSE,    
      touchZoom = FALSE,
      zoomSnap = 0.25,
      zoomDelta = 0.25
    )) %>%
      addTiles() %>%
      setView(lng = 2, lat = 47, zoom = 5) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) 
  })
  
  observeEvent(input$select_ville, {
    req(input$mode == "liste")
    current_selection(input$select_ville)
  })
  
  observeEvent(input$select_expert, {
    req(input$mode == "expert")
    current_selection("")
  })
  
  observeEvent(input$map_marker_click, {
    click_id <- input$map_marker_click$id
    current_selection(click_id)
    
    if(input$mode == "liste") {
      updateSelectInput(session, "select_ville", selected = click_id)
    }
  })
  
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers()
    
    if (input$mode == "liste") {
      df_to_plot <- villes_coords
    } else {
      req(input$select_expert)
      df_to_plot <- full_data %>% 
        filter(Expertise == input$select_expert) %>%
        distinct(Villes, lat, lng) %>%
        rename(ville = Villes)
    }
    
    if(nrow(df_to_plot) > 0) {
      proxy %>% addCircleMarkers(
        data = df_to_plot, lat = ~lat, lng = ~lng, layerId = ~ville, label = ~ville,
        color = ifelse(df_to_plot$ville == current_selection(), "#d9534f", "#0275d8"),
        radius = ifelse(df_to_plot$ville == current_selection(), 12, 8),
        fillOpacity = 0.8, weight = 2
      )
    }
  })
  
  output$info_panel <- renderUI({
    if(current_selection() == "") {
      return(p(em("Veuillez cliquer sur un marqueur bleu de la carte pour afficher les expertises de cette ville.")))
    }
    
    infos <- full_data %>% 
      mutate(original_id = row_number()) %>% 
      filter(Villes == current_selection())
    
    if(nrow(infos) == 0) return(p("Aucune donnée pour cette ville."))
    
    expertises_uniques <- unique(infos$Expertise)
    
    liste_panels <- lapply(expertises_uniques, function(exp) {
      lignes_sous_expertise <- infos %>% filter(Expertise == exp)
      
      boutons_commentaires <- lapply(1:nrow(lignes_sous_expertise), function(j) {
        row_actuelle <- lignes_sous_expertise[j, ]
        id_global <- row_actuelle$original_id
        
        commentaires_split <- strsplit(as.character(row_actuelle$Commentaire), ";")[[1]]
        commentaires_html <- lapply(commentaires_split, function(item) {
          item_clean <- trimws(item)
          if(item_clean != "") div(paste("•", item_clean), style = "color: #555; font-size: 0.9em;")
        })
        
        actionLink(
          inputId = paste0("comment_click_", id_global),
          label = div(
            style = "text-align: left; color: inherit;",
            strong(paste("Thématique #", j), style = "font-size: 0.85em; color: #d9534f; display:block;"),
            commentaires_html
          ),
          style = paste0(
            "display: block; padding: 10px; margin-bottom: 10px; border-left: 3px solid #666; ",
            "border-radius: 4px; text-decoration: none; transition: all 0.2s; ",
            if(!is.null(selected_row_index()) && selected_row_index() == id_global) "background: #eaeaea; border-left-color: #d9534f; font-weight: bold;" else "background: #fdfdfd;"
          )
        )
      })
      
      nav_panel(title = exp, boutons_commentaires)
    })
        
    onglet_actuel <- isolate(input$expertise_tabs_id)
    
    active_tab <- if (!is.null(onglet_actuel)) {
      onglet_actuel
    } else if (input$mode == "expert") {
      input$select_expert
    } else {
      NULL
    }
    
    do.call(navset_tab, c(liste_panels, list(id = "expertise_tabs_id", selected = active_tab)))
})
  
  output$description_contact_panel <- renderUI({
    if(is.null(selected_row_index()) || current_selection() == "") {
      return(em("Sélectionnez une ville sur la carte puis cliquez sur un bloc de commentaires pour charger les informations."))
    }
    
    row_data <- full_data[selected_row_index(), ]
    
    layout_columns(
      div(
        strong(paste(row_data$Commentaire, " (", row_data$Thématique, ")"), style = "color: #d9534f; font-size: 1.1em; display:block; margin-bottom:5px;"),
        p(
          if(!is.na(row_data$description) && row_data$description != "") row_data$description else "Aucune description disponible.",
          style = "font-size: 0.9em; color: #444; line-height: 1.4;"
        )
      ),
      div(
        strong("Contact :", style = "color: #d9534f; font-size: 1.1em; display:block; margin-bottom:5px;"),
        p(
          if(!is.na(row_data$contact) && row_data$contact != "") row_data$contact else "Aucun contact disponible.",
          style = "font-size: 0.9em; color: #444;"
        )
      ),
      col_widths = c(8, 4)
    )
  })
}

shinyApp(ui = ui, server = server)                                
