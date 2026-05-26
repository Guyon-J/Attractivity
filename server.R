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
    
    active_tab <- if(input$mode == "expert") input$select_expert else NULL
    do.call(navset_tab, c(liste_panels, list(selected = active_tab)))
  })
  
  observe({
    req(current_selection() != "")
    lapply(1:nrow(full_data), function(i) {
      observeEvent(input[[paste0("comment_click_", i)]], {
        selected_row_index(i)
      })
    })
  })
  
  output$description_contact_panel <- renderUI({
    if(is.null(selected_row_index()) || current_selection() == "") {
      return(em("Sélectionnez une ville sur la carte puis cliquez sur un bloc de commentaires pour charger les informations."))
    }
    
    row_data <- full_data[selected_row_index(), ]
    
    layout_columns(
      div(
        strong(paste("Description (", row_data$Expertise, ") :"), style = "color: #d9534f; font-size: 1.1em; display:block; margin-bottom:5px;"),
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