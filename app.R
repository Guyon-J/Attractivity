# ==========================================
# Dépendances
# ==========================================

library(shiny)
library(bslib)
library(leaflet)
library(visNetwork)
library(dplyr)
library(shinyjs)
library(readxl)

# ==========================================
# Global
# ==========================================

villes_coords <- data.frame(
  ville = c("Amiens", "Angers", "Besançon", "Bordeaux", "Brest", "Caen", "Clermont-Ferrand", 
            "Dijon", "Grenoble", "Lille", "Limoges", "Lyon", "Marseille", "Metz", 
            "Montpellier", "Nancy", "Nantes", "Nice", "Nîmes", "Orléans", "Paris", 
            "Poitiers", "Reims", "Rennes", "Rouen", "Saint-Etienne", "Strasbourg", "Toulouse", "Tours"),
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
# PREPARATION DES DONNEES
# ==========================================

theme_colors <- c(
  "Soin" = "#0275d8",
  "Recherche" = "#d9534f",
  "Translationnel" = "#5cb85c"
)

get_site_info <- function(row) {
  cols <- c("Site", "site", "SITE", "Site_nom", "Etablissement", "etablissement")
  for (col in cols) {
    if (col %in% names(row) && !is.null(row[[col]]) && !is.na(row[[col]]) && trimws(as.character(row[[col]])) != "") {
      return(trimws(as.character(row[[col]])))
    }
  }
  return("Non renseigné")
}

get_site_web <- function(row) {
  cols <- c("site internet", "site_internet", "site internet ", "url", "URL", "Site internet")
  for (col in cols) {
    if (col %in% names(row) && !is.null(row[[col]]) && !is.na(row[[col]]) && trimws(as.character(row[[col]])) != "") {
      return(trimws(as.character(row[[col]])))
    }
  }
  return(NULL)
}

render_detail_box <- function(row) {
  comm_val <- if(!is.na(row$Commentaire) && row$Commentaire != "") row$Commentaire else ""
  them_val <- if(!is.na(row$Thématique) && row$Thématique != "") row$Thématique else ""
  desc_val <- if(!is.na(row$description) && row$description != "") row$description else "Non renseignée."
  contact_val <- if(!is.na(row$contact) && row$contact != "") row$contact else "Non renseigné."
  
  unite_val <- if("Unité" %in% names(row) && !is.na(row$Unité) && row$Unité != "") row$Unité 
  else if("Unite" %in% names(row) && !is.na(row$Unite) && row$Unite != "") row$Unite 
  else "Non renseignée"
  
  site_info <- get_site_info(row)
  site_web <- get_site_web(row)
  
  border_col <- if(them_val %in% names(theme_colors)) theme_colors[[them_val]] else "#003366"
  
  div(
    style = paste0("background: #f8fafc; padding: 18px; margin-bottom: 12px; border-left: 5px solid ", border_col, "; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);"),
    
    if(comm_val != "") {
      p(
        tags$strong(comm_val, style = "font-size: 1.1em; color: #0f172a;"), " ",
        if(them_val != "") tags$span(style = paste0("color: ", border_col, "; font-weight: 600;"), paste0("(", them_val, ")")),
        style = "margin-bottom: 8px;"
      )
    },
    
    p(strong("Description : "), desc_val, style = "color: #334155; margin-bottom: 12px; line-height: 1.5;"),
    
    div(
      style = "font-size: 0.95em; color: #475569;",
      p(strong("Unité : "), unite_val, style = "margin-bottom: 4px;"),
      p(strong("Site : "), site_info, style = "margin-bottom: 4px;"),
      p(strong("Contact : "), contact_val, style = "margin-bottom: 12px;"),
      if(!is.null(site_web)) {
        tags$a(
          href = site_web, target = "_blank", class = "btn btn-sm",
          style = "background-color: #003366; color: #ffffff; border-color: #003366; font-weight: 600; text-decoration: none; margin-top: 4px;",
          "🌐 Site internet"
        )
      }
    )
  )
}


# ==========================================
# INTERFACE UTILISATEUR (UI)
# ==========================================

ui <- page_navbar(
  id = "main_nav",
  title = "Collège National de Biochimie-Biologie Moléculaire Médicale",
  theme = bs_theme(bootswatch = "journal"),
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .home-title { color: #003366; font-weight: 800; font-size: 34px; margin-bottom: 35px; }
      .home-card-btn {
        width: 100%; height: 150px; font-size: 24px; font-weight: 700;
        border-radius: 14px; margin-bottom: 20px; transition: all 0.3s ease;
        display: flex; flex-direction: column; justify-content: center; align-items: center;
        background-color: #1e293b; color: #ffffff; border: 2px solid #334155;
        box-shadow: 0 6px 12px -2px rgba(0, 0, 0, 0.25);
      }
      .home-card-btn:hover { 
        background-color: #0f172a; color: #38bdf8; transform: translateY(-4px); border-color: #38bdf8;
      }
      
      .leaflet-container { height: 600px !important; width: 100%; border-radius: 8px; }
      .map-fixed-card { height: 670px !important; overflow: hidden; }
      
      .thema-btn {
        width: 140px; height: 140px; border-radius: 50%; font-size: 17px; font-weight: bold;
        border: 2px solid #cbd5e1; color: #475569; background-color: #e2e8f0; 
        transition: all 0.3s ease; margin: 10px auto; display: block;
      }
      .thema-btn.active-soin { background-color: #0275d8 !important; color: white !important; border-color: #0275d8 !important; }
      .thema-btn.active-recherche { background-color: #d9534f !important; color: white !important; border-color: #d9534f !important; }
      .thema-btn.active-trans { background-color: #5cb85c !important; color: white !important; border-color: #5cb85c !important; }
      
      .grid-3-columns {
        display: grid; grid-template-columns: repeat(3, 1fr); gap: 20px; margin-top: 15px;
      }
      @media (max-width: 900px) { .grid-3-columns { grid-template-columns: repeat(1, 1fr); } }
      
      .city-card {
        transition: transform 0.2s, box-shadow 0.2s;
        border: 1px solid #cbd5e1; border-top: 5px solid #003366; background: #ffffff; border-radius: 10px; overflow: hidden;
        display: flex; flex-direction: column; justify-content: space-between;
      }
      .city-card:hover { transform: translateY(-4px); box-shadow: 0 10px 20px rgba(0,0,0,0.1); }
      .city-card-img { width: 100%; height: 140px; object-fit: cover; background-color: #cbd5e1; }
      
      .expertise-badge-btn {
        display: inline-flex; align-items: center; gap: 8px;
        padding: 12px 20px; margin: 8px; border-radius: 12px;
        background-color: #1e293b; color: #ffffff; font-weight: 700; font-size: 15px;
        border: 2px solid #334155; transition: all 0.3s ease; cursor: pointer;
        box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1);
      }
      .expertise-badge-btn:hover {
        background-color: #0f172a; color: #38bdf8; border-color: #38bdf8;
        transform: translateY(-3px); box-shadow: 0 6px 12px rgba(0,0,0,0.25);
      }
      
      .city-card-img-wrapper {
        position: relative;
        width: 100%;
        height: 140px;
      }
      .city-card-img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        background-color: #cbd5e1;
      }
      .photo-credit-badge {
        position: absolute;
        bottom: 8px;
        right: 8px;
        background-color: rgba(255, 255, 255, 0.92);
        color: #0f172a;
        padding: 3px 8px;
        border-radius: 6px;
        font-size: 11px;
        font-weight: 600;
        box-shadow: 0 2px 4px rgba(0,0,0,0.15);
        pointer-events: none;
        max-width: 85%;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }
    "))
  ),
  
  # ACCUEIL
  nav_panel("Accueil", value = "page_accueil",
            div(style = "text-align: center; padding: 50px 20px;",
                h2("Carte nationale des expertises en Biochimie et Biologie Moléculaire", class = "home-title"),
                layout_column_wrap(
                  width = 1/2,
                  actionButton("go_villes", "🏙️ Villes", class = "home-card-btn"),
                  actionButton("go_thema", "🎯 Thématiques", class = "home-card-btn"),
                  actionButton("go_expert", "🔬 Expertises", class = "home-card-btn"),
                  actionButton("go_carte", "🗺️ Carte interactive", class = "home-card-btn"),
                )
            )
  ),
  
  # VILLES
  nav_panel("Villes", value = "page_villes",
            h3("Recherche par ville", style = "color: #003366; font-weight: bold; margin-bottom: 20px;"),
            uiOutput("villes_grid")
  ),
  
  # THÉMATIQUES
  nav_panel("Thématiques", value = "page_thema",
            div(style = "text-align: center; margin-bottom: 25px;",
                h3("Sélectionnez une ou plusieurs thématiques", style = "color: #003366; font-weight: bold;"),
                layout_column_wrap(
                  width = 1/3,
                  actionButton("btn_soin", "Soin", class = "thema-btn"),
                  actionButton("btn_recherche", "Recherche", class = "thema-btn"),
                  actionButton("btn_trans", "Translationnel", class = "thema-btn")
                )
            ),
            hr(),
            uiOutput("thematique_results")
  ),
  
  # EXPERTISES
  nav_panel("Expertises", value = "page_expert",
            h3("Répertoire des Expertises", style = "color: #003366; font-weight: bold; margin-bottom: 10px;"),
            div(style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 6px; padding: 20px 10px;",
                uiOutput("expertises_badges")
            )
  ),
  
  # CARTE
  nav_panel("Carte", value = "page_carte",
            div(style = "padding: 15px;",
                layout_column_wrap(
                  width = 1/2,
                  card(
                    class = "map-fixed-card",
                    card_header("Carte interactive", style = "color: #003366; font-weight: bold; font-size: 18px;"),
                    leafletOutput("map", height = "600px")
                  ),
                  card(
                    class = "map-fixed-card",
                    card_header(uiOutput("map_right_header"), style = "color: #003366; font-weight: bold; font-size: 18px;"),
                    div(style = "height: 600px; overflow-y: auto;", uiOutput("map_right_content"))
                  )
                )
            )
  )
)

# ==========================================
# SERVEUR
# ==========================================

server <- function(input, output, session) {
  
  selected_city_map <- reactiveVal(NULL)
  
  # Navigation
  observeEvent(input$go_carte, { nav_select("main_nav", "page_carte") })
  observeEvent(input$go_villes, { nav_select("main_nav", "page_villes") })
  observeEvent(input$go_thema, { nav_select("main_nav", "page_thema") })
  observeEvent(input$go_expert, { nav_select("main_nav", "page_expert") })
  
  # -----------------------------------------------------------
  # CARTE
  # -----------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = FALSE, scrollWheelZoom = FALSE,
      doubleClickZoom = FALSE, touchZoom = FALSE,
      zoomSnap = 0.1, zoomDelta = 0.5
    )) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = 2.2137, lat = 46.2276, zoom = 5.7) %>% 
      addCircleMarkers(
        data = full_data %>% distinct(Villes, lat, lng), 
        lat = ~lat, lng = ~lng, layerId = ~Villes, label = ~Villes, group = "city_markers",
        color = "#003366", fillColor = "#0275d8", radius = 10, fillOpacity = 0.8, weight = 2
      )
  })
  
  observeEvent(input$map_marker_click, {
    click_id <- input$map_marker_click$id
    selected_city_map(click_id)
    
    shinyjs::runjs("Shiny.setInputValue('current_node_click', null);")
    
    cities_df <- full_data %>% distinct(Villes, lat, lng)
    
    leafletProxy("map") %>%
      clearGroup("city_markers") %>%
      addCircleMarkers(
        data = cities_df,
        lat = ~lat, lng = ~lng, layerId = ~Villes, label = ~Villes, group = "city_markers",
        color = ifelse(cities_df$Villes == click_id, "#d9534f", "#003366"),
        fillColor = ifelse(cities_df$Villes == click_id, "#d9534f", "#0275d8"),
        radius = ifelse(cities_df$Villes == click_id, 13, 10),
        fillOpacity = ifelse(cities_df$Villes == click_id, 1.0, 0.8),
        weight = ifelse(cities_df$Villes == click_id, 3, 2)
      )
  })
  
  observeEvent(input$reset_node_filter, {
    shinyjs::runjs("Shiny.setInputValue('current_node_click', null);")
  })
  
  output$map_right_header <- renderUI({
    v <- selected_city_map()
    if(is.null(v)) return("Réseau & Détails")
    span(v)
  })
  
  output$map_right_content <- renderUI({
    v <- selected_city_map()
    if(is.null(v)) {
      return(div(
        style = "text-align: center; color: #64748b; padding: 60px 20px;",
        h4("🗺️ Sélectionnez une ville"),
        p("Cliquez sur un marqueur de la carte pour afficher son réseau d'expertises et ses détails.")
      ))
    }
    
    div(
      visNetworkOutput("network_ville", height = "280px"),
      hr(style = "margin: 10px 0;"),
      uiOutput("network_details")
    )
  })
  
  output$network_ville <- renderVisNetwork({
    ville_cliquee <- selected_city_map()
    req(ville_cliquee)
    
    df_ville <- full_data %>% filter(Villes == ville_cliquee)
    expertises <- unique(df_ville$Expertise)
    
    nodes <- data.frame(
      id = c(ville_cliquee, expertises),
      label = c(ville_cliquee, expertises),
      shape = c("box", rep("dot", length(expertises))),
      color = list(
        background = c("#003366", rep("#e2e8f0", length(expertises))),
        border = c("#001a33", rep("#cbd5e1", length(expertises))),
        highlight = "#38bdf8"
      ),
      font = list(
        color = c("white", rep("black", length(expertises))),
        size = c(20, rep(16, length(expertises))),
        face = "bold"
      ),
      shapeProperties = list(borderRadius = 8),
      size = c(32, rep(20, length(expertises)))
    )
    
    edges <- data.frame(
      from = ville_cliquee, to = expertises,
      color = list(color = "#cbd5e1", highlight = "#0275d8"), width = 2
    )
    
    visNetwork(nodes, edges) %>%
      visNodes(shadow = TRUE) %>%
      visEdges(smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)) %>%
      visOptions(highlightNearest = TRUE) %>%
      visInteraction(zoomView = FALSE, dragView = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visEvents(selectNode = "function(nodes) { Shiny.setInputValue('current_node_click', nodes.nodes[0]); }")
  })
  
  output$network_details <- renderUI({
    ville_cliquee <- selected_city_map()
    req(ville_cliquee)
    
    infos <- full_data %>% filter(Villes == ville_cliquee)
    noeud <- input$current_node_click
    
    # Si un nœud d'expertise précis est sélectionné (et que ce n'est pas le nœud ville)
    if(!is.null(noeud) && !is.na(noeud) && length(noeud) > 0 && noeud != ville_cliquee && noeud %in% infos$Expertise) {
      infos <- infos %>% filter(Expertise == noeud)
      header_info <- div(
        style = "background-color: #e0f2fe; border: 1px solid #bae6fd; color: #0369a1; padding: 8px 12px; border-radius: 6px; margin-bottom: 12px; font-size: 0.9em; display: flex; justify-content: space-between; align-items: center;",
        span(strong("Filtre actif : "), noeud),
        actionLink("reset_node_filter", "Tout afficher ✖️", style = "color: #0284c7; font-weight: bold; text-decoration: none;")
      )
    } else {
      header_info <- p(
        em(paste0("Toutes les fiches pour ", ville_cliquee, " (cliquez sur un nœud d'expertise pour filtrer) :")),
        style = "color: #64748b; margin-bottom: 12px; font-size: 0.9em;"
      )
    }
    
    if(nrow(infos) == 0) return(p("Aucune information disponible pour cette sélection.", style = "color: #666;"))
    
    tagList(
      header_info,
      lapply(1:nrow(infos), function(i) render_detail_box(infos[i, ]))
    )
  })
  
  # -----------------------------------------------------------
  # VILLES
  # -----------------------------------------------------------
  output$villes_grid <- renderUI({
    villes_list <- sort(unique(full_data$Villes))
    
    div(
      class = "grid-3-columns",
      lapply(villes_list, function(v) {
        records <- full_data %>% filter(Villes == v)
        exp_list <- unique(records$Expertise)
        
        # Vérification de l'existence du fichier local dans www/villes/
        rel_img_path <- paste0("villes/", v, ".jpg")
        full_img_path <- paste0("www/", rel_img_path)
        
        img_url <- if (file.exists(full_img_path)) rel_img_path else "villes/default.jpg"
        
        # Récupération du crédit photo
        credit_text <- if (v %in% names(credits_photos)) credits_photos[[v]] else "📷 Droits réservés"
        
        div(
          class = "city-card",
          
          # Conteneur Image + Badge de crédit
          div(
            class = "city-card-img-wrapper",
            tags$img(src = img_url, class = "city-card-img", alt = v),
            div(class = "photo-credit-badge", credit_text)
          ),
          
          # Contenu de la carte
          div(
            style = "padding: 15px; display: flex; flex-direction: column; flex-grow: 1;",
            h5(v, style = "color: #003366; font-weight: bold; margin-bottom: 10px; font-size: 1.1em;"),
            p(strong("Expertises :"), style = "margin-bottom: 5px; color: #475569;"),
            tags$ul(
              style = "padding-left: 20px; margin-bottom: 15px; font-size: 0.9em; flex-grow: 1;",
              lapply(head(exp_list, 3), function(e) tags$li(e)),
              if(length(exp_list) > 3) tags$li(em(paste("+", length(exp_list) - 3, "autre(s)")))
            ),
            tags$button(
              class = "btn w-100 mt-auto",
              style = "background-color: #003366; color: #ffffff; border-color: #003366; font-weight: 600;",
              onclick = sprintf("Shiny.setInputValue('clicked_ville_card', '%s', {priority: 'event'})", v),
              "Voir détails"
            )
          )
        )
      })
    )
  })
  
  observeEvent(input$clicked_ville_card, {
    v <- input$clicked_ville_card
    items <- full_data %>% filter(Villes == v)
    
    showModal(modalDialog(
      title = h3(v, style = "color: #003366; font-weight: bold; margin: 0;"),
      size = "l", easyClose = TRUE,
      lapply(1:nrow(items), function(i) render_detail_box(items[i, ]))
    ))
  })
  
  # -----------------------------------------------------------
  # THÉMATIQUES
  # -----------------------------------------------------------
  selected_themes <- reactiveVal(character(0))
  
  observeEvent(input$btn_soin, {
    curr <- selected_themes()
    if ("Soin" %in% curr) selected_themes(setdiff(curr, "Soin")) else selected_themes(c(curr, "Soin"))
  })
  observeEvent(input$btn_recherche, {
    curr <- selected_themes()
    if ("Recherche" %in% curr) selected_themes(setdiff(curr, "Recherche")) else selected_themes(c(curr, "Recherche"))
  })
  observeEvent(input$btn_trans, {
    curr <- selected_themes()
    if ("Translationnel" %in% curr) selected_themes(setdiff(curr, "Translationnel")) else selected_themes(c(curr, "Translationnel"))
  })
  
  observe({
    themes <- selected_themes()
    shinyjs::toggleClass("btn_soin", "active-soin", condition = "Soin" %in% themes)
    shinyjs::toggleClass("btn_recherche", "active-recherche", condition = "Recherche" %in% themes)
    shinyjs::toggleClass("btn_trans", "active-trans", condition = "Translationnel" %in% themes)
  })
  
  output$thematique_results <- renderUI({
    themes <- selected_themes()
    if(length(themes) == 0) {
      return(div(style="text-align:center; color:#64748b; padding: 40px;", em("Sélectionnez au moins une thématique ci-dessus pour afficher le tableau.")))
    }
    
    df_filtered <- full_data %>% 
      filter(Thématique %in% themes) %>% 
      arrange(Villes, Expertise)
    
    if(nrow(df_filtered) == 0) return(p("Aucun résultat trouvé pour cette sélection."))
    
    villes_rle <- rle(as.character(df_filtered$Villes))
    row_is_start <- rep(FALSE, nrow(df_filtered))
    row_span_len <- rep(1, nrow(df_filtered))
    
    idx <- 1
    for(k in seq_along(villes_rle$lengths)) {
      len <- villes_rle$lengths[k]
      row_is_start[idx] <- TRUE
      row_span_len[idx] <- len
      idx <- idx + len
    }
    
    rows_html <- list()
    for(i in 1:nrow(df_filtered)) {
      row_data <- df_filtered[i, ]
      comm_text <- if(!is.na(row_data$Commentaire) && row_data$Commentaire != "") row_data$Commentaire else "-"
      
      btn_more <- tags$button(
        class = "btn btn-sm",
        style = "background-color: #003366; color: #ffffff; border-color: #003366; font-weight: 600;",
        onclick = sprintf("Shiny.setInputValue('clicked_thema_table_row', %d, {priority: 'event'})", i),
        "En savoir plus 🔍"
      )
      
      if(row_is_start[i]) {
        td_ville <- tags$td(
          rowspan = row_span_len[i],
          style = "vertical-align: middle; font-weight: bold; color: #003366; background-color: #f8fafc; font-size: 1.05em;",
          row_data$Villes
        )
        rows_html[[i]] <- tags$tr(
          td_ville,
          tags$td(style = "vertical-align: middle; font-weight: 600;", row_data$Expertise),
          tags$td(style = "vertical-align: middle; color: #334155;", comm_text),
          tags$td(style = "vertical-align: middle; text-align: center;", btn_more)
        )
      } else {
        rows_html[[i]] <- tags$tr(
          tags$td(style = "vertical-align: middle; font-weight: 600;", row_data$Expertise),
          tags$td(style = "vertical-align: middle; color: #334155;", comm_text),
          tags$td(style = "vertical-align: middle; text-align: center;", btn_more)
        )
      }
    }
    
    div(
      style = "overflow-x: auto; margin-top: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08);",
      tags$table(
        class = "table table-hover table-bordered bg-white mb-0",
        tags$thead(
          class = "table-light",
          tags$tr(
            tags$th("Ville", style = "width: 20%; color: #003366; font-size: 1.05em;"),
            tags$th("Expertise", style = "width: 25%; color: #003366; font-size: 1.05em;"),
            tags$th("Commentaire", style = "width: 40%; color: #003366; font-size: 1.05em;"),
            tags$th("Action", style = "width: 15%; text-align: center; color: #003366; font-size: 1.05em;")
          )
        ),
        tags$tbody(rows_html)
      )
    )
  })
  
  observeEvent(input$clicked_thema_table_row, {
    idx <- input$clicked_thema_table_row
    themes <- selected_themes()
    
    df_filtered <- full_data %>% 
      filter(Thématique %in% themes) %>% 
      arrange(Villes, Expertise)
    
    req(idx <= nrow(df_filtered))
    row_data <- df_filtered[idx, ]
    
    showModal(modalDialog(
      title = h3(paste(row_data$Villes, "-", row_data$Expertise), style = "color: #003366; font-weight: bold; margin: 0;"),
      size = "m", easyClose = TRUE,
      render_detail_box(row_data)
    ))
  })
  
  # -----------------------------------------------------------
  # EXPERTISES
  # -----------------------------------------------------------
  output$expertises_badges <- renderUI({
    all_expertises <- sort(unique(full_data$Expertise))
    
    lapply(all_expertises, function(exp) {
      tags$button(
        class = "expertise-badge-btn",
        onclick = sprintf("Shiny.setInputValue('clicked_expertise_badge', '%s', {priority: 'event'})", gsub("'", "\\\\'", exp)),
        tags$span("🔬"), exp
      )
    })
  })
  
  observeEvent(input$clicked_expertise_badge, {
    exp <- input$clicked_expertise_badge
    items <- full_data %>% filter(Expertise == exp)
    villes_in_exp <- unique(items$Villes)
    
    showModal(modalDialog(
      title = h3(exp, style = "color: #003366; font-weight: bold; margin: 0;"),
      size = "l", easyClose = TRUE,
      
      lapply(villes_in_exp, function(v_name) {
        sub_items <- items %>% filter(Villes == v_name)
        div(
          style = "margin-bottom: 20px;",
          h4(paste("🏙️", v_name), style = "color: #003366; font-weight: bold; border-bottom: 2px solid #e2e8f0; padding-bottom: 5px; margin-bottom: 12px;"),
          lapply(1:nrow(sub_items), function(i) render_detail_box(sub_items[i, ]))
        )
      })
    ))
  })
  
  
  # -----------------------------------------------------------
  # Crédit photo
  # -----------------------------------------------------------
  
  credits_photos <- c(
    "Amiens"           = "📷 Photo : Alex",
    "Angers"           = "📷 Photo : P. Bernardon",
    "Besançon"         = "📷 Photo : L. Lemoine",
    "Bordeaux"         = "📷 Photo : J. Di Nella",
    "Brest"            = "📷 Photo : P. Goiffon",
    "Caen"             = "📷 Photo : J. Garratt",
    "Clermont-Ferrand" = "📷 Photo : P. Dome",
    "Dijon"            = "📷 Photo : D. Tischer",
    "Grenoble"         = "📷 Photo : M. Wieland",
    "Lille"            = "📷 Photo : T. Zielonka",
    "Limoges"          = "📷 Photo : S. Wander",
    "Lyon"             = "📷 Photo : D. H. N. Nguyen",
    "Marseille"        = "📷 Photo : E. Schmidt",
    "Metz"             = "📷 Photo : D. Grandmougin",
    "Montpellier"      = "📷 Photo : H. J. Rivas",
    "Nancy"            = "📷 Photo : G. Griffay",
    "Nantes"           = "📷 Photo : H. Carle",
    "Nice"             = "📷 Photo : Constantin",
    "Nîmes"            = "📷 Photo : ",
    "Orléans"          = "📷 Photo : R. Peillon",
    "Paris"            = "📷 Photo : C. Karidis",
    "Poitiers"         = "📷 Photo : P. Farjam",
    "Reims"            = "📷 Photo : Adlan",
    "Rennes"           = "📷 Photo : T. Ly",
    "Rouen"            = "📷 Photo : J. Desplanques",
    "Saint-Etienne"    = "📷 Photo : D. Leveque",
    "Strasbourg"       = "📷 Photo : Y. Deshko",
    "Toulouse"         = "📷 Photo : K. Bessat",
    "Tours"            = "📷 Photo : P. Bernardon"
  )  
}

shinyApp(ui = ui, server = server)                                
