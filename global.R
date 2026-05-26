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

url_excel <- "https://ubcloud.u-bordeaux.fr/s/MToHHbgfFZjXnaQ/download"
temp_file <- tempfile(fileext = ".xlsx")
download.file(url_excel, destfile = temp_file, mode = "wb")
data_info <- read_excel(temp_file)

full_data <- inner_join(data_info, villes_coords, by = c("Villes" = "ville"))