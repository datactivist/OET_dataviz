## PRE-RUN IMPORT DATA DASHBOARD

# Librairies
message("Import des librairies")
library(tidyverse)
library(glue)
library(jsonlite)
library(rrapply)
library(rvest)
library(sf)

# Copie données carto Colombia si besoin
message("Copie données carto Colombia si besoin")
volume_dir <- "/data"
static_dir <- "/home/app/static_data"
static_files <- list.files(static_dir, pattern = "\\.geojson$", full.names = FALSE)

for (file in static_files) {
  src <- file.path(static_dir, file)
  dest <- file.path(volume_dir, file)

  if (!file.exists(dest)) {
    file.copy(src, dest, overwrite = FALSE)
  }
}



# Scraping wiki table relations -------------------------------------------


# Scrape de la page entière
message("Scrape de la page wiki pour récup id pays")
content <- read_html("https://wiki.openstreetmap.org/wiki/OpenHistoricalMap/Countries")
body_table <- content |> html_nodes('body')  |>
                    html_nodes('table') |>
                    html_table(dec = ",") 
# Extraction de la table et formatage 
relation_pays <- body_table[[1]] |> 
  select(Name, `OSM boundary=administrative (latest)`) |> 
  rename(Pays = Name,
         id_relation = `OSM boundary=administrative (latest)`) |> 
  mutate(id_relation = paste0("/boundary/", str_extract(id_relation, "\\d+"))) |> 
  add_row(Pays = "World (default)", id_relation = "", .before = 1)

# Export
rio::export(relation_pays, "/data/id_pays_wiki.csv") 



# Import des données pour chaque pays -------------------------------------


# Fonction pour récupérer les données de chaque mesure
get_all_data <- function(url, name_export_data){
    #Appels API
  data <- purrr::map(
          .x = relation_pays$id_relation,
          .y = data.frame(matrix(ncol = 1, nrow = 1)),
          possibly(.f = ~fromJSON(txt = paste0(url, .x), flatten = T)$data |> mutate(id_relation = .x), otherwise = NA_character_),
          .default = NA)
    #Compilation
  data <- data[data !=  "NA"] # replace NA (DOIs non matchés avec OpenAlex) by NULL
  data <- rrapply(data, condition = Negate(is.null), how = "prune") #remove NULL
  dataframe <- data |> 
    bind_rows() |> 
    left_join(relation_pays, by = "id_relation")
  assign("dataframe", dataframe, envir = .GlobalEnv)
    #Export
  rio::export(dataframe, paste0("/data/api/", name_export_data, ".csv"))
}

# On applique la fonction pour récupérer toutes les données
  #--- COUNTS
  # lines
message("Appels API counts/lines")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_lines/counts", "data_lines_all")
message("Length /1000 données counts/lines")
dataframe2 <- dataframe |> 
  mutate(length = as.numeric(length) / 1000,
         labels.transmission.length = as.numeric(labels.transmission.length) / 1000,
         labels.transmission_overhead.length = as.numeric(labels.transmission_overhead.length) / 1000)
rio::export(dataframe2, "/data/api/data_lines_all.csv")
  # substations
message("Appels API counts/substations")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_substations/counts", "data_substations_all")
  # supports
message("Appels API counts/supports")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_supports/counts", "data_supports_all")

  #--- MAPPERS
  # lines
message("Appels API mappers/lines")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_lines/mappers", "mappers_lines_all")
  # substations
message("Appels API mappers/substations")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_substations/mappers", "mappers_substations_all")
  # supports
message("Appels API mappers/supports")
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_supports/mappers", "mappers_supports_all")



# Line length growth per country ------------------------------------------

# Import des données lignes de tous les pays
message("Import données counts/lines")
data_line_all <- read_csv("/data/api/data_lines_all.csv")

# Mise en forme pour retrouver length, growth en % et growth en km
  # Early OET
message("Calcul growth km et % par période - early OET")
line_length_growth_earlyOET <- data_line_all |> 
  filter(t >= "2024-11-01",
         Pays != "World (default)") |> 
  slice(c(1, n()), .by = Pays) |> 
  mutate(growth_percent = (labels.transmission.length - lag(labels.transmission.length)) / lag(labels.transmission.length), 
         growth_km = labels.transmission.length - lag(labels.transmission.length),
         .by = Pays) |> 
  filter(!is.na(growth_percent)) |> 
  select(t, Pays, labels.transmission.length, growth_percent, growth_km) |> 
  rename(Country = Pays)
  # Kickoff
message("Calcul growth km et % par période - Kickoff")
line_length_growth_kickoff <- data_line_all |> 
  filter(t >= "2025-03-01",
         Pays != "World (default)") |> 
  slice(c(1, n()), .by = Pays) |> 
  mutate(growth_percent = (labels.transmission.length - lag(labels.transmission.length)) / lag(labels.transmission.length), 
         growth_km = labels.transmission.length - lag(labels.transmission.length),
         .by = Pays) |> 
  filter(!is.na(growth_percent)) |> 
  select(t, Pays, labels.transmission.length, growth_percent, growth_km) |> 
  rename(Country = Pays)
  # Public launch
message("Calcul growth km et % par période - Public launch")
line_length_growth_publicLaunch <- data_line_all |> 
  filter(t >= "2025-08-01",
         Pays != "World (default)") |> 
  slice(c(1, n()), .by = Pays) |> 
  mutate(growth_percent = (labels.transmission.length - lag(labels.transmission.length)) / lag(labels.transmission.length), 
         growth_km = labels.transmission.length - lag(labels.transmission.length),
         .by = Pays) |> 
  filter(!is.na(growth_percent)) |> 
  select(t, Pays, labels.transmission.length, growth_percent, growth_km) |> 
  rename(Country = Pays)

# Tous ensemble
message("Calcul growth km et % par période - All")
line_length_growth <- line_length_growth_earlyOET |> 
  rename_at(vars(-Country, -t, -labels.transmission.length), ~paste0(., "_earlyOET")) |> 
  left_join(line_length_growth_kickoff |> 
              select(-t, -labels.transmission.length) |> 
              rename_at(vars(-Country), ~paste0(., "_kickoff")), 
            by = "Country") |> 
  left_join(line_length_growth_publicLaunch |> 
              select(-t, -labels.transmission.length) |> 
              rename_at(vars(-Country), ~paste0(., "_publicLaunch")), 
            by = "Country")
                               

# Export
rio::export(line_length_growth, paste0("/data/api/line_length_growth_table2.csv"))

