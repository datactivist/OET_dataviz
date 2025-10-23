## PRE-RUN IMPORT DATA DASHBOARD
# Tue Oct 21 2025 ------------------------------

# Librairies
library(tidyverse)
library(glue)
library(jsonlite)
library(rrapply)


# Scraping wiki table relations -------------------------------------------


# Scrape de la page entière
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
rio::export(relation_pays, "scripts/data/id_pays_wiki.csv") 



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
    #Export
  rio::export(dataframe, paste0("scripts/data/api/", name_export_data, ".csv"))
}

# On applique la fonction pour récupérer toutes les données
  # lines
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_lines/counts", "data_line_all")
  # substations
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_substations/counts", "data_substations_all")
  # supports
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_supports/counts", "data_supports_all")



# Line length growth per country ------------------------------------------

data_line_all <- read_csv("scripts/data/api/data_line_all.csv")
