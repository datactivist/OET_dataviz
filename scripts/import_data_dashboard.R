## PRE-RUN IMPORT DATA DASHBOARD
# Tue Oct 21 2025 ------------------------------

# Librairies
library(tidyverse)
library(glue)
library(jsonlite)
library(rrapply)
library(rvest)


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
  assign("dataframe", dataframe, envir = .GlobalEnv)
    #Export
  rio::export(dataframe, paste0("scripts/data/api/", name_export_data, ".csv"))
}

# On applique la fonction pour récupérer toutes les données
  # lines
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_lines/counts", "data_line_all")
dataframe2 <- dataframe |> 
  mutate(length = as.numeric(length) / 1000,
         labels.transmission.length = as.numeric(labels.transmission.length) / 1000,
         labels.transmission_overhead.length = as.numeric(labels.transmission_overhead.length) / 1000)
rio::export(dataframe2, "scripts/data/api/data_line_all.csv")
  # substations
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_substations/counts", "data_substations_all")
  # supports
get_all_data("https://mapyourgrid.infos-reseaux.com/projects/2025-01_supports/counts", "data_supports_all")



# Line length growth per country ------------------------------------------

# Import des données lignes de tous les pays
data_line_all <- read_csv("scripts/data/api/data_line_all.csv")

# Mise en forme pour retrouver length, growth en % et growth en km
line_length_growth <- data_line_all |> 
  filter(t >= "2025-01-01",
         Pays != "World (default)") |> 
  slice(c(1, n()), .by = Pays) |> 
  mutate(growth_percent = (length - lag(length)) / lag(length), 
         growth_km = length - lag(length),
         .by = Pays) |> 
  filter(!is.na(growth_percent)) |> 
  select(-c(amount, id_relation)) |> 
  rename(Country = Pays)

# Export
rio::export(line_length_growth, paste0("scripts/data/api/line_length_growth_table.csv"))

