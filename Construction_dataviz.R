### Indicateurs en dataviz pour aperçu général des contributions du projet MapYouGrid

# Librairies
library(tidyverse)
library(jsonlite)
library(geojsonR)
library(httr)
library(sf)
library(readxl)
library(curl)
library(gt)
library(gtExtras)
library(janitor)

# Data
  # JSON
comunity_stats <- fromJSON("https://raw.githubusercontent.com/open-energy-transition/MapYourGrid/refs/heads/main/docs/data/community-stats.json", 
                 flatten = TRUE)
line_lenght <- fromJSON("https://raw.githubusercontent.com/open-energy-transition/MapYourGrid/refs/heads/main/docs/data/line-length.json", 
                 flatten = TRUE)
power_stat <- fromJSON("https://raw.githubusercontent.com/open-energy-transition/MapYourGrid/refs/heads/main/docs/data/power-stats.json", 
                 flatten = TRUE)
  # GEOSJON
    # countries
temp_file <- tempfile(fileext = ".geojson")
GET("https://raw.githubusercontent.com/open-energy-transition/MapYourGrid/refs/heads/main/docs/data/countries.geojson", 
    write_disk(temp_file, overwrite = TRUE))
countries <- st_read(temp_file, quiet = TRUE)
    # regionsv2
GET("https://raw.githubusercontent.com/open-energy-transition/MapYourGrid/refs/heads/main/docs/data/regionsv2.geojson", 
    write_disk(temp_file, overwrite = TRUE))
regionsv2 <- st_read(temp_file, quiet = TRUE)
  # XLSX
destfile <- "GEM_Global_Integrated_Power_February_2025_update_II.xlsx"
curl_download("https://github.com/open-energy-transition/MapYourGrid/raw/refs/heads/main/docs/data/GEM-Global-Integrated-Power-February-2025-update-II.xlsx", destfile)
global_data <- read_excel(destfile, sheet = "Power facilities")
  # CSV
line_lenght_country <- read_csv("line_lenght_growth_country_data.csv") |> 
  #on remet growth % en numérique
  mutate(`Growth since 2025-01-01 (%)` = as.numeric(str_sub(`Growth since 2025-01-01 (%)`, 1, nchar(`Growth since 2025-01-01 (%)`)-1)) / 100)

# Observation données
nepal_countries <- countries |> 
  filter(NAME == "Nepal") |> 
  t()
nepal_global_data <- global_data |> 
  filter(`Country/area` == "Nepal")



###########################################################
############# Community Mapping Progress ##################
###########################################################




###########################################################
############# Line Length Growth per Country ##############
###########################################################

# Préparation des données

  #chargement du dataset countrypops (codes des pays)
data("countrypops")
countrypops <- countrypops |> 
  distinct(country_name, country_code_2, country_code_3) |> 
  mutate(country_name = tolower(str_replace(country_name, "&", "and"))) 

  #jointure avec nos données pour récupérer le code de pays 
line_lenght_country_prep <- line_lenght_country |> 
  mutate(Country2 = tolower(Country), #minuscules pour otpimiser le match
         Country2 = case_match(Country2, #remplace à la main des pays non matchés
                               "people's republic of china" ~ "china",
                               "ivory coast" ~ "cote d'ivoire",
                               "state of palestine" ~ "palestine",
                               "democratic republic of the congo" ~ "congo (drc)",
                               "republic of the congo" ~ "congo (republic)",
                               "the bahamas" ~ "bahamas",
                               "the gambia" ~ "gambia",
                               "timor-leste" ~ "east timor",
                               "federated states of micronesia" ~ "micronesia",
                               "são tomé and príncipe" ~ "sao tome and principe",
                               "kingdom of the netherlands" ~ "netherlands",
                               .default = Country2)) |> 
  left_join(countrypops, by = c("Country2" = "country_name")) |> 
  na.omit() |> 
  #mise en forme df
  select(-Country2) |> 
  relocate(country_code_2, .before = Country) |> 
  rename(` ` = country_code_2)

# Affichage de la table
table_line_country <- line_lenght_country_prep |> 
  arrange(Country) |> 
  gt() |> 
  #drapeaux par pays
  fmt_flag(columns = ` `) |> 
  #format colonne en %
  fmt_percent(columns = `Growth since 2025-01-01 (%)`,
            decimals = 1, drop_trailing_zeros = TRUE,
            dec_mark = ",") |> 
  #coloration des évolutions en %
  data_color(columns = `Growth since 2025-01-01 (%)`,
             rows = `Growth since 2025-01-01 (%)` != 0,
             method = "bin",
             apply_to = "text",
             palette = c("#bf2f2f", "black", "#279f2b"),
             bins = c(-1, 0, 1)) |> 
  #coloration des évolutions en valeurs absolues
  data_color(columns = `Growth since 2025-01-01 (km)`,
             rows = `Growth since 2025-01-01 (%)` > 0,
             method = "numeric",
             palette = c("#FECF5D", "#279f2b"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
  data_color(columns = `Growth since 2025-01-01 (km)`,
             rows = `Growth since 2025-01-01 (%)` < 0,
             method = "numeric",
             palette = c("#bf2f2f", "#ffa500"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
  data_color(columns = `Growth since 2025-01-01 (km)`,
             rows = `Growth since 2025-01-01 (%)` == 0,
             method = "numeric",
             palette = c("grey80"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
             
             # method = "numeric", 
             # colors = scales::col_numeric(palette = c("#d73027","#FECF5D", "#ffa500", "#279f2b"), 
             #                              domain = NULL)) |> 
  #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "left"),
            locations = cells_body(is.numeric)) |> 
  #noms de pays en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Country)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "left"),
                         cell_fill("grey80")),
            locations = cells_column_labels()) |> 
#bordures en blanc
  tab_options(table_body.hlines.style = "solid",
              table_body.hlines.width = 10, 
              table_body.hlines.color = "white") |> 
  #opt_table_lines("none") |> 
  #taille colonnes
  cols_width(` ` ~ px(30)) |> 
  #intéractivité table
  opt_interactive(use_search = TRUE,
                  page_size_default = 25) |> 
  #cache colonne
  cols_hide(country_code_3)
table_line_country
gtsave(table_line_country, "figures/table_line_country.html")


###########################################################
############# Line Length Growth per Continent ############
###########################################################


# Préparation des données

  #chargement des données des pays avec continent correspondant
continent_data <- read_delim("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/world-administrative-boundaries/exports/csv/?delimiters=%3B&lang=fr&timezone=Europe%2FParis&use_labels=true", ";") |> 
  distinct(`ISO 3 territory code`, `Continent of the territory`) |> 
  clean_names()

  #jointure avec nos données pour récupérer le code de pays 
line_lenght_country_prep2 <- line_lenght_country_prep |> 
  left_join(continent_data, by = c("country_code_3" = "iso_3_territory_code")) |> 
    #agrégation par continent
  summarise(nb_pays = n(),
            mean_power_line = round(mean(`Power line length (km) 2025-07-01`), 0),
            min_power_line = min(`Power line length (km) 2025-07-01`),
            max_power_line = max(`Power line length (km) 2025-07-01`),
            sum_power_line = sum(`Power line length (km) 2025-07-01`),
            min_growth_percent = min(`Growth since 2025-01-01 (%)`),
            max_growth_percent = max(`Growth since 2025-01-01 (%)`),
            mean_growth_percent = mean(`Growth since 2025-01-01 (%)`),
            min_growth_km = min(`Growth since 2025-01-01 (km)`),
            max_growth_km = max(`Growth since 2025-01-01 (km)`),
            mean_growth_km = mean(`Growth since 2025-01-01 (km)`),
            pays = paste(Country, collapse = ", "),
            code_pays = paste(` `, collapse = ","),
            .by = continent_of_the_territory) |> 
      #mise en forme des stats
  mutate(across(where(is.numeric) & !all_of(c("mean_power_line", "mean_growth_percent", 
                                              "mean_growth_km", "min_growth_percent", "max_growth_percent")), 
                ~ format(as.integer(., 0), nsmall = 1, big.mark = ".")))
  #préparation liste déroulante de spays par continent
  #mutate(Continent = ifelse(is.na(pays), NA_character_,
  #                            paste0("<details><summary>", continent_of_the_territory, "</summary>", pays, "</details>")))



      ###----- Power line length (km)

# Fonction pour plotter les densités
plot_density_power <- function(continent) {
  line_lenght_country_prep |> 
    left_join(continent_data, by = c("country_code_3" = "iso_3_territory_code")) |> 
    filter(continent_of_the_territory == continent) |>
    ggplot(aes(x = `Power line length (km) 2025-07-01`, y = continent)) +
    geom_violin(fill = '#036D7A') +
    #xlim(min(line_lenght_country_prep2$min_power_line), 
    #     max(line_lenght_country_prep2$max_power_line)) +
    theme_void() +
    labs(x = element_blank(), y = element_blank()) 
}

# Affichage de la table
table_line_continent_power <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(continent_of_the_territory, nb_pays, ends_with("power_line")) |> 
  #dernière mise en forme avant table
  rename(Continent = continent_of_the_territory,
         `Number of countries` = nb_pays) |> 
  arrange(Continent) |> 
  relocate(Continent) |> 
  mutate(Distribution = Continent) |> 
  #table
  gt() |> 
  #violin plot de distribution
  text_transform(
    locations = cells_body(columns = 'Distribution'),
    fn = function(column) {
      map(column, plot_density_power) |>
        ggplot_image(height = px(50), aspect_ratio = 1)
    }
  )  |> 
  #coloration des moyennes
  data_color(columns = mean_power_line,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Power line length (km) 2025-07-01',
              columns = c(mean_power_line, min_power_line, max_power_line, sum_power_line, Distribution)) |> 
  cols_label(sum_power_line = "Total", min_power_line = "Min", 
             max_power_line = "Max", mean_power_line = "Mean") |> 
  #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "left"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "left")),
            locations = cells_column_labels()) |> 
  #styles de la table
  opt_stylize(style = 1, color = 'gray') |> 
  #bordures en blanc
  tab_options(table_body.hlines.style = "solid",
              table_body.hlines.width = 2, 
              table_body.hlines.color = "white",
              table_body.border.top.color = "#6b6b6b",
              table_body.border.top.style = "solid",
              table_body.border.top.width = 2.4)
table_line_continent_power
gtsave(table_line_continent_power, "figures/table_line_continent_power.png")


      ###----- Growth since 2025-01-01 (%)

# Fonction pour plotter les densités
plot_density_growth_percent <- function(continent) {
  line_lenght_country_prep |> 
    left_join(continent_data, by = c("country_code_3" = "iso_3_territory_code")) |> 
    filter(continent_of_the_territory == continent) |>
    ggplot(aes(x = `Growth since 2025-01-01 (%)`, y = continent)) +
    geom_violin(fill = '#036D7A') +
    #xlim(min(line_lenght_country_prep2$min_power_line), 
    #     max(line_lenght_country_prep2$max_power_line)) +
    theme_void() +
    labs(x = element_blank(), y = element_blank()) 
}

# Affichage de la table
table_line_continent_growth_percent <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(continent_of_the_territory, nb_pays, ends_with("growth_percent")) |> 
  #dernière mise en forme avant table
  rename(Continent = continent_of_the_territory,
         `Number of countries` = nb_pays) |> 
  arrange(Continent) |> 
  relocate(Continent) |> 
  mutate(Distribution = Continent) |> 
  #table
  gt() |> 
  #violin plot de distribution
  text_transform(
    locations = cells_body(columns = 'Distribution'),
    fn = function(column) {
      map(column, plot_density_growth_percent) |>
        ggplot_image(height = px(50), aspect_ratio = 1)
    }
  )  |> 
  #coloration des moyennes
  data_color(columns = mean_growth_percent,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Growth since 2025-01-01 (%)',
              columns = c(mean_growth_percent, min_growth_percent, max_growth_percent, Distribution)) |> 
  cols_label(min_growth_percent = "Min", max_growth_percent = "Max", mean_growth_percent = "Mean") |> 
    #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "left"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "left")),
            locations = cells_column_labels()) |> 
  #styles de la table
  opt_stylize(style = 1, color = 'gray') |> 
  #bordures en blanc
  tab_options(table_body.hlines.style = "solid",
              table_body.hlines.width = 2, 
              table_body.hlines.color = "white",
              table_body.border.top.color = "#6b6b6b",
              table_body.border.top.style = "solid",
              table_body.border.top.width = 2.4) |> 
  #format colonne en %
  fmt_percent(columns = c(min_growth_percent, max_growth_percent, mean_growth_percent),
            decimals = 2, drop_trailing_zeros = TRUE,
            dec_mark = ",")
table_line_continent_growth_percent
gtsave(table_line_continent_growth_percent, "figures/table_line_continent_growth_percent.png")




      ###----- Growth since 2025-01-01 (km)

# Fonction pour plotter les densités
plot_density_growth_km <- function(continent) {
  line_lenght_country_prep |> 
    left_join(continent_data, by = c("country_code_3" = "iso_3_territory_code")) |> 
    filter(continent_of_the_territory == continent) |>
    ggplot(aes(x = `Growth since 2025-01-01 (km)`, y = continent)) +
    geom_violin(fill = '#036D7A') +
    #xlim(min(line_lenght_country_prep2$min_power_line), 
    #     max(line_lenght_country_prep2$max_power_line)) +
    theme_void() +
    labs(x = element_blank(), y = element_blank()) 
}

# Affichage de la table
table_line_continent_growth_km <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(continent_of_the_territory, nb_pays, ends_with("growth_km")) |> 
  #dernière mise en forme avant table
  rename(Continent = continent_of_the_territory,
         `Number of countries` = nb_pays) |> 
  arrange(Continent) |> 
  relocate(Continent) |> 
  mutate(Distribution = Continent,
         mean_growth_km = round(mean_growth_km, 0)) |> 
  #table
  gt() |> 
  #violin plot de distribution
  text_transform(
    locations = cells_body(columns = 'Distribution'),
    fn = function(column) {
      map(column, plot_density_growth_km) |>
        ggplot_image(height = px(50), aspect_ratio = 1)
    }
  )  |> 
  #coloration des moyennes
  data_color(columns = mean_growth_km,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Growth since 2025-01-01 (km)',
              columns = c(mean_growth_km, min_growth_km, max_growth_km, Distribution)) |> 
  cols_label(min_growth_km = "Min", max_growth_km = "Max", mean_growth_km = "Mean") |> 
    #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "left"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "left")),
            locations = cells_column_labels()) |> 
  #styles de la table
  opt_stylize(style = 1, color = 'gray') |> 
  #bordures en blanc
  tab_options(table_body.hlines.style = "solid",
              table_body.hlines.width = 2, 
              table_body.hlines.color = "white",
              table_body.border.top.color = "#6b6b6b",
              table_body.border.top.style = "solid",
              table_body.border.top.width = 2.4)
table_line_continent_growth_km
gtsave(table_line_continent_growth_km, "figures/table_line_continent_growth_km.png")






  
  
