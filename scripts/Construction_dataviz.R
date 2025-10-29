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
library(plotly)
library(scales)
library(ggtext)
library(htmlwidgets)


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
line_lenght_country <- read_csv("data/line_lenght_growth_country_data.csv") |> 
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
             rows = `Growth since 2025-01-01 (km)` > 0,
             method = "numeric",
             palette = c("#FECF5D", "#279f2b"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
  data_color(columns = `Growth since 2025-01-01 (km)`,
             rows = `Growth since 2025-01-01 (km)` < 0,
             method = "numeric",
             palette = c("#bf2f2f", "#ffa500"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
  data_color(columns = `Growth since 2025-01-01 (km)`,
             rows = `Growth since 2025-01-01 (km)` == 0,
             method = "numeric",
             palette = c("grey80"),
             bins = c(-Inf, 0, Inf),
             alpha = .8) |>
  #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(is.numeric)) |> 
  #noms de pays en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Country)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = cell_text(align = "left"),
            locations = cells_column_labels()) |> 
  #bordures en blanc
  tab_options(table_body.hlines.style = "solid",
              table_body.hlines.width = 10, 
              table_body.hlines.color = "white") |> 
  #taille colonnes
  cols_width(` ` ~ px(30)) |> 
  #intéractivité table
  opt_interactive(use_pagination = FALSE) |> 
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
            sum_growth_km = sum(`Growth since 2025-01-01 (km)`),
            Continent = paste(Country, collapse = ", "),
            code_pays = paste(` `, collapse = ","),
            .by = continent_of_the_territory) |> 
      #mise en forme des stats
  mutate(across(where(is.numeric) & !all_of(c("mean_power_line", "mean_growth_percent", 
                                              "mean_growth_km", "min_growth_percent", "max_growth_percent")), 
                ~ format(as.integer(., 0), nsmall = 1, big.mark = "."))) |> 
  #préparation liste déroulante de spays par continent
  mutate(Continent_detail = ifelse(is.na(Continent), NA_character_,
                             paste0("<details><summary>", continent_of_the_territory, "</summary>", Continent, "</details>")))



      ###----- Power line length (km)

# Affichage de la table
table_line_continent_power <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(nb_pays, ends_with("power_line"), Continent, Continent_detail) |> 
  #dernière mise en forme avant table
  rename(`Number of countries` = nb_pays) |> 
  arrange(Continent_detail) |> 
  relocate(Continent) |> 
  #table
  gt() |> 
  #liste interactive des pays par continent
  text_transform(
    locations = cells_body(columns = vars(Continent)),
    fn = function(x) {
      # on renvoie la colonne Detail_html ligne par ligne, enveloppée par html()
      lapply(seq_along(x), function(i) html(line_lenght_country_prep2$Continent_detail[i]))
    }) |> 
  cols_hide(Continent_detail) |> 
  #coloration des moyennes
  data_color(columns = mean_power_line,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Power line length (km) 2025-07-01',
              columns = c(mean_power_line, min_power_line, max_power_line, sum_power_line)) |> 
  cols_label(sum_power_line = "Sum", min_power_line = "Min", 
             max_power_line = "Max", mean_power_line = "Mean") |> 
  #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "center")),
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
gtsave(table_line_continent_power, "figures/table_line_continent_power.html")


      ###----- Growth since 2025-01-01 (%)

# Affichage de la table
table_line_continent_growth_percent <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(nb_pays, ends_with("growth_percent"), Continent, Continent_detail) |> 
  #dernière mise en forme avant table
  rename(`Number of countries` = nb_pays) |> 
  arrange(Continent_detail) |> 
  relocate(Continent) |> 
  #table
  gt() |> 
  #liste interactive des pays par continent
  text_transform(
    locations = cells_body(columns = vars(Continent)),
    fn = function(x) {
      # on renvoie la colonne Detail_html ligne par ligne, enveloppée par html()
      lapply(seq_along(x), function(i) html(line_lenght_country_prep2$Continent_detail[i]))
    }) |> 
  cols_hide(Continent_detail) |> 
  #coloration des moyennes
  data_color(columns = mean_growth_percent,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Growth since 2025-01-01 (%)',
              columns = c(mean_growth_percent, min_growth_percent, max_growth_percent)) |> 
  cols_label(min_growth_percent = "Min", max_growth_percent = "Max", mean_growth_percent = "Mean") |> 
    #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "center")),
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
gtsave(table_line_continent_growth_percent, "figures/table_line_continent_growth_percent.html")




      ###----- Growth since 2025-01-01 (km)

# Affichage de la table
table_line_continent_growth_km <- line_lenght_country_prep2 |> 
  #sélection des variables à afficher
  select(nb_pays, ends_with("growth_km"), Continent, Continent_detail) |> 
  #dernière mise en forme avant table
  rename(`Number of countries` = nb_pays) |> 
  arrange(Continent_detail) |> 
  relocate(Continent) |> 
  mutate(mean_growth_km = round(mean_growth_km, 0)) |> 
  #table
  gt() |> 
  #liste interactive des pays par continent
  text_transform(
    locations = cells_body(columns = vars(Continent)),
    fn = function(x) {
      # on renvoie la colonne Detail_html ligne par ligne, enveloppée par html()
      lapply(seq_along(x), function(i) html(line_lenght_country_prep2$Continent_detail[i]))
    }) |> 
  cols_hide(Continent_detail) |> 
  #coloration des moyennes
  data_color(columns = mean_growth_km,
             colors = scales::col_numeric(palette = c("#b3d3d7", "#036d7a"), domain = NULL)) |> 
  #groupe de statistiques
  tab_spanner(label = 'Growth since 2025-01-01 (km)',
              columns = c(mean_growth_km, min_growth_km, max_growth_km, sum_growth_km)) |> 
  cols_label(min_growth_km = "Min", max_growth_km = "Max", mean_growth_km = "Mean", sum_growth_km = "Sum") |> 
    #alternance gris / blanc par ligne
  opt_row_striping() |> 
  #centrer les nombres
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(-Continent)) |> 
  #lighter les nombres sauf nb_countries
  tab_style(style = cell_text(weight = "lighter"),
            locations = cells_body(-c(Continent, `Number of countries`))) |> 
  #noms de continent en gras
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(Continent)) |> 
  #mise en forme des noms de colonnes
  tab_style(style = list(cell_text(align = "center")),
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
gtsave(table_line_continent_growth_km, "figures/table_line_continent_growth_km.html")



###########################################################
############# Line Length Colombia ########################
###########################################################


# Import des données
stats_lines_colombia <- read_csv("data/stats_lines_colombia.csv") |> 
  mutate(ts = as.Date(ts, format = "%Y-%m-%d"))

# Dataviz line lenght
graph <- stats_lines_colombia |> 
  ggplot() +
  geom_line(aes(x = ts, y = len, group = 1,
                text = paste0("Time : ", ts, "\nLine lenght : ", format(as.integer(len, 0), nsmall = 1, big.mark = ","), " km")),
            color = "#036D7A", size = .7) +
  geom_point(aes(x = ts, y = len, group = 1,
                text = paste0("Time : ", ts, "\nLine lenght : ", format(as.integer(len, 0), nsmall = 1, big.mark = ","), " km")),
            color = "#036D7A", size = .3) +
  geom_vline(xintercept = as.Date("2025-03-01"), linetype = 2, color = "red4") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", breaks = pretty_breaks(n = 10)) +
  labs(x = "Source : Data from the MapYourGrid project (2025)", #caption mis comme nom axe X pour garder fix position
       y = "Line lenght (km)", 
       title = "Line lenght of Colombia") +
  theme_custom() +
  theme(axis.ticks.x = element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.x = ggplot2::element_line(color = "#cbcbcb"),
        axis.title.x = element_text(size = 12, color = "#858383", family = "Arial", hjust = 0))
graph
viz <- ggplotly(graph, tooltip = c("text")) |> 
  layout(xaxis = list(rangeslider = list(visible = TRUE)), #slider sous l'axe x
         annotations = list(list(x = as.numeric(as.Date("2025-03-10")),
                                 y = max(stats_lines_colombia$len, na.rm = TRUE),
                                 text = "Start of the MYG<br>project",   
                                 xref = "x", yref = "y", showarrow = FALSE,
                                 font = list(color = "#8B0000"),
                                 align = "left", xanchor = "left", yanchor = "top")))
viz
saveWidget(viz, "figures/lineplot_line-lenght_colombia.html")



###########################################################
############# Sections per country ########################
###########################################################


# Dataviz sections 
graph <- stats_lines_colombia |> 
  ggplot() +
  geom_line(aes(x = ts, y = amount, group = 1,
                text = paste0("Time : ", ts, "\nNumber of sections : ", amount)),
            color = "#ECC229", size = .7) +
  geom_point(aes(x = ts, y = amount, group = 1,
                text = paste0("Time : ", ts, "\nNumber of sections : ", amount)),
            color = "#ECC229", size = .3) +
  geom_vline(xintercept = as.Date("2025-03-01"), linetype = 2, color = "red4") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%m-%Y", breaks = pretty_breaks(n = 10)) +
  labs(x = "Source : Data from the MapYourGrid project (2025)", #caption mis comme nom axe X pour garder fix position
       y = "Sections (nb.)", 
       title = "Number of sections of Colombia") +
  theme_custom() +
  theme(axis.ticks.x = element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.x = ggplot2::element_line(color = "#cbcbcb"),
        axis.title.x = element_text(size = 12, color = "#858383", family = "Arial", hjust = 0))
graph
viz <- ggplotly(graph, tooltip = c("text")) |> 
  layout(xaxis = list(rangeslider = list(visible = TRUE)), #slider sous l'axe x
         annotations = list(list(x = as.numeric(as.Date("2025-03-10")),
                                 y = max(stats_lines_colombia$amount, na.rm = TRUE),
                                 text = "Start of the MYG<br>project",   
                                 xref = "x", yref = "y", showarrow = FALSE,
                                 font = list(color = "#8B0000"),
                                 align = "left", xanchor = "left", yanchor = "top")))
viz
saveWidget(viz, "figures/lineplot_nb-sections_colombia.html")



###########################################################
############# Growth base 100 per country #################
###########################################################


# Préparation des données
amount_mars25 <- stats_lines_colombia |> 
  filter(ts == "2025-03-01")
len_mars25 <- stats_lines_colombia |> 
  filter(ts == "2025-03-01")
table <- stats_lines_colombia |> 
  arrange(ts) |> 
  # transformation des métriques en base 100 en mars 2025
  mutate(amount_100_mars25 = amount / amount_mars25$amount * 100,
         amount_100_diff = round(amount_100_mars25 - 100, 1),
         amount_diff = amount - amount_mars25$amount,
         len_100_mars25 = len / len_mars25$len * 100,
         len_100_diff = round(len_100_mars25 - 100, 1),
         len_diff = round(len - len_mars25$len, 0))
  #calcul de la valeur maximale en base 100 (pour placer "Start of MYG project")
max_len_100 <- max(table$len_100_mars25, na.rm = TRUE)
max_amount_100 <- max(table$amount_100_mars25, na.rm = TRUE)
max_max_100 <- max(c(max_len_100,max_amount_100))

# Dataviz
graph <- table |> 
  ggplot() +
  #ligne and aire colorée jusq'à 100 pour nb of sections
  geom_ribbon(aes(x = ts, ymax = amount_100_mars25, group = 1, ymin = 100), 
            fill = "#ECC229", alpha = .3) +
  geom_line(aes(x = ts, y = amount_100_mars25, group = 1, 
                text = case_when(amount_100_diff < 0 ~ paste0("Time : ", ts, "\n↓ ", abs(amount_100_diff), 
                                                              "% (", amount_diff, " sections)"),
                                 amount_100_diff == 0 ~ "Base 100",
                                 .default = paste0("Time : ", ts, "\n↑ ", abs(amount_100_diff), 
                                                              "% (+", amount_diff, " sections)")),
                color = "Nb. of sections"), size = .7) +
  #ligne and aire colorée jusq'à 100 pour line lenght
  geom_ribbon(aes(x = ts, ymax = len_100_mars25, group = 1, ymin = 100), 
            fill = "#037A4C", alpha = .3) +
  geom_line(aes(x = ts, y = len_100_mars25,group = 1,
                text = case_when(len_100_diff < 0 ~ paste0("Time : ", ts, "\n↓ ", abs(len_100_diff), "% (", 
                                                           format(as.integer(len_diff, 0), nsmall = 1, big.mark = ","), 
                                                           " km line lenght)"),
                                 len_100_diff == 0 ~ "Base 100",
                                 .default = paste0("Time : ", ts, "\n↑ ", abs(len_100_diff), "% (+", 
                                                           format(as.integer(len_diff, 0), nsmall = 1, big.mark = ","), 
                                                           " km line lenght)")),
                color = "Line lenght"), size = .7) +
  #ligne pointillée au 1er mars 2025
  geom_vline(xintercept = as.Date("2025-03-01"), linetype = 2, color = "red4") +
  #mises en forme générales
  scale_x_date(date_labels = "%m-%Y", breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("Nb. of sections" = "#ECC229", 
                                "Line lenght" = "#037A4C")) +
  labs(x = "Source : Data from the MapYourGrid project (2025)", #caption mis comme nom axe X pour garder fix position 
       y = "Base 100 in 2025-03", 
       subtitle = "test", color = "",
       title = "<span style='color: #037A4C;'>Line lenght</span> and <span style='color: #ECC229;'>number of sections</span> base 100 in March 2025 - Colombia") +
  theme_custom() +
  theme(axis.ticks.x = element_line(color = "#cbcbcb"),
        plot.title = element_markdown(),
        legend.position = "top",
        panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.minor.x = ggplot2::element_line(color = "#cbcbcb"),
        axis.title.x = element_text(size = 12, color = "#858383", family = "Arial", hjust = 0))
graph
viz <- ggplotly(graph, tooltip = c("text")) |> 
  layout(legend = list(reverse = TRUE),
         annotations = list(list(x = as.numeric(as.Date("2025-03-10")),
                                 y = max_max_100,
                                 text = "Start of the MYG<br>project",   
                                 xref = "x", yref = "y", showarrow = FALSE,
                                 font = list(color = "#8B0000"),
                                 align = "left", xanchor = "left", yanchor = "top")))
viz
saveWidget(viz, "figures/lineplot_base-100_sections-len_colombia.html")




###########################################################
############# Carto des indiateurs ########################
###########################################################


# Import des données
worldmap_indicators <- st_read("scripts/data/worldmap_indicators.geojson", quiet = TRUE)

# Cartographie
library(mapview)
worldmap_indicators |> 
  select(power_line_total_length, quality_score, geometry) |> 
  st_as_sf() |> 
  mapview(zcol = "quality_score", 
          na.color = "grey40",
          #légende
          legend = FALSE,
          layer.name = "quality_score", #titre
          layers.control.pos = "topright", #position
          #fond de carte
          basemaps = c("Esri.WorldShadedRelief", "OpenStreetMap.DE"),
          #page de chaque pays
          popup = leafpop:::popupIframe("https://r-spatial.github.io/mapview/articles/mapview_04-popups.html", 
                                        width = 600, height = 700),
          #couleurs
          col.regions = ifelse(worldmap_indicators$quality_score == 0, "grey50", 
                               colorNumeric(palette = colorRampPalette(c("red", "yellow", "green"))(100),
                                            domain = c(1, 100)))) |> 
  leafem::addFeatures(weight = .7, color = "grey15", opacity = .8) |>  #contours pays
  #vue un peu zoomée pour pas avoir le monde en 3 fois
  setView(lng=0, lat=55, zoom = 2) |>  
  setMaxBounds(lng1=-180, lat1=-90, lng2=180, lat2=90) 





###########################################################
############# Depuis l'API ################################
###########################################################


# Import des données
line_world <- fromJSON("https://mapyourgrid.infos-reseaux.com/projects/2025-01_lines/counts/boundary/195271", flatten = TRUE)
line_world <- line_world$data
