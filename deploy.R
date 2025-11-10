library(rsconnect)

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_NAME"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Deploy Quarto Shiny document
rsconnect::deployApp(
  appDir = "scripts",
  appPrimaryDoc = "application_OET.qmd",
  appName = "Progress_MapYourGrid",
  forceUpdate = TRUE
)
