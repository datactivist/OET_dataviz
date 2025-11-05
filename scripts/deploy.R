options(repos = c(CRAN = "https://cloud.r-project.org"))

rsconnect::setAccountInfo(
  name   = Sys.getenv("SHINYAPPS_NAME"),
  token  = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

# Désactive renv si présent par accident
if ("renv" %in% rownames(installed.packages())) renv::deactivate()

rsconnect::deployApp(
  appDir       = "scripts",
  appPrimaryDoc = "application_OET.qmd",
  appName      = "Progress_MapYourGrid",
  forceUpdate  = TRUE
)
