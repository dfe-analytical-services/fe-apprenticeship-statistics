# Create conditional app name based on branch

# Set account info
rsconnect::setAccountInfo(
  name="department-for-education",
  token=Sys.getenv("SHINYAPPS_TOKEN"),
  secret=Sys.getenv("SHINYAPPS_SECRET")
)

# Deploy
rsconnect::deployApp(appName = "Apprenticeships Statistics")