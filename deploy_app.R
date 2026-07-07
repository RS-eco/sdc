# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables (login to shinyapps.io to see SECRET)
library(rsconnect)
connectCloudUser()

# Create a manifest file
rsconnect::writeManifest("inst/app")

# Deploy the app
rsconnect::deployApp(appDir="inst/app/")
#rsconnect::deployApp(appFiles=c("inst/app/app.R", list.files("data", full.names=T), "inst/extdata/manifest.csv", "R/standardiseColumns.R", "R/make_plot.R"))
#=> Throws up error