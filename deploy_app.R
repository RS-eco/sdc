# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables (login to shinyapps.io to see SECRET)
library(rsconnect)
rsconnect::connectCloudUser()

# Create a manifest file
rsconnect::writeManifest(appFiles="app.R")

# Check app Dependencies
rsconnect::appDependencies()

# Deploy the app
options(rsconnect.verbose = TRUE)
rsconnect::deployApp(appFiles=c("app.R", list.files("data", full.names=T), "inst/extdata/manifest.csv", "R/standardiseColumns.R", "R/make_plot.R"), forceUpdate=T)
#=> Throws up error