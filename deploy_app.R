# Install rsconnect
#install.packages('rsconnect')

# Set the account info from secret variables (login to shinyapps.io to see SECRET)
#rsconnect::setAccountInfo(name='RS-eco',
#                          token=,
#                          secret='<SECRET>')

# Create a manifest file
library(rsconnect)
rsconnect::writeManifest()

# Deploy the app
#rsconnect::deployApp(account="RS-eco", server = 'shinyapps.io',
#                     appFiles = c("app.R", list.files("data", full.names=T), "R/standardiseColumns.R", "R/make_plot.R"), 
#                     appName="sdc", appTitle="Swiss Data Cube")


