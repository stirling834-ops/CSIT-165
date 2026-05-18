library("leaflet")
library("dplyr")
library("tidyr")
library("kableExtra")
library("knitr")

# Objective 0: Downloading the necessary files from the database
# Deaths
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "deaths_global.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", destfile = "deaths_US.csv")
# Confirmations
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "confirmations_global.csv")
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", destfile = "confirmations_US.csv")
# Recoveries
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", destfile = "recoveries_global.csv")

# Loading dataframes
deaths_global <- read.csv("deaths_global.csv")
deaths_US <- read.csv("deaths_US.csv")
confirmations_global <- read.csv("confirmations_global.csv")
confirmations_US <- read.csv("confirmations_US.csv")
recoveries_global <- read.csv("recoveries_global.csv")


# Objective 1: Global Map
# Step 1: Create dataframes for datapoints on the plot
# Creating a vector with all unique regions in the dataset
unique_regions <- unique(confirmations_global$Country.Region)
# Creating a dataframe containing the sums of every data for every unique country
confirmations_sums <- data.frame(matrix(nrow = length(unique_regions), ncol = 4))
colnames(confirmations_sums) <- c(colnames(confirmations_global[2:4]), colnames(confirmations_global[length(confirmations_global[1,])]))
# Using dplyr to put the needed sum of all of the values for every date according to each unique country for both
# confirmations & deaths on the global scale
# Confirmation Sums
confirmations_sums <- confirmations_global %>%
  group_by(Country.Region) %>%
  summarise(
    across(2:3, mean),
    across(ncol(confirmations_global)-1, sum)
  )
# Deaths Sums
deaths_sums <- deaths_global %>%
  group_by(Country.Region) %>%
  summarise(
    across(2:3, mean),
    across(ncol(deaths_global)-1, sum)
  )

# Step 2: Creating a global map plot with leaflet
pal1 <- colorFactor(c("blue", "red"), domain = confirmations_sums[[4]])
pal2 <- colorFactor(c("blue", "red"), domain = deaths_sums[[4]])
# Plotting plots with confirmations sums
leaflet(data = confirmations_sums) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addCircles(
    lat = ~Lat,
    lng = ~Long,
    label = ~Country.Region,
    color = ~pal1(confirmations_sums[[4]]),
    popup = ~paste("Confirmations:", confirmations_sums[[4]]),
    radius = 75000,
    group = "Confirmations"
  ) %>%
  addCircles(
    lat = ~Lat,
    lng = ~Long,
    label = ~Country.Region,
    color = ~pal2(deaths_sums[[4]]),
    popup = ~paste("Deaths:", deaths_sums[[4]]),
    radius = 75000,
    group = "Deaths"
  ) %>%
  addLayersControl(
    overlayGroups = c("Confirmations", "Deaths"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Deaths")


# Objective 2: Narrowing Down Hot Spots
# Rearranging both confirmation_sums & deaths_sums to be in descending order based on values for confirmations & deaths
# Confirmations
confirmations_sums <- confirmations_sums %>%
  arrange(desc(confirmations_sums[[4]]))
# Deaths
deaths_sums <- deaths_sums %>%
  arrange(desc(deaths_sums[[4]]))
# Creating a dataframe meant for the table to use. This includes a
tally_table <- data.frame(
  Rank = 1:nrow(confirmations_sums),
  Country1 = confirmations_sums$Country.Region,
  Count1 = confirmations_sums[[4]],
  Country2 = deaths_sums$Country.Region,
  Count2 = deaths_sums[[4]]
)
# Now to create the table itself with the kable package
kable(
  tally_table,
  col.names = c("Rank", "Country", "Count", "Country", "Count")
) %>%
  add_header_above(c(
    " " = 1,
    "Confirmations" = 2,
    "Deaths" = 2
  ))