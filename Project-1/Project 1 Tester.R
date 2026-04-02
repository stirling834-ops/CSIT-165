# download datasets fresh each time the document is rendered
download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", destfile = "deaths.csv")

download.file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", destfile = "confirmations.csv")

# load into data frames
deaths <- read.csv("deaths.csv")
confirmations <- read.csv("confirmations.csv")

# Objective 1
# first date column is index 5; find row with max confirmations and deaths on day 1
max_confirmations_row <- which.max(confirmations[, 5])
max_deaths_row <- which.max(deaths[, 5])

# extract region names
origin_confirmations <- confirmations[max_confirmations_row, "Country.Region"]
origin_deaths <- deaths[max_deaths_row, "Country.Region"]

# save origin coordinates for use in objective 3
origin_lat <- confirmations[max_confirmations_row, "Lat"]
origin_long <- confirmations[max_confirmations_row, "Long"]

# confirm both datasets point to the same origin
if (origin_confirmations == origin_deaths) {
  print(paste("The origin of COVID-19 is", origin_confirmations))
} else {
  print(paste("Confirmations origin:", origin_confirmations,
              "Deaths origin:", origin_deaths))
}

# Objective 2
latest_region <- ""
latest_date_index <- 0

# outer loop: iterate through each row (region)
for (i in 1:nrow(confirmations)) {
  # inner loop: scan date columns left to right (col 5 onward)
  for (j in 5:ncol(confirmations)) {
    # first non-zero value = first confirmed case for this region
    if (confirmations[i, j] > 0) {
      # if this region's first case is more recent than current latest, update
      if (j > latest_date_index) {
        latest_date_index <- j
        latest_region <- confirmations[i, "Country.Region"]
        latest_lat <- confirmations[i, "Lat"]
        latest_long <- confirmations [i, "Long"]
      }
      # stop scanning dates for this region once first case is found
      break
    }
  }
}

latest_region
names(confirmations)[latest_date_index]

# Objective 3
# Calculating the distance from the origin of COVID-19 to the latest area to have a first confirmed case, which uses
# the dism function from the geosphere package to calculate distance from longitude/latitude to meters
distance_meters <- distm(c(origin_long,origin_lat), c(latest_long,latest_lat), fun=distGeo)
# This can then be calculated from meters to miles
distance_miles <- distance_meters/1609.344
# Printing the results
print(paste(latest_region, "is", distance_miles, "miles away from", origin_confirmations))

# Objective 4
# Getting information to create a new dataframe for risk score assessment
# This includes States/Provinces, Countries/Regions, Latitude, Longitude, Confirmations, Deaths, and the Risk Score
# This will be important due to needing to filter out all instances of cases and deaths on cruise ships
# It also makes it nicer for seeing the data all in one place
filtered_states <- confirmations[confirmations$Lat != 0 & !is.na(confirmations$Lat),1]
filtered_regions <- confirmations[confirmations$Lat != 0 & !is.na(confirmations$Lat),2]
filtered_lat <- confirmations[confirmations$Lat != 0 & !is.na(confirmations$Lat),3]
filtered_long <- confirmations[confirmations$Lat != 0 & !is.na(confirmations$Lat),4]
filtered_confirmations <- confirmations[confirmations$Lat != 0 & !is.na(confirmations$Lat),length(confirmations[1,])]
filtered_deaths <- deaths[deaths$Lat != 0 & !is.na(deaths$Lat),length(deaths[1,])]
# Calculating the risk scores for every region
risk_scores <- 100*(filtered_deaths/filtered_confirmations)
# Creating a vector for the headers of each column in the new dataframe
headers <- c("Province/State", "Country/Region", "Lat", "Long", "Confirmations", "Deaths", "Risk Score")
# Creating the dataframe
risk_assessment <- data.frame(filtered_states, filtered_regions, filtered_lat, filtered_long, filtered_confirmations, filtered_deaths, risk_scores)
colnames(risk_assessment) <- headers
# Evaluating which areas have the highest and lowest risk scores
# First the row number needs to be retrieved
max_risk_row = which.max(risk_assessment[, 7])
min_risk_row = which.min(risk_assessment[, 7])
# Next, the region names associated with the max and min risk are extracted
max_risk_region = risk_assessment[max_risk_row, 2]
min_risk_region = risk_assessment[min_risk_row, 2]
# Printing the results
print(paste("The region with the highest risk is", max_risk_region))
print(paste("The region with the lowest risk is", min_risk_region))
