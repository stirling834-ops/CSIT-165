library(tidyverse)
library(leaflet)
library(knitr)
library(kableExtra)
library(cowplot)
library(lubridate)

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
deaths_us <- read.csv("deaths_US.csv")
confirmations_global <- read.csv("confirmations_global.csv")
confirmed_us <- read.csv("confirmations_US.csv")
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


# Objective 3
ca_confirmed <- confirmed_us %>%
  filter(Province_State == "California") %>%
  select(Admin2, starts_with("1/"), starts_with("2/"), starts_with("3/"), 
         starts_with("4/"), starts_with("5/"), starts_with("6/"),
         starts_with("7/"), starts_with("8/"), starts_with("9/"),
         starts_with("10/"), starts_with("11/"), starts_with("12/")) %>%
  pivot_longer(-Admin2, names_to = "date", values_to = "confirmed") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

ca_total <- ca_confirmed %>%
  group_by(date) %>%
  summarise(confirmed = sum(confirmed))

moderna_date <- as.Date("2021-01-29") + weeks(6)
delta_date <- as.Date("2021-05-11")
omicron_date <- as.Date("2021-11-26")

ca_plot <- ggplot(ca_total, aes(x = date, y = confirmed)) +
  geom_point(size = 0.5) +
  geom_vline(xintercept = moderna_date, color = "steelblue", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = delta_date, color = "firebrick", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = omicron_date, color = "red", linetype = "dashed", linewidth = 1.2) +
  labs(title = "California COVID-19 Confirmations",
       x = "Date", y = "Cumulative Confirmations") +
  theme_minimal()

ca_plot

top3_cities <- ca_confirmed %>%
  filter(Admin2 %in% c("Los Angeles", "San Diego", "Orange"))

city_plot <- ggplot(top3_cities, aes(x = date, y = confirmed, color = Admin2)) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = moderna_date, color = "steelblue", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = delta_date, color = "firebrick", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = omicron_date, color = "red", linetype = "dashed", linewidth = 1.2) +
  scale_color_manual(values = c("Los Angeles" = "#d95f02", "Orange" = "#1b9e77", "San Diego" = "#7570b3")) +
  labs(title = "Top 3 CA Counties COVID-19 Confirmations",
       x = "Date", y = "Cumulative Confirmations", color = "County") +
  theme_minimal() +
  theme(legend.key.size = unit(1.5, "lines")) +
  guides(color = guide_legend(override.aes = list(size = 4)))

city_plot

city_plot <- city_plot + theme(panel.grid = element_line(color = "grey90"))
plot_grid(ca_plot, city_plot, ncol = 1)


# Objective 4
last_col_confirmed <- ncol(confirmed_us)
last_col_deaths <- ncol(deaths_us)

obj4_data <- deaths_us %>%
  select(Admin2, Province_State, Population) %>%
  mutate(
    confirmed = confirmed_us[[last_col_confirmed]],
    deaths = deaths_us[[last_col_deaths]]
  ) %>%
  filter(Population > 0, confirmed > 0, deaths > 0)

pop_plot <- ggplot(obj4_data, aes(x = log(Population), y = log(confirmed))) +
  geom_point(size = 0.8, alpha = 0.5, color = "#6a0dad") +
  labs(title = "Population vs Confirmations",
       x = "Log(Population)", y = "Log(Confirmed)") +
  theme_minimal()

death_plot <- ggplot(obj4_data, aes(x = log(confirmed), y = log(deaths))) +
  geom_point(size = 0.8, alpha = 0.5, color = "#6a0dad") +
  labs(title = "Confirmations vs Deaths",
       x = "Log(Confirmed)", y = "Log(Deaths)") +
  theme_minimal()

plot_grid(pop_plot, death_plot, ncol = 2)

