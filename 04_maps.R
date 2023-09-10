library(tidyverse)
library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rmapshaper)


world_polygons <- ne_countries(scale = "medium", returnclass = "sf")

csv_data <- read.csv("C:/Users/User/Downloads/Documents/Largest WWTP/clean_largewwtp.csv")
csv_data <- csv_data %>%
  distinct(Plant_Name, .keep_all = TRUE)

merged_data <- world_sf %>%
  left_join(csv_data, by = c(
    "City" = "City",
    "Country" = "Country"
  ))
merged_data <- merged_data %>%
  distinct(Plant_Name.x, .keep_all = TRUE) %>%
  select(-matches("\\.y$"))
colnames(merged_data) <- gsub("\\.x$", "", colnames(merged_data))

merged_data <- merged_data %>%
  mutate(City_Country = paste(City, Country, sep = ", ")) %>%
  mutate_if(is.numeric, ~ na_if(., 0))


popup_fun <- function(City_Country, Opening_Year, WetWeather, DryWeather) {
  paste(
    "<b>Location:</b>", City_Country,
    "<br>",
    "<b>Year Established:</b>", Opening_Year, "<br",
    "<b>Wet Weather Capacity:</b>", scales::number(WetWeather, big.mark = ","),
    "<br>",
    "<b>Dry Weather Capacity:</b>", scales::number(DryWeather, big.mark = ","),
    "<br>"
  )
}
pal_city_type <- colorFactor(c("gold", "purple"), c("Capital City", "City"))


mymap <- leaflet() %>%
  addPolygons(
    data = world_polygons,
    weight = 1,
    color = "black",
    fillColor = "darkolivegreen",
    fillOpacity = 1
  ) %>%
  addCircleMarkers(
    data = merged_data,
    weight = 1,
    fillColor = "blue",
    fillOpacity = 0.5,
    popup = ~ popup_fun(City_Country, Opening_Year, WetWeather, DryWeather)
  ) %>%
  addControl(
    html = "<h2>Largest Wastewater Treatment Plants in the World</h2>",
    position = c("topleft")
  ) %>%
  setMapWidgetStyle(style = list(background = "white"))
