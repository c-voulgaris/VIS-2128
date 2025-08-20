library(here)
library(tidyverse)
library(sf)
library(tigris)

## The three layers I will include are the United States coastline, the CBSA
## boundary for the New York metro area, and the UZAs within the New York 
## metro area.

county_boundary <- counties(state = "NY") |>
  filter(NAME == "New York")

water <- area_water(state = "NY",
                    county = "New York")

roads <- roads(state = "NY", county = "New York")

parks <- landmarks(state = "NY",
                   type = "area") |>
  filter(MTFCC %in% 
           c("K2180", # Park
             "K2181", # National Park Service Land
             "K2182", # National Forest or Recreation Area
             "K2183", # Tribal Park, Forest, or Recreation Area
             "K2184", # State Park, Forest, or Recreation Area
             "K2185", # Regional Park, Forest, or Recreation Area
             "K2186", # County Park, Forest, or Recreation Area
             "K2187", # County Subdivision Park, Forest, or Recreation Area
             "K2188", # Incorporated Place Park, Forest, or Recreation Area
             "K2189", # Private Park, Forest, or Recreation Area
             "K2190")) |> # Other Park, Forest, or Recreation Area 
  st_filter(county_boundary)

ggplot(roads) +
  geom_sf(color = "gray") +
  geom_sf(data = parks,
          color = NA,
          aes(fill = MTFCC)) +
  geom_sf(data = water,
          color = NA,
          fill = "lightblue") +
  scale_fill_manual(values = c("darkgreen",
                               "darkgoldenrod",
                               "darkseagreen2",
                               "darkorange",
                               "chartreuse2"),
                    name = "Type of park",
                    labels = c("Park (general)",
                               "National Park Service Land",
                               "National Forest or Recreation Area",
                               "City Park",
                               "Other Park")) +
  theme_void()

ggsave(here("week1",
            "examples",
            "park-map-2025.png"),
       height = 7.5,
       width = 8,
       units = "in",
       dpi = 600)
