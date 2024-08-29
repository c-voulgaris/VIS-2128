library(tigris)
library(sf)
library(tidyverse)
library(crsuggest)
library(ggspatial)
library(here)
library(ggthemes)

# Load railways layer
rail <- rails() |>
  st_set_crs("WGS84")

# Suggest a projected coordinate system
crs_potential <- suggest_crs(rail)

# Select a projected coordinate system
my_crs <- crs_potential$crs_proj4[1]

# Define a plot bounding box in the projected coordinate system
pts <- tibble(name = c("SW", "NE"), 
              lat = c(24.5, 49.2),
              lon = c(-117.1, -65.76570398048696 )) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84") |>
  st_transform(my_crs)

bbox <- st_bbox(pts)

# Reproject the rail data
rail_projected <- rail |>
  st_transform(my_crs)

# Load states
states <- states() |>
  filter(!STUSPS %in% c("AS", "MP", "GU", "PR", "VI", "AK", "HI"))

# Load places
UAs <- urban_areas() |>
  st_transform(my_crs)

# Plot railways
ggplot(rail_projected) +
  geom_sf(data = states,
          fill = NA,
          aes(color = "State boundary"),
          key_glyph = draw_key_abline) +
  geom_sf(aes(color = "Rail line"),
          key_glyph = draw_key_abline,
          size = 0.5) +
  geom_sf(data = UAs,
          color = NA,
          aes(fill = "Urban area")) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  scale_fill_manual(name = "",
                    values = "brown") +
  scale_color_manual(name = "",
                     values = c("gray",
                                "skyblue4")) +
  theme_map() +
  theme(legend.background = element_rect(fill = NA))


here("week1",
     "examples",
     "railway.jpg") |>
  ggsave(width = 8, height = 6, units = "in", dpi = 600)
