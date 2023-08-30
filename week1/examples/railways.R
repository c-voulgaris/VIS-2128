library(tigris)
library(sf)
library(tidyverse)
library(crsuggest)
library(ggspatial)
library(here)

# Load railways layer
rail <- rails() |>
  st_set_crs("WGS84")

# Suggest a projected coordinate system
crs_potential <- suggest_crs(rail)

# Select a projected coordinate system
my_crs <- crs_potential$crs_proj4[1]

# Define a plot bounding box in the projected coordinate system
pts <- tibble(name = c("SW", "NE"), 
              lat = c(17.5, 51.5),
              lon = c(-121, -65.76570398048696 )) |>
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
  annotate("text", 
           x = bbox["xmin"] + 
             0*(bbox["xmax"] - bbox["xmin"]),
           y = bbox["ymin"] + 
             1*(bbox["ymax"] - bbox["ymin"]),
           label = "Rail connections among urban areas",
           size = 6,
           vjust = 1, hjust = 0) +
  annotate("text", 
           x = bbox["xmin"] + 
             0*(bbox["xmax"] - bbox["xmin"]),
           y = bbox["ymin"] + 
             0*(bbox["ymax"] - bbox["ymin"]),
           label = "My classmate, Dawon Oh helped me with this assignment by suggesting I reproject the data before plotting.\nAll data are from United States Census TIGER files.\nThis map was created in the R programming language using the tigris, sf, crsuggest, and tidyverse, and ggspatial packages",
           size = 3,
           vjust = 0, hjust = 0) +
  annotation_scale(pad_y = unit(1, "in"),
                   pad_x = unit(0.5, "in")) +
  annotation_north_arrow(pad_y = unit(1.5, "in"),
                   pad_x = unit(0.5, "in")) +
  theme_void() +
  theme(legend.position = "right",
        plot.margin = margin(0, 40, 0, 0))

here("week1",
     "examples",
     "railway.pdf") |>
  ggsave(width = 11, height = 8.5, units = "in")
