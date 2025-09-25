library(tidyverse)
library(here)
library(sf)
library(units)
library(maptiles)
library(tidyterra)
library(ggthemes)
library(cowplot)
library(grid)

## load data
dengue <- here("week4",
               "example",
               "DengueClustersKML.kml") |>
  st_read() |>
  st_transform(3414) |>
  mutate(area_km2 = as.numeric(st_area(geometry))/1000000)

park_facil <- here("week4",
                   "example",
                   "ParkFacilities.geojson") |>
  st_read() |>
  st_transform(3414)

bike_rental <- park_facil |>
  filter(CLASS == "BICYCLE RENTAL SHOP") 

bike_rental <- bike_rental |>
  mutate(nearest_dengue = st_nearest_feature(bike_rental, dengue)) |>
  mutate(dist_to_dengue = st_distance(bike_rental, 
                                      dengue[nearest_dengue,], 
                                      by_element = TRUE)) |>
  mutate(dengue_dist_km = as.numeric(dist_to_dengue)/1000)

bike_path <- here("week4",
                  "example",
                  "CyclingPathNetworkGEOJSON.geojson") |>
  st_read() |>
  st_transform(3414) |>
  mutate(length_km = as.numeric(st_length(geometry))/1000)

boundary <- here("week4",
                 "example",
                 "MasterPlan2019SubzoneBoundaryNoSeaGEOJSON.geojson") |>
  st_read()

bike_path_buffer <- st_buffer(bike_path, dist = 100)

bike_path_dengue <- st_intersection(bike_path_buffer, dengue) 

bike_path_dengue <- bike_path_dengue|>
  mutate(area_km2 = as.numeric(st_area(geometry))/1000000)

bike_path_dengue_buffer <- st_buffer(bike_path_dengue, dist = 500)

detail_bbox <- st_bbox(bike_path_dengue_buffer)

detail_rect <- st_as_sfc(detail_bbox)

bike_rental_detail <- bike_rental |>
  st_intersection(detail_rect)

dengue_detail <- dengue |>
  st_intersection(detail_rect)

bike_path_detail <- bike_path |>
  st_intersection(detail_rect)

bike_path_dengue_detail <- bike_path_dengue |>
  st_intersection(detail_rect) |>
  mutate()

## Area of polygons
sum(dengue$area_km2)

sum(bike_path_dengue$area_km2) / sum(dengue$area_km2)

## Lengths of lines
sum(bike_path$length_km)

## summary of distance
summary(bike_rental$dist_to_dengue)


## Figures
base_map_full <- get_tiles(boundary,
                           provider = "CartoDB.PositronNoLabels",
                           crop = TRUE,
                           zoom = 10)

base_map_detail <- get_tiles(detail_rect,
                             provider = "CartoDB.PositronNoLabels",
                             crop = TRUE,
                             zoom = 13)

ggplot(bike_rental) +
  geom_histogram(aes(x = dengue_dist_km))

full_plot_with_legend <- ggplot(bike_path_dengue) +
  geom_spatraster_rgb(data = base_map_full) +
  geom_sf(data = dengue, color = NA,
          aes(fill = "More than 100 meters\nfrom bike network")) +
  geom_sf(color = NA,
          aes(fill = "Within 100 meters of\nbike network")) +
  geom_sf(data = bike_rental,
          aes(color = dengue_dist_km,
              shape = "Park-based\nbike rental")) +
  geom_sf(data = bike_path, color = "darkgreen", 
          linewidth = 0.5,
          aes(linetype = "Bike network")) +
  geom_sf(data = detail_rect, fill = NA, linewidth = 1) +
  scale_color_viridis_c(option = "D",
                        name = "Distance from\ndengue cluster",
                        breaks = breaks <- seq(1, 5, by = 1),
                        labels = paste0(breaks, " km")) +
  scale_fill_manual(name = "Dengue cluster",
                    values = c("darkred", "darkorange")) +
  scale_linetype(name = "") +
  scale_shape(name = "") +
  theme_void() 

full_plot_with_legend +
  theme(legend.position = "none")

here("week4",
     "example",
     "full.png") |>
  ggsave(height = 3,
         width = 5,
         dpi = 600,
         units = "in")

detail_plot_with_legend <- ggplot(bike_path_dengue) +
  geom_spatraster_rgb(data = base_map_detail) +
  geom_sf(data = dengue_detail, color = NA,
          aes(fill = "More than 100 meters\nfrom bike network")) +
  geom_sf(color = NA,
          aes(fill = "Within 100 meters of\nbike network")) +
  geom_sf(data = bike_rental_detail,
          aes(color = dengue_dist_km,
              shape = "Park-based\nbike rental")) +
  geom_sf(data = bike_path_detail, color = "darkgreen", 
          linewidth = 1,
          aes(linetype = "Bike network")) +
  geom_sf(data = detail_rect, fill = NA, linewidth = 1) +
  scale_color_viridis_c(option = "A",
                        name = "Distance from\ndengue cluster",
                        breaks = breaks <- c(0.5, 1.0, 1.5),
                        labels = paste0(breaks, " km")) +
  scale_fill_manual(name = "Dengue cluster",
                    values = c("darkred", "darkorange")) +
  scale_linetype(name = "") +
  scale_shape(name = "") +
  coord_sf(xlim = c(detail_bbox["xmin"], detail_bbox["xmax"]),
           ylim = c(detail_bbox["ymin"], detail_bbox["ymax"])) +
  theme_void()

detail_plot_with_legend +
  theme(legend.position = "none")

here("week4",
     "example",
     "detail.png") |>
  ggsave(height = 4.5,
         width = 7.5,
         dpi = 600,
         units = "in")

detail_legend <- get_legend(detail_plot_with_legend)
full_legend <- get_legend(full_plot_with_legend)

grid.newpage()
grid.draw(detail_legend)

grid.newpage()
grid.draw(full_legend)


