options(java.parameters = '-Xmx2G')

library(tidyverse)
library(r5r)
library(sf)
library(here)
library(units)

data_geo <- here("week4",
                 "Examples",
                 "network-geom.geojson") |>
  st_read() |>
  select(boundary, admin_level, amenity, name, highway)

boundary <- data_geo |>
  filter(!is.na(boundary),
         admin_level == 8,
         name == "Mladá Boleslav")

main_roads <- data_geo |>
  st_filter(boundary) |>
  filter(highway == "primary" |
           highway == "secondary" |
           highway == "motorway" |
           highway == "tertiary" |
           highway == "residential" |
           highway == "unclassified")

amenities <- data_geo |>
  filter(!is.na(amenity)) |>
  st_filter(boundary)

schools <- amenities |>
  filter(amenity == "school") |>
  st_centroid()

restaurants <- amenities |>
  filter(amenity == "restaurant" |
           amenity == "fast_food" |
           amenity == "food_court") |>
  st_centroid()

proj_crs <- "+proj=cass +lat_0=48.0384638888889 +lon_0=31.8041805555556 +x_0=0 +y_0=0 +a=6376045 +rf=310 +pm=ferro +units=m +no_defs"

boundary <- boundary |>
  st_transform(proj_crs)

grid <- st_make_grid(boundary, 
                     cellsize = as_units(10, "m"),
                     square = FALSE) |>
  st_as_sf() |>
  st_filter(boundary)

grid <- grid |>
  mutate(id = as.character(seq(1, nrow(grid))))

grid_pts <- st_centroid(grid) 

restaurants <- restaurants |>
  mutate(id = seq(1:nrow(restaurants)))

schools <- schools |>
  mutate(id = seq(1:nrow(schools)))

grid_pts <- grid_pts |>
  st_transform("WGS84")

my_core <- here("week4",
                "Examples") |>
  setup_r5()

# Number of restautants within 10 minute walk of each school

tt_mat_1 <- travel_time_matrix(my_core, 
                               origins = schools,
                               destinations = restaurants,
                               mode = "WALK")

school_table <- tt_mat_1 |>
  filter(travel_time_p50 < 11) |>
  group_by(from_id) |>
  summarise(n_restaurants = n()) |>
  mutate(id = as.numeric(from_id))

schools <- schools |>
  left_join(school_table) |>
  replace_na(list(n_restaurants = 0))



# area within 10 minute walk of any restaurant.

tt_mat_2 <- travel_time_matrix(my_core, 
                               origins = restaurants,
                               destinations = grid_pts,
                               mode = "WALK")

stop_r5()

iso_cells <- tt_mat_2 |>
  filter(travel_time_p50 <= 10) |>
  group_by(to_id) |>
  summarise(n_restaurants = n()) |>
  rename(id = to_id)

isochrone <- grid |>
  right_join(iso_cells) |>
  st_union()

ggplot(isochrone) +
  geom_sf()

ggplot(schools) +
  geom_sf(data = boundary,
          aes(fill = "Area of city more than 10-minute walk from a restaurant"),
          color = NA) +
  geom_sf(data = isochrone,
          color = NA,
          aes(fill = "Area of city less than 10-minute walk from a restaurant")) +
  geom_sf(data = main_roads,
          color = "snow3") +
  geom_sf(aes(color = n_restaurants,
              size = "School")) +
  geom_sf(data= restaurants, 
          color = "lightpink4",
          aes(size = "Restaurant")) +
  scale_color_viridis_c(name = "Number of\nrestaurants within\n10-minute walk\nfrom school",
                        option = "C",
                        direction = -1,
                        breaks = seq(0, 12, by=2)) +
  scale_size_manual(values = c(0.5, 2), name = "") +
  scale_fill_manual(values = c("wheat3", "oldlace"),
                    name = "") +
  theme_void() 

ggsave(here("week4",
            "Examples",
            "mlada-boleslav.png"),
       height = 7,
       width = 10, 
       units = "in",
       dpi = 600)
