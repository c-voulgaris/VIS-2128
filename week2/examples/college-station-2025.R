library(tidycensus)
library(sf)
library(tidyverse)
library(tigris)
library(ggthemes)
library(here)

msa <- core_based_statistical_areas() |>
  filter(NAME == "College Station-Bryan, TX")

counties <- counties(state = "TX") |>
  st_centroid() |>
  st_filter(msa)

variables = c(undergrad_stu = "B14001_008",
              grad_students = "B14001_009",
              total_populat = "B01001_001")

populations <- get_acs(geography = "tract",
                       state = "TX",
                       county = counties$NAME,
                       variables = variables,
                       output = "wide")

tracts <- tracts(state = "TX",
                 county = counties$NAME) |>
  select(GEOID, ALAND) |>
  left_join(populations) |>
  mutate(non_college = total_populatE - undergrad_stuE - grad_studentsE) |>
  mutate(pop_dens_km2 = 1000000 * total_populatE / ALAND)

landmarks <- landmarks(type = "area", state = "TX") |>
  filter(FULLNAME == "Texas A & M Univ")

roads <- primary_secondary_roads(state = "TX") |>
  st_filter(tracts)

detail_box <- tibble(maxlat = 30.75,
                     minlat = 30.48,
                     minlong = -96.45,
                     maxlong = -96.2)

ggplot(tracts) +
  geom_sf(aes(fill = pop_dens_km2)) +
  geom_sf(data = roads, color  = "white", linewidth = 0.75) +
  geom_rect(data = detail_box,
            aes(xmin = minlong,
                xmax = maxlong,
                ymin = minlat,
                ymax = maxlat),
            color = "black",
            linewidth = 1,
            fill = NA) +
  scale_fill_gradient(low = "gray20",
                      high = "white",
                      na.value = "black",
                      transform = "log10",
                       name = "People per\nsquare kilometer") +
  theme_void()

ggsave(here("week2",
            "examples",
            "college_station_chloropleth.png"),
       width = 3.5, 
       height = 3.5, 
       dpi = 600)

ugrad_points <- st_sample(tracts,
                         size = round(tracts$undergrad_stuE/100))

grad_points <- st_sample(tracts,
                          size = round(tracts$grad_studentsE/100))

other_points <- st_sample(tracts,
                         size = round(tracts$non_college/100))

ggplot(tracts) +
  geom_sf(data = landmarks,
          aes(fill = "Texas A&M University"),
          color = NA) +
  geom_sf(data = other_points,
          aes(color = "Not a college student"),
          alpha = 0.5,
          shape = 16) +
    geom_sf(data = ugrad_points,
          aes(color = "Student (undergraduate program)"),
          alpha = 0.5,
          shape = 16) +
  geom_sf(data = grad_points,
          aes(color = "Student (graduate or professional program)"),
          alpha = 0.5,
          shape = 16) +
  geom_sf(data = roads, color  = "black", linewidth = 0.75) +
  coord_sf(xlim = c(detail_box$minlong, detail_box$maxlong),
           ylim = c(detail_box$minlat, detail_box$maxlat)) +
  scale_color_manual(values = c("gray40",
                                "gold2",
                                "forestgreen"),
                     name = "1 dot = 100 people") +
  scale_fill_manual(name = "",
                    values = "firebrick") +
  theme_void() 

ggsave(here("week2",
            "examples",
            "college_station_dots.png"),
       width = 9, 
       height = 8, 
       dpi = 600)
