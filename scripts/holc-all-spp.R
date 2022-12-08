library(tidyverse)
library(gbifdb)
library(fst)
library(sf)
library(terra)

# # connect to gbif
# db <- gbif_remote()
# # I use a local version of gbif but should work as well with the above code
# # db <- gbif_remote(bucket="gbif", 
# #                   endpoint_override = "minio.cirrus.carlboettiger.info", 
# #                   version="2022-03-01")
# 
# # grab all occurances in US, count by lat long and class of app
# US_spp <- db |>
#   filter(countrycode == "US") |>
#   count(class, decimallatitude, decimallongitude) |>
#   collect()

# read from local
US_spp <- read.fst('input_data/holc_obs_group.fst')

# Grab holc polygons
holc <- st_read("https://dsl.richmond.edu/panorama/redlining/static/fullDownload.geojson") |>
  dplyr::select(state, holc_grade, geometry) |>
  dplyr::filter(!is.na(holc_grade) & holc_grade != 'E') %>% #|> 
  dplyr::filter(!st_is_empty(.)) |>                         # dot notation needs old pipe
  sf::st_make_valid(.) 

# Make gbif occurnaces spatial (maybe should change this to terra vsct?)
us_pts <- US_spp |>
  st_as_sf(coords = c("decimallongitude", "decimallatitude"), 
           crs = st_crs(holc)) 

# clean holc
# DHL shouldn't be needed given sf::st_make_valid(.) above..
holc <- holc |>
  dplyr::mutate(valid =st_is_valid(holc)) |>
  dplyr::filter(valid=="TRUE")

# clean holc some more
holc_area  <- holc |>
  mutate(area = st_area(geometry)) |>
  as_tibble() |>
  dplyr::select(-geometry) |>
  group_by(holc_grade) |>
  summarise(area = sum(area)) |>
  mutate(area = as.numeric(area)) |>
  ungroup()

# join points with holc 
holc_obs <- st_join(us_pts, holc, join = st_within)

# write fst
as.data.frame(holc_obs) |>
  dplyr::select(-c(valid, geometry)) |>
  write_fst("holc_obs_group.fst")


(holc_obs_group <- read.fst('input_data/holc_obs_group.fst') |> tibble())

holc_obs_group |> glimpse()

class_to_keep <- c('Aves', 'Insecta', 'Mammalia', 'Reptilia', 'Amphibia')


# keep not-graded
holc_obs_group |> 
  # tidylog::filter(!is.na(holc_grade)) |> 
  tidylog::filter(class %in% class_to_keep) |> 
  mutate(holc_grade = ifelse(is.na(holc_grade), 'not graded', holc_grade)) |> 
  group_by(class, holc_grade) |> 
  summarise(sum_n = sum(n)) |> 
  arrange(holc_grade, class) -> class_grade_count

class_grade_count
class_grade_count |> tail() # class is NA? Filter that out

class_grade_count |> janitor::tabyl(class)


# redlining colors
holc_pal <- c('#92BC6B' # green
              , '#92C7C9' # blue
              , '#E7DC6B' # yellow
              , '#E47D67' # red
              #, '#A9A9A9'
) # dark gray)

class_grade_count |> 
  filter(holc_grade != 'not graded') |> 
  ggplot(aes(holc_grade, sum_n, fill = holc_grade)) + 
  geom_col() + 
  scale_fill_manual(values = holc_pal) +
  facet_wrap(~class, nrow = 2, scales = 'free_y') + 
  theme_bw() +
  NULL

holc_obs_group |> 
  group_by(class) |> 
  count(sort = TRUE)



