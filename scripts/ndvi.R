## GEE code to get annual mean ndvi 
## https://code.earthengine.google.com/ee8c5950ecc9fbbce3f102bdd5b77b8a?noload=1
library(tidyverse)

files <- list.files("working_data/ndvi/")
ndvi_data <- list()

for(i in 1:length(files)){
  nm <- str_split(files[i], "_")
  area <- nm[[1]][3]
  year <- str_sub(nm[[1]][4], end= -5)
  ndvi <- read_csv(paste0("working_data/ndvi/", files[i])) 
  colnames(ndvi)[2] ="GEOID"
  ndvi <- ndvi |>
    dplyr::select(GEOID, mean, max, stdDev) |>
    mutate(year = year,
           area = area,
           GEOID = as.numeric(GEOID))
  ndvi_data[[i]] <- ndvi
}

ndvi_data <- bind_rows(ndvi_data)

ndvi_data |> group_by(GEOID, area) |>
  summarise(mean_ndvi = mean(mean)) |>
  write_csv("working_data/mean_ndvi.csv")
  