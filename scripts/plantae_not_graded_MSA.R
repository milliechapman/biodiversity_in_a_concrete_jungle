
# load libraries
# packages we'll be using
packs <- c(
  'tidyverse'  # a must have
  , 'tidylog'    # prints out what was done in dplyr and tidr
  # , 'gbifdb' # GBIF
  , 'fst' # a faster table, makes outputs files much smaller, too.
  # , 'terra'
  , 'KnowBR'    # creates biodiversity estimates like completeness.
  , 'sf'        # spatial support
  , 'mapview'   # webmaps
  , 'janitor'   # cleans things up, also pipe-friendly cross-tabulations
  , 'tictoc'    # times things
  , 'beepr'     # makes noises
)


# check for all of the libraries, install if you don't have them
if (length(setdiff(packs, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packs, rownames(installed.packages())))  
}

# load them
vapply(packs, library, character.only = TRUE, logical(1), logical.return = TRUE, quietly = TRUE)



data(adworld) # needed for KnowBPolygon


msa_ungraded <- st_read('working_data/MSA_donut/msa_ungraded.gpkg') |> as_Spatial() # for KnowBPolygon

(holc <- st_read('working_data/holc_polys_saved/holc_plys_2022-12-08 20-13-17.gpkg') |> 
    filter(st_is(geom, 'POLYGON')) |> 
    as_Spatial()) # for KnowBPolygon


# work only with data NOT intersecting HOLC polygons per MSA (aka the non-graded areas)
(
  plantae_not_holc <- # notice name
    read_fst('../biodiversity_in_a_concrete_jungle_data_too_big/gbif_by_HOLC/gbif_plantae_holc_2023-01-23.fst') |> 
    filter(holc_grade == 'Not Graded') |>                   # NOT GRADED
    select(species, n_obs, X, Y, msa_GEOID) |>              # ID cut, msa_GEOID added
    filter(!is.na(species)) |> 
    group_by(species, X, Y, msa_GEOID) |> 
    count(name = 'counts') |> 
    ungroup() |> 
    select(species, lon = X, lat = Y, counts, msa_GEOID) |> # cosmetic reorder
    data.frame()                                            # KnowBPolygon doesn't like tibble
)


# 2x checks
plantae_not_holc |> dim()
plantae_not_holc |> tail()
plantae_not_holc |> arrange(desc(counts))
plantae_not_holc |> tabyl(msa_GEOID) |> tibble() |> arrange(desc(n)) # is this legit?


# create directory for completeness outputs for plantae within ungraded MSAs
ifelse( !dir.exists(paste0(getwd(), '/working_data/completeness_MSA/plantae_not_graded'))
        , dir.create(paste0(getwd(), '/working_data/completeness_MSA/plantae_not_graded'))
        , FALSE)

setwd(paste0(getwd(), '/working_data/completeness_MSA/plantae_not_graded')) # FUNGUS
getwd()


tic(); for(i in msa_ungraded$msa_GEOID){
  # where are we
  print(i)
  
  # make a folder for that MSA
  ifelse( !dir.exists(paste0('msa_', i))
          , dir.create(paste0('msa_', i))
          , FALSE)
  
  # set the working directory to that new place
  setwd(paste0(getwd(), '/msa_', i)) # plants!
  getwd()
  
  # the main event
  tic(); KnowBPolygon(  data = plantae_not_holc |> filter(msa_GEOID == i) 
                        , format = 'A'
                        , shape = msa_ungraded[msa_ungraded$msa_GEOID == i,] # filtering shape and 
                        , shapenames = 'msa_GEOID'
                        , save = 'CSV'
                        , dec = '.'
                        , jpg = FALSE
                        , Maps = FALSE
  ); toc()
  
  # clean up
  file.remove('Species per site.CSV')
  file.remove('Inf.txt')
  file.remove('Standard error of the estimators.CSV')
  
  # a little reset
  setwd('../')
}; toc()



# clean up
rm(plantae_not_holc)

# RESET THE WORKING DIRECTORY
setwd('../../../')
getwd()
