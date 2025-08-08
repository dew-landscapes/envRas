
library(targets)
library(geotargets)
library(tarchetypes)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/save_satellite_layer.R"
             , "R/make_indice.R"
             )
           )

# tar options --------
# parallel over individual layer rather than across layers, so no need for crew_controller_local etc
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               # , controller = crew_controller_local(workers = floor(parallel::detectCores() * 2 / 3) # number of cores downloading from DEA
               #                                      , crashes_max = 0L
               #                                      , options_local = crew_options_local(log_directory = fs::path(tars$satellite$store, "log"))
               #                                      )
               # , storage = "worker"
               # , retrieval = "worker"
               )

# from setup --------
tar_load(c(cube_directory)
         , store = tars$setup$store
         )

targets <- list(
  # targets --------
  ## settings -------
  ### setup -------
  tar_target(name = set_file
               , command = fs::path("settings/setup.yaml")
               , format = "file"
               )
  , tar_target(name = settings
               , command = yaml::read_yaml(set_file)
               )
  ### satellite ------
  , tar_target(set_file_satellite
               , fs::path("settings/satellite.yaml")
               , format = "file"
               )
  , tar_target(settings_satellite
               , yaml::read_yaml(set_file_satellite)
               )
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  ### base grid -------
  , tar_target(base_grid_file
               , fs::path(tars$setup$store, "objects", "base_grid")
               , format = "file"
               )
  , tar_terra_rast(base_grid
                   , terra::rast(base_grid_file)
                   )
  #### bbox -------
  , tar_target(bbox
               , sf::st_bbox(base_grid) |>
                 sf::st_as_sfc() |>
                 sf::st_transform(crs = settings$crs$decdeg) |> # need decimal lat/long for rstac
                 sf::st_bbox()
               )
  ### items ------
  , tar_target(items
               , rstac::stac(settings_satellite$source_url) |>
                 rstac::stac_search(collections = settings_satellite$collection
                                    , bbox = bbox
                                    , datetime = paste0(as.character(min_date)
                                                        , "/"
                                                        , as.character(max_date)
                                                        )
                                    ) |>
                 rstac::get_request() |>
                 rstac::items_fetch()
               )
  )

## get data --------
### layers --------
if(length(yaml::read_yaml("settings/satellite.yaml")$layers)) {

  layers <- tar_map(values = list(layers = yaml::read_yaml("settings/satellite.yaml")$layers)
                    , tar_target(name = layer
                                 , command = save_satellite_layer(items = items
                                                             , base_grid = base_grid
                                                             , layer = layers
                                                             , start_date = min_date
                                                             , end_date = max_date
                                                             , cloud_mask = NULL
                                                             , base_dir = cube_directory
                                                             , settings = c(settings, settings_satellite)
                                                             # gdalcubes::write_tif args
                                                             , pack = list(type = "int16"
                                                                           , scale = 1
                                                                           , offset = 0
                                                                           , nodata = -999
                                                                           )
                                                             )
                                 )
                    )

}



### variabilities ----------
if(length(yaml::read_yaml("settings/satellite.yaml")$variability)) {

  variability <- tar_map(values = list(layers = yaml::read_yaml("settings/satellite.yaml")$variability)
                         , tar_target(name = variability
                                      , command = save_satellite_layer(items = items
                                                                  , base_grid = base_grid
                                                                  , layer = layers
                                                                  , start_date = min_date
                                                                  , end_date = max_date
                                                                  , cloud_mask = NULL
                                                                  , base_dir = cube_directory
                                                                  , settings = c(settings, settings_satellite)
                                                                  # no pack
                                                                  )
                                      )
                         )

}

### indices -------
if(length(yaml::read_yaml("settings/satellite.yaml")$indices)) {
  
  #### combine layers --------
  comb_layers <- tar_combine(name = layers_comb
                             , layers[["layer"]]
                             , command = dplyr::bind_rows(!!!.x)
                             )
  
  #### calculate -------
  indices <- tar_map(values = list(indices = yaml::read_yaml("settings/satellite.yaml")$indices |> names())
                     , tar_target(name = indice
                                  , command = make_indice(indice = indices
                                                          , start_date = min_date
                                                          , layers = layers_comb
                                                          , base_dir = cube_directory
                                                          , settings = settings_satellite
                                                          )
                                  )
                     )

}

list(targets
     , if(exists("layers")) layers
     , if(exists("comb_layers")) comb_layers
     , if(exists("variability")) variability
     , if(exists("indices")) indices
     )
