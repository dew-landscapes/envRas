
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_tile.R"
             , "R/terra_reproject.R"
             , "R/make_dist_raster.R"
             , "R/make_cube_dir.R"
             )
           )

## cores --------
possible_cores <- parallel::detectCores() * 2 / 3
max_cores <- 100
use_cores <- min(possible_cores, max_cores)

## RAM -------
total_terra_ram_prop <- 0.6 # across all cores
terra_memfrac <- total_terra_ram_prop / use_cores  # prop of available memory allowed per core (or per SDM)

# controllers  ---------
this_run_start <- format(Sys.time(), "%Y%m%d_%H%M")

main_controller <- crew_controller_local(workers = use_cores
                                         , options_local = crew_options_local(log_directory = fs::path(tars$coast$store
                                                                                                       , "log"
                                                                                                       , this_run_start
                                                                                                       , "main_controller"
                                                                                                       )
                                                                              )
                                         , name = "main_controller"
                                         , tasks_max = 1
                                         )
# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$coast))
               , controller = main_controller
               )

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , "settings/setup.yaml"
                , yaml::read_yaml(!!.x)
                )
  ### coast -------
  , tar_file_read(settings_coast
                  , "settings/coast.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_coast
                               )
               , format = "file"
               )
  ### base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               , format = "file"
               )
  ## maps --------
  ### coast ------
  , tar_target(coast_file
               , fs::path(settings$data_dir, "vector", "aus.parquet")
               , format = "file"
               )
  ### coast mask --------
  # no separate mask needed as the coast is the mask
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  ### out file --------
  , tar_target(coast_tif_file
                 , fs::path(cube_directory
                            , paste0("coast__distance__"
                                     , min_date
                                     , ".tif"
                                     )
                            )
                 )
)

# 90 m targets ----------
if(yaml::read_yaml("settings/setup.yaml")$grain$res == 90) {
  
  distance <- list(
    ## coast--------
    ### split -------
    tar_target(name = tile_extents
               , envTargets::make_tile_extents(base_grid_path = base_grid_path)
               )
    
    , tar_target(use_memfrac
                 , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
                   {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
                 )
    ### apply -------
    , tar_target(tile_coast
                 , make_dist_tile(base_grid_path = base_grid_path
                                  , extent = tile_extents
                                  , sf_dist_file = coast_file
                                  , terra_options = list(memfrac = use_memfrac)
                                  , sf_mask_file = coast_file # = coast_mask_file
                                  , sf_mask_positive = TRUE
                                  , dist_limit = 5000
                                  , out_dir = fs::path(tars$coast$store, "tiles")
                                  # via dots... to terra::lapp
                                  , wopt = list(datatype = "INT2S") # easily encompasses -10000 to 10000 m
                                  )
                 , pattern = map(tile_extents)
                 , format = "file"
                 )
    ### combine -------
    , tar_target(coast
                 , make_dist_raster(tile_coast
                                    , out_file = coast_tif_file
                                    , datatype = "INT2S" # easily encompasses -10000 to 10000 m
                                    , gdal = c("TILED=YES"
                                               , "COPY_SRC_OVERVIEWS=YES"
                                               , "COMPRESS=DEFLATE"
                                               )
                                    , names = "distance"
                                    )
                 , format = "file"
                 )
    )
  
}


# not 90 m targets ---------
if(yaml::read_yaml("settings/setup.yaml")$grain$res < 90) {
  
  distance <- list(
    ## coast -------
    tar_target(coast
                 , terra_reproject(in_file = gsub("__\\d{2}\\/"
                                                  , "__90/"
                                                  , x = fs::path(coast_tif_file)
                                                  )
                                   , base_grid_path = base_grid_path
                                   , out_file = coast_tif_file
                                   , method = "bilinear"
                                   , datatype = "INT2S"
                                   , overwrite = TRUE
                                   )
                 )
  )
  
}

list(targets, distance)
