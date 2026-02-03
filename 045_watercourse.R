
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_tile.R"
             , "R/terra_reproject.R"
             , "R/make_dist_raster.R"
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
                                         , options_local = crew_options_local(log_directory = fs::path(tars$watercourse$store
                                                                                                       , "log"
                                                                                                       , this_run_start
                                                                                                       , "main_controller"
                                                                                                       )
                                                                              )
                                         , name = "main_controller"
                                         , tasks_max = 1
                                         )
# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$watercourse))
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
  ### watercourse -------
  , tar_file_read(settings_wc
                  , "settings/watercourse.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , fs::path("/mnt"
                          , "envshare" # Hack while no write access to SAMBA. This should be "envcube"
                          , envRaster::name_env_tif(x = c(settings$extent
                                               , settings$grain
                                               , source = settings_wc$source
                                               , collection = settings_wc$collection
                                               )
                                         , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                                         , cube_defn = c("temp", "res")
                                         , dir_only = TRUE
                                         , prefixes = c("sat", "use")
                                         , fill_null = TRUE
                                         )$out_dir
                          )
               )
  ## wc mask --------
  , tar_target(wc_mask_file
               , fs::path(settings$data_dir, "vector", "water_poly.parquet")
               , format = "file"
               )
  ## base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
               , format = "file"
               )
  ## make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               , format = "file"
               )
  ## maps -------
  ### wc ------
  , tar_target(wc_file
               , fs::path(settings$data_dir, "vector", "water_lines.parquet")
               , format = "file"
               )
  ## prep -------
  ### dates -------
  , tar_target(name = max_date
               , paste0(as.numeric(format(Sys.Date(), "%Y")) - 1, "-12-31") 
               )
  , tar_target(name = min_date
               , command = lubridate::as_date(max_date) - lubridate::as.period(envFunc::find_name(settings, "temp")) + lubridate::as.period("P1D")
               )
  ### files --------
  , tar_target(wc_tif_file
                 , fs::path(cube_directory
                            , paste0("watercourse__distance__"
                                     , min_date
                                     , ".tif"
                                     )
                            )
                 )
)

# 90 m targets ----------
if(yaml::read_yaml("settings/setup.yaml")$grain$res == 90) {
  
  distance <- list(
    ## wc--------
    ### split -------
    tar_target(name = tile_extents
               , envTargets::make_tile_extents(base_grid_path = base_grid_path)
               )
    
    , tar_target(use_memfrac
                 , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
                   {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
                 )
    ### apply -------
    , tar_target(tile_wc
                 , make_dist_tile(base_grid_path = base_grid_path
                                  , extent = tile_extents
                                  , vect_line_file = wc_file
                                  , terra_options = list(memfrac = use_memfrac)
                                  , vect_mask_file = wc_mask_file
                                  , dist_limit = 2000
                                  , out_dir = fs::path(tars$watercourse$store, "tiles")
                                  # via dots... to terra::lapp
                                  , wopt = list(datatype = "INT2S") # covers a bit more than -27000 to 27000
                                  )
                 , pattern = map(tile_extents)
                 , format = "file"
                 )
    ### combine -------
    , tar_target(wc
                 , make_dist_raster(tile_wc
                                    , out_file = wc_tif_file
                                    , datatype = "INT2S" # easily encompasses -1000 to 1000 m
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
    ## wc -------
    tar_target(wc
                 , terra_reproject(in_file = gsub("__\\d{2}\\/"
                                                  , "__90/"
                                                  , x = fs::path(wc_tif_file)
                                                  )
                                   , base_grid_path = base_grid_path
                                   , out_file = wc_tif_file
                                   , method = "bilinear"
                                   , datatype = "INT2S"
                                   , overwrite = TRUE
                                   )
                 )
  )
  
}

list(targets, distance)
