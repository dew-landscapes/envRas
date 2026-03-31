
library(targets)
library(tarchetypes)
library(crew)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_cube_dir.R"
             , "R/make_polygon_overlap_tile.R"
             , "R/combine_tiles.R"
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
                                         , options_local = crew_options_local(log_directory = fs::path(tars$fire$store
                                                                                                       , "log"
                                                                                                       , this_run_start
                                                                                                       , "main_controller"
                                                                                                       )
                                                                              )
                                         , name = "main_controller"
                                         , tasks_max = 1
                                         )
# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$fire))
               , controller = main_controller
               )

mappings <- yaml::read_yaml("settings/fire.yaml")$methods

# targets --------
list(
  ## settings -------
  ### setup -------
  tar_file_read(settings
                , fs::path(tars$setup$store, "objects", "settings")
                , readRDS(!!.x)
                )
  , tar_file_read(extent_sf
                  , fs::path(tars$setup$store, "objects", "extent_sf")
                  , readRDS(!!.x)
                  )
  ### fire -------
  , tar_file_read(settings_fire
                  , "settings/fire.yaml"
                  , yaml::read_yaml(!!.x)
                  )
  ## cube directory ------
  , tar_target(cube_directory
               , make_cube_dir(set_scale = settings
                               , set_source = settings_fire
                               , cube_dir = settings$cube_dir
                               )
               )
  ### base grid path-------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif") |>
                 as.character()
               )
  ## fires file ------
  , tar_target(fire_file
               , fs::path(settings$fire_dir, "vector", "fire.parquet")
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
  # MAP------
  , tar_map(values = tibble::tibble(method = mappings
                                    , tar_id = envTargets::make_tar_id(method)
                                    , out_file = fs::path(tars$fire$store
                                                          , paste0("tiles_", method)
                                                          )
                                    )
            , names = "tar_id"
    
    ## out file --------
    , tar_target(tif_file
                   , fs::path(cube_directory
                              , paste0("fire__"
                                       , method
                                       , "__"
                                       , min_date
                                       , ".tif"
                                       )
                              )
                   )
    ## fire--------
    , tar_target(tile_background
                 , if(method == "count") 0 else (min(sfarrow::st_read_parquet(fire_file)$fireyear, na.rm = TRUE) - 1)
                 )
    ### split -------
    , tar_target(name = tile_extents
                 , envTargets::make_tile_extents(base_grid_path = base_grid_path)
                 )
    , tar_target(use_memfrac
                 , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
                   {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
                 )
    ### apply -------
    , tar_target(tiles
                 , make_polygon_overlap_tile(base_grid_path = base_grid_path
                                             , extent = tile_extents
                                             , polygon_file = fire_file
                                             , polygon_field = "fireyear"
                                             , polygon_func = method
                                             , tile_background = tile_background
                                             , terra_options = list(memfrac = use_memfrac)
                                             , out_dir = fs::path(tars$fire$store, paste0("tiles_", method))
                                             , force_new = FALSE
                                             # via dots... to terra::lapp
                                             , wopt = list(datatype = "INT2S") # covers a bit more than -27000 to 27000
                                             )
                 , pattern = map(tile_extents)
                 , format = "file"
                 )
      ### combine -------
      , tar_target(combine
                   , combine_tiles(tiles
                                   , out_file = tif_file
                                   , sf_mask = extent_sf
                                   # via dots
                                   , datatype = "INT2S" # covers a bit more than -27000 to 27000
                                   , gdal = c("TILED=YES"
                                              , "COPY_SRC_OVERVIEWS=YES"
                                              , "COMPRESS=DEFLATE"
                                              )
                                   , names = method
                                   )
                   , format = "file"
                   )
  )
)
