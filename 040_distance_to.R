
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# source ------
tar_source(c("R/make_dist_rast.R"
             , "R/terra_reproject.R"
             , "R/make_coast_raster.R"
             )
           )

# cores --------
use_cores <- floor(parallel::detectCores() * 3 / 4)

# ram -------
total_terra_ram_prop <- 0.6 # across all cores
terra_memfrac <- total_terra_ram_prop / use_cores # prop of available memory allowed per core (or per tile)

# tar options --------
tar_option_set(packages = sort(unique(yaml::read_yaml("settings/packages.yaml")$packages))
               , controller = crew_controller_local(workers = use_cores
                                                    , crashes_max = 0L
                                                    , options_local = crew_options_local(log_directory = fs::path(tars$distance_to$store, "log")
                                                                                         , log_join = TRUE
                                                                                         )
                                                    )
               )

# targets --------
targets <- list(
  ## settings -------
  ### setup -------
  tar_target(name = set_file
               , command = fs::path("settings/setup.yaml")
               , format = "file"
               )
  , tar_target(name = settings
               , command = yaml::read_yaml(set_file)
               )
  ### distance_to -------
  , tar_target(name = set_file_distance_to
               , command = fs::path("settings/distance_to.yaml")
               , format = "file"
               )
  , tar_target(name = settings_distance_to
               , command = yaml::read_yaml(set_file_distance_to)
               )
  ## cube directory ------
  , tar_target(cube_directory
               , name_env_tif(x = c(settings$extent
                                    , settings$grain
                                    , source = settings_distance_to$source
                                    , collection = settings_distance_to$collection
                                    )
                              , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                              , cube_defn = c("temp", "res")
                              , dir_only = TRUE
                              , prefixes = c("sat", "use")
                              , fill_null = TRUE
                              )$out_dir %>%
                 fs::path("I:", .)
               )
  ## make cube directory --------
  , tar_target(make_cube_dir
               , fs::dir_create(cube_directory)
               )
  ## maps -------
  ### coast ------
  , tar_target(coast_file
               , fs::path("..", "..", "..", "data", "vector", "aus.parquet")
               , format = "file"
               )
  , tar_target(coast_sf
               , arrow::open_dataset(coast_file) |>
                 sfarrow::read_sf_dataset() |>
                 sf::st_transform(crs = sf::st_crs(terra::rast(base_grid_path))) |>
                 sf::st_make_valid()
               )
  , tar_target(coast_sf_line
               , coast_sf |>
                 sf::st_cast("MULTILINESTRING") |>
                 sf::st_cast("LINESTRING")
               )
  ### base grid -------
  , tar_target(base_grid_path
               , fs::path(dirname(cube_directory), "base.tif")
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
    ## split -------
    tar_target(name = tile_extents
               , make_tile_extents(base_grid_path = base_grid_path)
               )
    
    , tar_target(use_memfrac
                 , if(nrow(tile_extents >= use_cores)) {terra_memfrac} else
                   {(total_ram * total_terra_ram_prop / nrow(tile_extents)) / total_ram}
                 )
    ## coast--------
    ### apply -------
    , tar_terra_rast(tile_coast
                     , make_dist_rast(base_grid_path
                                      , tile_extents
                                      , sf_line = coast_sf_line
                                      , terra_options = list(memfrac = use_memfrac)
                                      )
                     , datatype = "INT4S" # integer metre accuracy
                     , pattern = map(tile_extents)
                     , cue = tar_cue(mode = "never")
                     )
    ### combine -------
    , tar_target(coast
                 , make_coast_raster(tiles = tile_coast
                                     , base_grid_path = base_grid_path
                                     , coast_sf = coast_sf
                                     , out_file = coast_tif_file
                                     # passed via ... to terra::lapp()
                                     , overwrite = TRUE
                                     , wopt = list(datatype = "INT4S"
                                                   , gdal = c("TILED=YES"
                                                            , "COPY_SRC_OVERVIEWS=YES"
                                                            , "COMPRESS=DEFLATE"
                                                            )
                                                   )
                                     )
                 , format = "file"
                 )
    )
  
}


# not 90 m targets ---------
if(yaml::read_yaml("settings/setup.yaml")$grain$res < 90) {
  
  distance <- list(
    ## coast -------
    , tar_target(coast
                 , terra_reproject(in_file = gsub("__\\d{2}"
                                                  , "__90"
                                                  , x = fs::path(coast_tif_file)
                                                  )
                                   , y = base_grid_path
                                   , filename = fs::path(coast_tif_file)
                                   , method = "bilinear"
                                   , datatype = "INT4S"
                                   , overwrite = TRUE
                                   )
                 )
  )
  
}

list(targets, distance)
