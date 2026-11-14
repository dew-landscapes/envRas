
library(targets)
library(sf) # some list items are sf and the list won't open properly if sf package isn't in global env
library(tmap)

tmap_mode("view")
tmap_options(basemap.server = c(OSM = "OpenTopoMap"
                          , Imagery = "Esri.WorldImagery"
                          )
             )

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

script <- "sat_var"
targets::tar_config_set(store = tars[[script]]$store
                        , script = tars[[script]]$script
                        )

if(FALSE) {
  
  # individual tar_make-------
  
  tar_visnetwork(label = "time")
  
  tar_poll()
  tar_dispatched()
  
  tar_meta(fields = any_of("error"), complete_only = TRUE)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE)
  
}

if(FALSE) {
  
  cube_dir <- tar_read(cube_directory, store = tars$satellite$store)
  
  env_df <- envRaster::name_env_tif(x = dirname(cube_dir), parse = TRUE) |>
    dplyr::mutate(start_date = as.Date(start_date)) |>
    dplyr::group_by(layer, func) |>
    dplyr::filter(start_date == max(start_date)) |>
    dplyr::ungroup()
  
  env_df |>
    dplyr::pull(path)|>
    purrr::walk(\(x) terra::rast(x) |>
                  terra::plot(main = gsub("\\.tif", "", basename(x)))
                )
  
}

if(FALSE) {
  
  # check last tifs --------
  tar_read(cube_directory) |>
    envRaster::name_env_tif(parse = TRUE) |>
    dplyr::mutate(mod_time = purrr::map(path, \(x) fs::file_info(x)$modification_time)) |>
    tidyr::unnest(cols = c(mod_time)) |>
    dplyr::arrange(desc(mod_time)) |>
    dplyr::select(layer, func, start_date, path, mod_time) |>
    dplyr::slice(1:16) |>
    dplyr::pull(path) |>
    envRaster::make_env_stack() |>
    terra::plot()
  
}

