
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


if(FALSE) {
  
  # individual tar_make-------
  
  script <- "setup"
  
  tar_visnetwork(script = tars[[script]]$script
                 , store = tars[[script]]$store
                 , label = "time"
                 )
  
  tar_poll(store = tars[[script]]$store)
  
  tar_meta(fields = any_of("error"), complete_only = TRUE, store = tars[[script]]$store)
  tar_meta(fields = any_of("warnings"), complete_only = TRUE, store = tars[[script]]$store)
  
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

