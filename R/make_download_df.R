make_download_df <- function(date_df
                             , settings
                             ) {
  
  date_df |>
    dplyr::cross_join(tibble::tibble(layer = settings$layers)) |>
    dplyr::cross_join(tibble::tibble(func = settings$func)) |>
    dplyr::left_join(envRaster::ras_layers |>
                       dplyr::select(layer, scale, offset)
                     )
  
}