generate_random_env <- function(ras_path
                                , extent_df
                                , out_epsg = 4326
                                , base_grid_path
                                , samp_prop = 0.01
                                ) {

  r <- terra::rast(ras_path)

  purrr::map(1:nrow(extent_df)
             , \(x) {

               base <- terra::rast(base_grid_path)

               terra::window(base) <- terra::ext(as.numeric(extent_df[x, 1:4]))

               non_na_cells <- terra::cells(base)

               if(length(non_na_cells) > 0) {

                 samp_size <- round(length(non_na_cells) * samp_prop, 0)
                 
                 if(samp_size > 0) {

                   samp_cells <- sample(non_na_cells, samp_size)
  
                   terra::window(r) <- terra::ext(as.numeric(extent_df[x, 1:4]))
  
                   terra::extract(x = r
                                  , y = samp_cells
                                  , xy = TRUE
                                  ) |>
                     tibble::as_tibble() |>
                     sf::st_as_sf(coords = c("x", "y")
                                  , crs = sf::st_crs(r)
                                  ) |>
                     sf::st_transform(crs = paste0("epsg:", out_epsg)) %>%
                     dplyr::bind_cols(sf::st_coordinates(.) |>
                                        tibble::as_tibble()
                                      ) |>
                     sf::st_set_geometry(NULL) |>
                     dplyr::select(cell_lat = Y
                                   , cell_long = X
                                   , everything()
                                   )
                   
                 } else tibble::tibble()

               } else tibble::tibble()

             }
             ) |>
    dplyr::bind_rows()

}

