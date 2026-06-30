generate_random_points <- function(extent_df
                                   , base_grid_path
                                   , samp_prop = 0.01
                                   , out_epsg = 4326
                                   , out_x = "long"
                                   , out_y = "lat"
                                   ) {

  purrr::map(1:nrow(extent_df)
             , \(x) {

               base <- terra::rast(base_grid_path)

               terra::window(base) <- terra::ext(as.numeric(extent_df[x, 1:4]))

               non_na_cells <- terra::cells(base)

               if(length(non_na_cells) > 0) {

                 samp_size <- round(length(non_na_cells) * samp_prop, 0)
                 
                 if(samp_size > 0) {

                   samp_points <- sample(non_na_cells, size = samp_size) |>
                     terra::xyFromCell(base
                                       , cell = _
                                       ) |>
                     tibble::as_tibble() |>
                     sf::st_as_sf(coords = c("x", "y")
                                  , crs = sf::st_crs(base)
                                  ) |>
                     sf::st_transform(crs = out_epsg) |>
                     sf::st_coordinates() |>
                     tibble::as_tibble() |>
                     dplyr::rename(!!rlang::ensym(out_x) := X
                                   , !!rlang::ensym(out_y) := Y
                                   )
                   
                 }

               }

             }
             ) |>
    purrr::compact() |>
    dplyr::bind_rows()

}

