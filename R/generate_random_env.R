generate_random_env <- function(ras_path
                                , extent_df
                                , out_epsg = 4326
                                ) {

  purrr::map(1:nrow(extent_df)
             , \(x) {
               
               if(!is.null(extent_df$random_points[[x]])) {
                 
                 r <- envRaster::make_env_stack(ras_path)
                 
                 terra::window(r) <- terra::ext(as.numeric(extent_df[x, 1:4]))
                 
                 e <- extent_df$random_points[[x]] |>
                   tibble::as_tibble() |>
                   dplyr::bind_cols(terra::extract(x = r
                                                   , y = extent_df$random_points[[x]]
                                                   ) |>
                                      tibble::as_tibble()
                                    ) |>
                   dplyr::filter(dplyr::if_any(3
                                               , \(x) !is.na(x)
                                               )
                                 )
                 
                 if(nrow(e) > 0) {
                 
                   e |>
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
                   
                 }
                     
               }
               
             }
             ) |>
    dplyr::bind_rows()

}

