make_cube_dir <- function(set_scale
                          , set_source
                          , cube_dir
                          ) {
  
  fs::path(cube_dir
           , envRaster::name_env_tif(x = c(set_scale$extent
                                           , set_scale$grain
                                           , source = set_source$source
                                           , collection = paste0(set_source$collection, collapse = "__")
                                           )
                                     , context_defn = c("vector", "filt_col", "filt_level", "buffer")
                                     , cube_defn = c("temp", "res")
                                     , dir_only = TRUE
                                     , prefixes = c("sat", "use")
                                     , fill_null = TRUE
                                     )$out_dir
           ) |>
    fs::dir_create()
  
}