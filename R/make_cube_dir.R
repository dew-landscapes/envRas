make_cube_dir <- function(set_scale
                          , set_source
                          , cube_dir
                          ) {
  
  dir <- fs::path(cube_dir
                  , envRaster::name_env_tif(x = c(set_scale$extent
                                                  , set_scale$grain
                                                  , source = list(set_source$source)
                                                  , collection = list(set_source$collection)
                                                  )
                                            , dir_only = TRUE
                                            , prefixes = c("sat", "use")
                                            , fill_null = TRUE
                                            )$out_dir
                  )
  
  dir <- gsub("__\\/", "/", dir)
  
  fs::dir_create(dir)
  
}