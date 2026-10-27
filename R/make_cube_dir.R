make_cube_dir <- function(set_scale
                          , set_source
                          , cube_dir
                          ) {
  
  a <- fs::path(cube_dir
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
  
  a <- gsub("__\\/", "/", a)
  
  if(!dir.exists(a)) {
  
    b <- base::dir.create(a, recursive = TRUE, showWarnings = FALSE)
    
    if(!b) stop("failed to create ", a)
    
  }
  
  return(a)
  
}