save_soil_layer <- function(paths_df
                            , key = Sys.getenv("TERN_API_KEY")
                            , out_file
                            , bbox
                            , force_new = FALSE
                            ) {
  
  r <- SLGACloud::cogLoad(paths_df$COGsPath
                          , api_key = key
                          )
  
  terra::window(r) <- terra::vect(bbox)
  
  r <- terra::app(clay
                  , fun = "mean"
                  , na.rm = TRUE
                  , filename = out_file
                  )
  
  
}