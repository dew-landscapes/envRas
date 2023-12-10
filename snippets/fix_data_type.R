
  orig_dir <- gsub("cube__P3M", "cube__P3M_tofix", settings$sat_cube_dir)
  #orig_dir <- fs::path(data_dir, "raster", "cube__P3M", basename(settings$sat_cube_dir))
 
  already_done <- dir_ls(orig_dir) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    parse_env_tif() %>%
    dplyr::mutate(out_file = fs::path(settings$sat_cube_dir, basename(path))
                  , raw_band = ! band %in% c("gdvi", "ndvi", "nbr", "nbr2")
                  , indice = band %in% c("gdvi", "ndvi", "nbr", "nbr2")
                  , done = file.exists(out_file)
                  , todo_band = as.logical(raw_band * !done)
                  , todo_indice = as.logical(indice * !done)
                  )
  
  
  # Bands -------
  purrr::walk2(already_done$path[already_done$todo_band]
               , already_done$out_file[already_done$todo_band]
               , ~ terra::writeRaster(x = terra::rast(.x)
                                      , filename = .y
                                      , wopt = list(datatype = "INT2U"
                                                    , gdal = c("COMPRESS = NONE")
                                                    )
                                      )
               )
  
  # Indices --------
  
  process_and_save <- function(in_ras, out_ras) {
    
    writeRaster(terra::rast(in_ras) * 10000
                , out_ras
                , overwrite = TRUE
                , datatype = "INT2S"
                , gdal = c("COMPRESS = NONE")
                )
    
    gc()
    
  }
  
  purrr::walk2(already_done$path[already_done$todo_indice]
               , already_done$out_file[already_done$todo_indice]
               , process_and_save
               )
  
  
  if(FALSE) {
    
    already_done$out_file[already_done$done] %>% terra::rast() %>% plot()
    
    
  }
  