  
  make_sat_cube <- function(base
                            , start_date
                            , end_date
                            , season
                            , out_dir
                            , collections
                            , period
                            , layers = c("green", "blue", "red")
                            , indices = list(gdvi = c("green", "nir")
                                             , ndvi = c("nir", "red")
                                             )
                            , mask = list(band = "oa_fmask"
                                         , mask = c(2, 3)
                                         )
                            , force_new = FALSE
                            , attempts = 3
                            , chunks = c(2, 2) # divisor for rows and cols
                            , chunk_dir = fs::path(Sys.getenv("TMPDIR"), "sat_cube_temp")
                            , chunk_prefix = "tile_"
                            , clean_temp = TRUE
                            , ... # passed to get_sat_data
                            ) {
    
    # capture args -------
    use_args <- c(as.list(environment()), list(...))
    use_args <- use_args[!grepl("base|out_dir|chunk|clean_temp", names(use_args))] # don't pass these to get_sat_data
    
    # stack_key ------
    stack_key <- paste0(season, "__", start_date)
    
    message(stack_key)
    
    # expected out files ------
    expected_files <- tibble::tibble(out_file = c(layers, names(indices))
                                     , start_date = start_date
                                     , season = season
                                     ) %>%
      dplyr::mutate(out_file = fs::path(out_dir
                                        , paste0(out_file
                                                 , "__"
                                                 , stack_key
                                                 , ".tif"
                                                 )
                                        )
                    , done = file.exists(out_file)
                    )
    
    check_files <- expected_files %>%
      dplyr::filter(!done)
    
    if(nrow(check_files)) {
      
      # create chunks -------
      
      fs::dir_create(fs::path(chunk_dir, stack_key))
      
      # makeTiles seems to deal with non-integer y, so values in chunk don't matter too much.
      chunks <- terra::makeTiles(base
                                 , y = c(terra::nrow(base) / chunks[1], terra::ncol(base) / chunks[2])
                                 , filename = fs::path(chunk_dir, stack_key, "tile_.tif")
                                 , na.rm = TRUE
                                 )
      
      # get chunk data-------
      purrr::map(chunks
                 , \(chunk) do.call("get_sat_data"
                                    , args = c(x = chunk
                                               , out_dir = fs::path(dirname(chunk), gsub("\\.tif", "", basename(chunk)))
                                               , use_args
                                               )
                                    )
                 )
      
      tifs <- c(layers, names(indices)) %>%
        tibble::enframe(name = NULL, value = "layer") %>%
        dplyr::mutate(tifs = purrr::map(layer
                                        , \(x) fs::dir_ls(fs::path(chunk_dir, stack_key)
                                                          , regexp = paste0(x, "__.*tif$")
                                                          , recurse = 1
                                                          )
                                        )
                      )
    
      if(nrow(tidyr::unnest(tifs, cols = c(tifs))) == nrow(expected_files) * length(chunks)) {
      
        fs::dir_create(out_dir)
        
        # put chunks together -------
        purrr::walk2(tifs$layer
                     , tifs$tifs
                     , \(x, y) {
                       
                       tif_dir <- unique(dirname(y))
                       
                       r <- terra::vrt(y)
                       
                       scale <- terra::scoff(terra::rast(y[[1]]))[[1]]
                       offset <- terra::scoff(terra::rast(y[[1]]))[[2]]
                       
                       out_file <- fs::path(out_dir, paste0(x, "__", season, "__", start_date, ".tif"))
                       
                       run <- if(file.exists(out_file)) force_new else TRUE
                       
                       if(run) {
                       
                         terra::writeRaster(r
                                            , filename = out_file
                                            , names = x
                                            , datatype = "INT2S"
                                            , scale = scale
                                            , offset = offset
                                            , gdal = c("COMPRESS = NONE")
                                            , overwrite = TRUE
                                            )
                         
                          # write log ------
                          logs <- tibble::tibble(path = fs::dir_ls(tif_dir
                                                                   , regexp = paste0(season, "__", start_date, ".*log$")
                                                                   , recurse = TRUE
                                                                   )
                                                 ) %>%
                            dplyr::mutate(layer = gsub("\\.log", "", basename(path))
                                          , tile = basename(dirname(path))
                                          , result = purrr::map(path, \(x) readr::read_lines(x))
                                          ) %>%
                            tidyr::unnest(cols = c(result)) %>%
                            dplyr::filter(grepl("100|ERROR", result)) %>%
                            dplyr::distinct() %>%
                            dplyr::mutate(result = dplyr::case_when(grepl("ERROR", result) ~ gsub("\n", "", result)
                                                                    , grepl("100", result) ~ "success"
                                                                    , TRUE ~ result
                                                                    )
                                          ) %>%
                            dplyr::arrange(layer, tile)
                          
                          rio::export(logs
                                      , file = fs::path(out_dir, paste0(season, "__", start_date, "__log.csv"))
                                      )
                         
                         if(clean_temp) {
                           
                           if(file.exists(out_file)) {
                             
                             fs::file_delete(y)
                             
                           }
                           
                         }
                         
                       }
                       
                       rm(r)
                       
                       gc()
                      
                     }
                     )
        
      } else {
        
        message("not enough tiles succeeded for: "
                , paste0(season, " ", start_date)
                , ". check DEA explorer status at https://monitoring.dea.ga.gov.au/783471032 and/or try increasing 'attempts' argument"
                )
      
      }
      
    } else message("all files done")
      
    # return------
    
    return(invisible(NULL))
    
  }
  