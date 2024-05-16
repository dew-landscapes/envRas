  
  _get_sat_data <- function(out_dir
                           , start
                           , end
                           , settings
                           , force_new = FALSE
                           , check_cube = TRUE
                           , fid_regex = NULL
                           ) {
    
    out_irr <- fs::path(out_dir, "irr")
    
    fs::dir_create(mget(ls(pattern = "out_")))
    
    rem_cube_file <- fs::path(out_irr
                              , "rem_cube.rds"
                              )
    
    irr_cube_file <- fs::path(out_irr
                              , "irr_cube.rds"
                              )
    
    make_new <- if(!file.exists(rem_cube_file)) TRUE else force_new
    
    if(make_new) {
      
      # irregular cube-------
      rem_cube <- sits::sits_cube(source = settings$use_source
                                  , collection = settings$use_collection
                                  , roi = settings$boundary
                                  , start_date = start
                                  , end_date = end
                                  )
      
    } else {
      
      rem_cube <- rio::import(rem_cube_file)
      
    }
    
    if(!is.null(fid_regex)) {
      
      # filter cube?------ e.g. landsat 8 only
      rem_cube <- rem_cube %>%
        dplyr::mutate(file_info = purrr::map(file_info
                                             , . %>%
                                               dplyr::filter(grepl(paste0(fid_regex, collapse = "|")
                                                                   , fid
                                                                   )
                                                             )
                                             )
                      )
      
    }
      
    # download-------
    irr_cube <- sits::sits_cube_copy(rem_cube
                                     , output_dir = out_irr
                                     )
    
    # save cube ------
    rio::export(irr_cube
                , irr_cube_file
                )
    
    return(irr_cube)
    
  }
  