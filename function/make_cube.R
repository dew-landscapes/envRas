  

  make_cube <- function(cube
                        , out_reg
                        , settings
                        ) {
    
    reg_cube_file <- fs::path(out_reg, "reg_cube_file.rds")
    
    fs::dir_create(dirname(reg_cube_file))
    
    cube <- cube %>%
      dplyr::mutate(file_info = purrr::map(file_info
                                           , . %>%
                                             dplyr::filter(fs::file_size(path) > "10KB")
                                           )
                    ) %>%
      dplyr::filter(purrr::map_lgl(file_info, ~ nrow(.) > 0))
    
    # regular cube-------
    reg_cube <- sits::sits_regularize(cube
                                      , output_dir = out_reg
                                      , res = settings$use_res
                                      , period = settings$use_period
                                      , multicores = settings$use_cores
                                      )
    
    rio::export(reg_cube
                , reg_cube_file
                )
    
    
    # add indices-------
    # NDVI
    reg_cube <- sits::sits_apply(reg_cube
                                 , NDVI = envRaster::ndvi(NIR08, RED) #(B08 - B04) / (B08 + B04)
                                 , output_dir = out_reg
                                 , multicores = settings$use_cores
                                 )
    
    rio::export(reg_cube
                , reg_cube_file
                )
    
    
    # NBR
    reg_cube <- sits::sits_apply(reg_cube
                                 , NBR = envRaster::nbr2(SWIR16, SWIR22)
                                 , output_dir = out_reg
                                 , multicores = settings$use_cores
                                 )
    
    rio::export(reg_cube
                , reg_cube_file
                )
    
    
    # NDMI
    reg_cube <- sits::sits_apply(reg_cube
                                 , NDMI = envRaster::ndmi(NIR08, SWIR16)
                                 , output_dir = out_reg
                                 , multicores = settings$use_cores
                                 )
    
    rio::export(reg_cube
                , reg_cube_file
                )
    
    rio::export(reg_cube
                , gsub("reg_cube", "final_cube", reg_cube_file)
                )
    
    return(reg_cube)
    
  }
  