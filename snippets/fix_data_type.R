
  # setup -------
  problem_dir <- settings$env_stack
  
  dir_create(fs::path(problem_dir, "to_fix"))
  
  indices <- c("gdvi", "ndvi", "nbr", "nbr2")

  to_fix <- fs::dir_ls(problem_dir
                       , regexp = "tif$"
                       ) %>%
    tibble::enframe(name = NULL, value = "path") %>%
    dplyr::filter(!grepl("base\\.tif$", path)) %>%
    parse_env_tif(cube = FALSE) %>%
    dplyr::mutate(temp_file = fs::path(problem_dir, "to_fix", basename(path))
                  , mult = dplyr::case_when(band %in% indices & source == "DEA" ~ 10000
                                            , !band %in% indices & source == "DEA" ~ 1
                                            , source == "NCI" ~ 100
                                            )
                  , type = dplyr::case_when(band %in% indices & source == "DEA" ~ "INT2S"
                                            , !band %in% indices & source == "DEA" ~ "INT2U"
                                            , source == "NCI" ~ "INT2S"
                                            )
                  )
  
  # check max and min values
  to_fix %>%
    dplyr::filter(source == "NCI") %>%
    dplyr::pull(temp_file) %>%
    terra::rast() %>%
    `[` ("50%") %>%
    terra::minmax()
  
  # move problem files --------
  fs::file_move(to_fix$path, to_fix$temp_file)
  
  # fix  --------
  
  process_and_save <- function(in_ras, out_ras, mult, type) {
    
    writeRaster(x = terra::rast(in_ras)["50%"] * mult
                , filename = out_ras
                , overwrite = TRUE
                , datatype = type
                , gdal = c("COMPRESS = NONE")
                )
    
    gc()
    
  }
  
  purrr::pwalk(list(to_fix$temp_file
                    , to_fix$path
                    , to_fix$mult
                    , to_fix$type
                    )
               , process_and_save
               )
  
  
  if(FALSE) {
    
    to_fix$path[24] %>% terra::rast() %>% plot()
    
    
  }
  