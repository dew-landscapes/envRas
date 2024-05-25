
  check_success <- function(dir
                            , tif_name
                            , removes = NULL
                            ) {
    
    use_tif_name <- if(!is.null(removes)) {
      
      gsub(paste0(removes, collapse = "|"), "", tif_name)
      
    } else tif_name
    
    success <- fs::dir_ls(dir
                            , regexp = paste0(use_tif_name, ".*log$", collapse = "|")
                            ) %>%
      tibble::enframe(name = NULL, value = "path") %>%
      dplyr::mutate(layer = gsub("__.*", "", basename(path))
                    , log = purrr::map(path, \(x) readr::read_file(x))
                    , log = purrr::map(log, \(x) paste0(x, collapse = "\n"))
                    , err = purrr::map_lgl(log, \(x) grepl("ERROR", x))
                    ) %>%
      dplyr::filter(!err) %>%
      dplyr::pull(layer)
    
    tif_name <- if(length(success) == 0) {
      
      tif_name
      
    } else {
      
      tif_name[!grepl(paste0(success, collapse = "|"), tif_name)]
      
    }
    
    return(tif_name)
    
  }
  