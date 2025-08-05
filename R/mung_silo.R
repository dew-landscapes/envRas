

  mung_silo <- function(files
                        , func
                        , base = NULL
                        ) {
    
    r <- purrr::map(files
                    , \(x) {
                      r <- rast(x)
                      yr <- format(time(r[[1]]), "%Y")
                      r <- tapp(r, index = as.integer(format(time(r), "%m")), func)
                      names(r) <- month.abb
                      time(r) <- seq(as.Date(paste0(yr, "-01-01")), by = "month", l = 12)
                      return(r)
                    }
                    ) %>%
      terra::rast() %>%
      terra::tapp(index = as.integer(format(time(.), "%m")), fun = func)
  
    names(r) <- month.abb
        
    if(!is.null(base)) {
      
      r <- r %>%
        terra::project(base)
      
    }
    
    return(r)
    
  }
    