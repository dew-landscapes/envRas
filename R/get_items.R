get_items <- function(url
                      , collection
                      , bbox
                      , min_date
                      , max_date
                      ) {
  
  items <- rstac::stac(url) |>
    rstac::stac_search(collections = collection
                       , bbox = bbox
                       , datetime = paste0(as.character(min_date)
                                           , "/"
                                           , as.character(max_date)
                                           )
                       ) |>
    rstac::get_request() |>
    rstac::items_fetch()
  
  return(items)
  
}