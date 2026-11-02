get_items <- function(url
                      , collection
                      , bbox
                      , min_date
                      , max_date
                      , retries = 5
                      ) {
  
  get_items <- \() {
    
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
    
  }
  
  items <- NULL
  counter <- 0
  
  while(is.null(items) && counter < retries) {
  
    safe_get_items <- purrr::safely(get_items)
    
    it <- safe_get_items()
    
    if(!is.null(it$result)) items <- it$result
    
    counter <- counter + 1
    
  }
  
  if(is.null(items)) stop("items is NULL")
  
  return(items)
  
}
