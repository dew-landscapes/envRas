check_remote_nc <- function(remote_file) {
  
  check_fun <- purrr::safely(ncdf4::nc_open)
  
  res <- check_fun(remote_file)
  
  if(is.null(res$result)) {
    
    FALSE
    
  } else {
    
    TRUE
    
  }
  
}
