download_nc_raw <- function(save_file
                            , remote_file
                            , force_new = FALSE
                            ) {
  
  if(any(!file.exists(save_file), force_new)) {
    
    result <- download.file(url = remote_file
                            , destfile = save_file
                            , mode = "wb"
                            )
    
    if(result != 0) save_file <- NULL
                        
  }
  
  return(save_file)

}
