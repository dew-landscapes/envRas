dir_create_tar <- function(dir) {
  
  if (!dir.exists(dir)) {
    
    dir.create(path, recursive = TRUE)
    
  }
  
  return(dir)
  
}