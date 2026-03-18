path_create_tar <- function(path) {
  
  if (!dir.exists(path)) {
    
    dir.create(path, recursive = TRUE)
    
  }
  
  return(path)
  
}