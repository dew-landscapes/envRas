save_geoparquet <- function(object
                            , out_file
                            ) {
  
  if(!file.exists(dirname(out_file))) fs::dir_create(dirname(out_file))
                 
  sfarrow::st_write_parquet(object
                            , out_file
                            )
                 
  return(out_file)
  
}