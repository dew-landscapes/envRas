
  library(magrittr)

  ras_dir <- fs::path("D:"
                      , "env"
                      , "data"
                      , "raster"
                      # , "cube__P3M"
                      # , "DEA__ga_ls8c_ard_3--ga_ls9c_ard_3__sa_ibrasub_xn__0__30"
                      , "aligned"
                      , "sa_ibrasub_xn__0__30"
                      )
  
  intos <- if(!grepl("sa_ibrasub_xn", ras_dir)) {
    
    c("source", "collection", "res", "epoch", "season", "band")
    
  } else {
    
    c("band", "start_date")
    
  }
  
  files <- ras_dir %>%
    envRaster::parse_env_tif()
  
  r <- stars::read_stars(files$path, proxy = TRUE) %>%
    setNames(files$band)
  
  # tmap::tmap_mode("view")
  
  tmap::tmap_options(max.raster = c(plot = 10000, view = 10000))
  
  tmap::tm_shape(r) +
    tmap::tm_raster()
  