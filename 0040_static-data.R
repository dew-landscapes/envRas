
if(FALSE) {
  
  start_date <- "1970-01-01"
  end_date <- "1970-01-01"
  
  # run wofs -----
  out_file <- fs::path(settings$wofs_dir
                       , paste0("wofs_filtered_summary__all__"
                                , start_date
                                , ".tif"
                                )
                       )
  
  rsi::get_stac_data(aoi = sf::st_bbox(settings$base) %>%
                       sf::st_as_sfc(crs = settings$epsg_proj)
                       
                     , start_date = start_date
                     , end_date = end_date
                     , pixel_x_size = settings[["sat_res", exact = TRUE]] 
                     , pixel_y_size = settings[["sat_res", exact = TRUE]] 
                     , asset_names = "frequency"
                     , stac_source = "https://explorer.sandbox.dea.ga.gov.au/stac"
                     , collection = "wofs_filtered_summary"
                     , output_filename = out_file
                     , composite_function = "median"
                     )
  
                     , query_function = rsi_query_api
                     , download_function = rsi_download_rasters
                     , sign_function = NULL
                     , rescale_bands = TRUE
                     , item_filter_function = NULL
                     , mask_band = NULL
                     , mask_function = NULL
                     , composite_function = c("merge", "median", "mean", "sum", "min", "max")
                     , limit = 999
                     , gdalwarp_options = c("-r", "bilinear", "-multi", "-overwrite", "-co"
                                            , "COMPRESS=DEFLATE", "-co", "PREDICTOR=2"
                                            , "-co", "NUM_THREADS=ALL_CPUS"
                                            )
                     , gdal_config_options = c(VSI_CACHE = "TRUE", GDAL_CACHEMAX = "30%"
                                               , VSI_CACHE_SIZE = "10000000", GDAL_HTTP_MULTIPLEX = "YES"
                                               , GDAL_INGESTED_BYTES_AT_OPEN = "32000"
                                               , GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", GDAL_HTTP_VERSION = "2"
                                               , GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES", GDAL_NUM_THREADS = "ALL_CPUS"
                                               , GDAL_HTTP_USERAGENT = "rsi (https://permian-global-research.github.io/rsi/)"
                                               )
                )
    
  items <- rstac::stac("https://explorer.sandbox.dea.ga.gov.au/stac") %>%
    rstac::stac_search(collections = settings[["wofs_filtered_summary", exact = TRUE]]
                       , bbox = settings[["bbox", exact = TRUE]]
                       , datetime = paste0(start_date
                                           , "/"
                                           , end_date
                                           )
                       ) %>%
    rstac::get_request()
  
  col <- gdalcubes::stac_image_collection(items$features
                                          , asset_names = "wofs_filtered_summary"
                                          )
  
  use_extent <- c(as.list(sf::st_bbox(settings$base))
                  , t0 = as.character(start_date)
                  , t1 = as.character(end_date)
                  )
  
  names(use_extent)[1:4] <- c("left", "bottom", "right", "top")

  v_num <- gdalcubes::cube_view(srs = paste0("EPSG:"
                                             , settings[["epsg_proj", exact = TRUE]]
                                             )
                                , extent = use_extent
                                , dx = settings[["sat_res", exact = TRUE]] 
                                , dy = settings[["sat_res", exact = TRUE]]
                                , dt = "P1D"
                                , aggregation = "median"
                                , resampling = "bilinear"
                                )   
  
  out_file <- fs::path(settings$wofs_dir
                       , paste0("wofs_filtered_summary__all__"
                                , start_date
                                , ".tif"
                                )
                       )
  
  run <- if(!file.exists(out_file)) TRUE else force_new
                      
  if(run) {
    
    if(!dir.exists(dirname(out_file))) fs::dir_create(dirname(out_file))
  
    r <- gdalcubes::raster_cube(col
                                , v_num
                                )
    
    gdalcubes::write_tif(r
                         , dir = dirname(out_file)
                         , prefix = paste0("wofs_filtered_summary"
                                           , "__all__"
                                           )
                         , pack = gdalcubes::pack_minmax(type = "int16"
                                                         , min = 0
                                                         , max = 1
                                                         )
                         , creation_options = list("COMPRESS" = "NONE")
                         )
    
  }
      
}                
  