save_dea_layer <- function(items
                           , base_grid
                           , layer
                           , agg_func = "median"
                           , start_date
                           , end_date
                           , cloud_mask = NULL
                           , base_dir
                           , period
                           , cores = 1
                           , force_new = TRUE
                           , out_file = fs::path(base_dir
                                                 , paste0(gsub("nbart_", "", layer), "__", agg_func, "__", start_date, ".tif")
                                                 )
                           , ...
                           , gdalcubes_config = list(VSI_CACHE = "TRUE",
                                                     VSI_CACHE_SIZE = "50000000",
                                                     GDAL_CACHEMAX = "5%",
                                                     GDAL_HTTP_MULTIPLEX = "NO",
                                                     GDAL_INGESTED_BYTES_AT_OPEN = "32000",
                                                     GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
                                                     GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
                                                     GDAL_HTTP_MAX_RETRY = "5",
                                                     GDAL_HTTP_RETRY_DELAY = "2",
                                                     GDAL_HTTP_TIMEOUT = "60",
                                                     GDAL_HTTP_CONNECTTIMEOUT = "20",
                                                     GDAL_NUM_THREADS = "1",
                                                     AWS_NO_SIGN_REQUEST = "YES",
                                                     AWS_REGION = "ap-southeast-2"
                                                     )
                           ) {
  
  stopifnot(length(out_file) == 1)
  
  if(all(file.exists(out_file), ! force_new)) return(out_file)
  
  if(! "rstac_doc" %in% class(items)) {
    
    items <- items[[1]]
  
  }
    
  if(items$numberReturned) {
    
    # gdalcubes config--------
    purrr::iwalk(gdalcubes_config
                 , \(x, idx) gdalcubes::gdalcubes_set_gdal_config(as.character(idx), as.character(x))
                 )
  
    gdalcubes::gdalcubes_options(parallel = cores)
    on.exit(gdalcubes::gdalcubes_options(parallel = FALSE))
    
    layer_to_get <- items |>
      rstac::items_assets() |>
      grep(paste0("^", layer, "$"), x = _, value = TRUE)
    
    coll <- gdalcubes::stac_image_collection(items$features
                                             , asset_names = layer_to_get
                                             )
    
    use_extent <- c(as.list(sf::st_bbox(base_grid)) |> purrr::set_names(c("left", "bottom", "right", "top"))
                    , t0 = as.character(start_date)
                    , t1 = as.character(end_date)
                    )
    
    view <- gdalcubes::cube_view(srs = paste0("EPSG:", terra::crs(base_grid, describe = T)$code)
                                 , dx = terra::res(base_grid)[1]
                                 , dy = terra::res(base_grid)[2]
                                 , dt = period
                                 , extent = use_extent
                                 , aggregation = agg_func
                                 , resampling = "bilinear"
                                 )
    
    r <- gdalcubes::raster_cube(coll
                                , view
                                , mask = if(!is.null(cloud_mask)) cloud_mask else NULL
                                ) |>
      gdalcubes::select_bands(layer_to_get)
    
    res <- gdalcubes::write_tif(r
                                , dir = base_dir
                                , prefix = paste0(gsub("nbart_", "", layer)
                                                  , "__"
                                                  , agg_func
                                                  , "__"
                                                  )
                                , ...
                                )
    
    if(! identical(res, out_file)) {
      
      fs::file_copy(res, out_file)
      fs::file_delete(res)
      
    }
    
    return(out_file)
    
  } else {
    
    return(NULL)
    
  }
  
}


                         