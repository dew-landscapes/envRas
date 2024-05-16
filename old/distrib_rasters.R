

dist_rast <- function(outdir =  fs::path("H:", "data", "raster", "distribution"),
                      show_plot = FALSE
                      ){
  
  distrib_ds <- dists_source( #from envPIA/function
                             taxon=NULL,
                             sources = c("epbc","expert"),
                             source_rank = TRUE,
                             standardise_taxonomy = FALSE
    )
  
  base <- settings$base #can change with 0000_run.R
  
    
  # Collect parquet files as df, including adding taxa as a column
    distrib_parquet_df <- map_df(distrib_ds$file, ~ {
      taxa <- distrib_ds$taxa[distrib_ds$file == .x]
      sfarrow::st_read_parquet(.x) %>%
        dplyr::mutate(taxa = taxa, .before=1) 
    }) 
    
    # select best data source, eg "likely to occur" if exists --
    distrib_df <- distrib_parquet_df %>% 
      left_join(distrib_ds) %>% 
      mutate(presence_rank = tidyr::replace_na(presence_rank, 2),
             presence_category = dplyr::coalesce(presence_category, ds)) %>% 
      dplyr::select(taxa, presence_category, presence_rank, ds, file, dir) %>%
      dplyr::group_by(taxa) %>%
      dplyr::filter(presence_rank == max(presence_rank)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(source = case_when(
                    grepl("likely", presence_category) ~ "likelyoccur",
                    grepl("may", presence_category) ~ "mayoccur",
                    grepl("expert", presence_category) ~ "expert")
      )
    
    
    # collapse multiple entries (of best rank) into one per taxa
    distrib_df_shape <- as_tibble(distrib_df) %>% 
      dplyr::group_by(taxa) %>% 
      dplyr::mutate(shape = st_union(shape)) %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(taxa, .keep_all = T)

  
  # Convert geometry and rasterise
  for(i in 1:nrow(distrib_df_shape)){ 
    taxa <- distrib_df_shape$taxa[i]
    
    if(!file.exists(fs::path(outdir,  "sa_ibrasub_xn____0__90", 
                             paste0(taxa, "_", distrib_df_shape$source[i], ".tif")))){
      # geom to spatvec and set projection
      vect <- terra::vect(distrib_df_shape$shape[i])
      proj <- terra::project(vect, "epsg:7845")
      
      # mask and crop base raster then fill with NA to match base extent (faster than no crop)
      crop <- terra::crop(settings$base, proj)
      mask <- terra::mask(crop, proj)
      extend <- terra::extend(mask, base) 
      names(extend) <- taxa
      
      if(show_plot == TRUE){plot(extend)}
      
      # save taxa.tif
      terra::writeRaster(extend, fs::path(outdir,  "sa_ibrasub_xn____0__90", 
                                          paste0(taxa, "_", distrib_df_shape$source[i], ".tif")),
                         overwrite = F)
    }
  }
}






