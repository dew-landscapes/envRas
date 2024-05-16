
distrib_rast <- function(taxon = NULL,
                         sources = c("epbc", "expert"), # "redlist"),
                         outdir = fs::path("H:", "data", "raster", "distribution", "sa_ibrasub_xn____0__90"),
                         base = settings$base,
                         get_distrib_ds = TRUE,
                         silent = TRUE){
  
  
  # setup -----
  if(get_distrib_ds){
    distrib_ds <- dists_source( #from envPIA/function
      taxon=NULL,
      sources = c("epbc","expert"),
      source_rank = TRUE,
      standardise_taxonomy = FALSE
    )
  }
  
  # setGDALconfig("GDAL_PAM_ENABLED", "FALSE") #disable writing of auxilary files with rasters -- if false, categorical rasts will save as integer
  
  
  # Expert distributions --------
  if("expert" %in% sources){
    
    expert_ds <- distrib_ds %>% 
      dplyr::filter(ds == "expert")
    
    if(!is.null(taxon)){
      expert_ds <- expert_ds %>% 
        dplyr::filter(grepl(taxon, taxa, ignore.case = T))
    } 
    
    for (i in 1:nrow(expert_ds)){
      
      taxa <- expert_ds$taxa[i]
      
      
      if(!file.exists(fs::path(outdir, "expert", 
                               paste0(taxa, ".tif")))){
        
        if(!silent){print(taxa)}
        
        fs::dir_create(outdir, "expert")
        
        distrib <- sfarrow::st_read_parquet(expert_ds$file[i]) %>% 
          dplyr::mutate(taxa = taxa) %>% 
          #merge multiple shapes 
          dplyr::mutate(shape = sf::st_union(shape)) %>% 
          dplyr::distinct(taxa, .keep_all = T)
        # geom to spatvec and set projection
        shape <- distrib$shape
        vect <- terra::vect(shape)
        proj <- terra::project(vect, "epsg:7845")
        
        # mask and crop base raster then fill with NA to match base extent 
        crop <- terra::crop(base, proj)
        mask <- terra::mask(crop, proj)
        extend <- terra::extend(mask, base) 
        
        # save taxa.tif
        terra::writeRaster(extend, 
                           filename = fs::path(outdir, "expert", 
                                               paste0(taxa, ".tif")),
                           overwrite = T, datatype = "INT1U")
      }
    }
  }
  
  # EPBC distributions ---------
  if("epbc" %in% sources){
    
    epbc_ds <- distrib_ds %>%
      filter(ds == "epbc")
    
    if(!is.null(taxon)){
      epbc_ds <- epbc_ds %>% 
        dplyr::filter(grepl(taxon, taxa, ignore.case = T))
    } 
    
    for(i in 1:nrow(epbc_ds)){
      # Read parquets one at a time
      taxa <- epbc_ds$taxa[i]
      
      
      if(!file.exists(fs::path(outdir, "epbc",
                               paste0(taxa, ".tif")))){
        
        if(!silent){print(taxa)}
        
        fs::dir_create(outdir, "epbc")
        
        distrib <- sfarrow::st_read_parquet(epbc_ds$file[epbc_ds$taxa==taxa]) %>%
          dplyr::mutate(taxa = taxa,
                        rank = factor(presence_category, levels = c("Species or species habitat may occur",
                                                                    "Species or species habitat likely to occur"))
          ) %>%
          #merge multiple shapes per category/rank
          dplyr::group_by(rank) %>%
          dplyr::mutate(shape = sf::st_union(shape)) %>%
          dplyr::ungroup() %>%
          dplyr::distinct(rank, .keep_all = T)
        
        #create rasters
        for(j in 1:nrow(distrib)){
          proj <-  terra::vect(distrib$shape[j]) %>%
            terra::project( "epsg:7845")
          
          # mask and crop base raster
          crop <- terra::crop(base, proj) %>%
            terra::setValues(distrib$rank[j])
          mask <- terra::mask(crop, proj)
          
          #raster for each category, filled to matching sa_ibrasub_xn extent
          assign(paste0("extend", j), terra::extend(mask, base))
          
        }
        
        # if both presence categories exist, combine & save
        if(nrow(distrib) == 2){
          
          merge <- terra::merge(extend1, extend2)
          terra::writeRaster(merge,
                             filename = fs::path(outdir, "epbc",
                                                 paste0(taxa, ".tif")),
                             datatype = "INT1S")
        }
        
        #otherwise just save last step
        if(nrow(distrib) == 1){
          terra::writeRaster(extend1,
                             filename = fs::path(outdir, "epbc",
                                                 paste0(taxa, ".tif")),
                             datatype = "INT1S")
        }
        
      }
      
      suppressWarnings(rm(distrib, proj, crop, mask, extend1))
      gc()
    }
  }
}





