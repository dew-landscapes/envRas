

  library(magrittr)
  library(ggplot2)

  eo_dirs <- c("H:/data/raster/aligned/Bakara__1000__30/")
  
  
  infos <- fs::dir_info(eo_dirs
                        , regexp = "tif$"
                        ) %>%
    dplyr::filter(type == "file") %>%
    dplyr::select(path) %>%
    dplyr::mutate(folder = basename(dirname(path))
                  , name = gsub("\\.tif", "", basename(path))
                  ) %>%
    tidyr::separate(folder
                    , into = c("aoi", "buffer", "res")
                    , sep = "__"
                    ) %>%
    tidyr::separate(name, into = c("source", "collection", "res", "epoch", "season", "band")
                    , sep = "__"
                    ) %>%
    dplyr::filter(grepl("^ga", collection)
                  , season == "summer"
                  ) %>%
    dplyr::mutate(s2 = stringr::str_count(collection, "--") > 1) %>%
    tidyr::nest(data = -c(s2, band, season)) %>%
    dplyr::mutate(stack = purrr::map(data, ~terra::rast(.$path))) %>%
    dplyr::select(season, band, s2, stack) %>%
    tidyr::pivot_wider(names_from = "s2", values_from = stack) %>%
    dplyr::mutate(with = purrr::map(`TRUE`, ~ terra::values(.[[1:3]]))
                  , without = purrr::map(`FALSE`, ~ terra::values(.[[1:3]]))
                  )
  
  infos %>%
    dplyr::select(!where(is.list), with, without) %>%
    tidyr::pivot_longer(!matches(c("season", "band"))
                        , names_to = "s2"
                        ) %>%
    dplyr::mutate(value = purrr::map(value, tibble::as_tibble)
                  , value = purrr::map(value, . %>% dplyr::mutate(cell = dplyr::row_number()))
                  ) %>%
    tidyr::unnest(cols = c(value)) %>%
    tidyr::pivot_longer(matches("%")
                        , names_to = "quantile"
                        ) %>%
    dplyr::group_by(season, band, s2, quantile) %>%
    #dplyr::filter(value >= quantile(value, probs = 0.01) & value <= quantile(value, probs = 0.99)) %>%
    dplyr::ungroup() %>%
    ggplot(aes(value, s2, fill = s2)) +
      ggridges::geom_density_ridges() +
      facet_grid(season ~ band
                 , scales = "free_x"
                 ) +
      scale_fill_viridis_d()
  
  
  terra::plot(infos$`TRUE`[[7]][[2]] - infos$`FALSE`[[7]][[2]]
       , col = viridis::viridis(50)
       #, range = c(min(values(r[[2]])), max(values(r[[2]])))
       #, main = paste0(unique(files$band), ": with s2 - without")
       )
  
  