

  eo_dirs <- c("D:/env/data/raster/aligned/Bakara__1000__30/")
  
  
  infos <- fs::dir_info(eo_dirs) %>%
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
    #dplyr::filter(season == "summer") %>%
    #dplyr::filter(band == sample(band, 1)) %>%
    #dplyr::filter(start_date == sample(start_date, 1)) %>%
    dplyr::mutate(s2 = stringr::str_count(collection, "--") > 1) %>%
    tidyr::nest(data = -c(s2, band, season)) %>%
    dplyr::mutate(stack = purrr::map(data, ~terra::rast(.$path))) %>%
    dplyr::select(season, band, s2, stack) %>%
    tidyr::pivot_wider(names_from = "s2", values_from = stack) %>%
    dplyr::mutate(r = purrr::map2(`TRUE`
                                 , `FALSE`
                                 , ~ (.x - .y) / .x
                                 )
                  , vals = purrr::map(r, values)
                  , vals = purrr::map(vals, tibble::as_tibble)
                  , with = purrr::map(`TRUE`, values)
                  , without = purrr::map(`FALSE`, values)
                  )
  
  infos %>%
    dplyr::select(!where(is.list), with, without) %>%
    tidyr::pivot_longer(!matches(c("season", "band"))
                        , names_to = "s2"
                        ) %>%
    dplyr::mutate(value = purrr::map(value, tibble::as_tibble)
                  , value = purrr::map(value, . %>% dplyr::mutate(cell = row_number()))
                  ) %>%
    tidyr::unnest(cols = c(value)) %>%
    tidyr::pivot_longer(matches("%")
                        , names_to = "quantile"
                        ) %>%
    dplyr::group_by(season, band, s2, quantile) %>%
    dplyr::filter(value >= quantile(value, probs = 0.01) & value <= quantile(value, probs = 0.99)) %>%
    dplyr::ungroup() %>%
    ggplot(aes(value, s2, fill = s2)) +
      geom_density_ridges() +
      facet_grid(season ~ band
                 , scales = "free_x"
                 ) +
      scale_fill_viridis_d()
  
  
  plot(r[[1]]
       , col = viridis::viridis(50), range = c(min(values(r[[2]])), max(values(r[[2]])))
       , main = paste0(unique(files$band), ": with s2 - without")
       )
  
  