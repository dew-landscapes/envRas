
  r <- settings$munged_dir %>%
    dir_ls(regexp = "tif$") %>%
    terra::rast() %>%
    `[`("50%")
  