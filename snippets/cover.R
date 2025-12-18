
library(targets)
tars <- yaml::read_yaml("_targets.yaml")

tar_load(cube_directory, store = tars$satellite$store)

r_to_fix <- fs::path(cube_directory, "swir_1__median__2015-01-01.tif")
r_to_fix_copy <- gsub("\\.tif", "_copy.tif", r_to_fix)
r_to_fix_with <- fs::path(cube_directory, "_swir_1__median__2015-01-01.tif")

fs::file_copy(fs::path(r_to_fix)
              , fs::path(r_to_fix_copy)
              )

fs::file_delete(r_to_fix)

terra::cover(terra::rast(r_to_fix_copy)
             , terra::rast(r_to_fix_with)
             , filename = r_to_fix
             )

if(FALSE) {
  
  # CHECK RESULTS BEFORE DELETING THESE FILES
  fs::file_delete(c(r_to_fix_copy, r_to_fix_with))
  
}