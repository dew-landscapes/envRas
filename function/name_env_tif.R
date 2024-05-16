

#' Use meta data to name raster cube paths/files
#'
#' Either create file path for saving a cube or parse the meta data from a cube
#'
#' @param x Either dataframe with path(s) to parse (in column `path`), character
#' vector of path(s) to search, or named list object.
#' @param dir_only Logical.
#' @param prefixes Character. If x is a named list, any name prefixes to remove
#' before matching to naming structure.
#' @param file_type Character. e.g. "tif". More than one can be provided but if
#' `parse` then `file_type[1]` is used for `out_file` file type.
#' @param parse Logical. If `FALSE` an `out_file` path will be returned rather
#' than parsed. Assumes the appropriately names columns are supplied in `df`.
#' @param fill_null Logical. If `TRUE`, will fill up to `x_null` definitions
#' with NULL (and issue a warning).
#' @param x_null Numeric. Even if `fill_null` is `TRUE`, if there are more than
#' `x_null` missing definitions, an error will be thrown.
#'
#'
#' @return If `!parse`, tibble with columns:
#' \describe{
#'   \item{vector}{Vector layer originally used to define area of interest}
#'   \item{filt_col}{Column name from vector to filter to define area of interest}
#'   \item{level}{Level(s) of filt_col originally filtered to define area of interest}
#'   \item{buffer}{Any buffer around area of interest}
#'   \item{period}{Period represented within layer using ISO8601-compliant time
#'   period for regular data cubes. e.g., "P16D" for 16 days}
#'   \item{res}{Resolution of output raster in units of rasters crs}
#'   \item{source}{e.g. `DEA` is digital earth Austrlia}
#'   \item{collection}{e.g. `ga_ls8c_ard_3` is landsat 8 collection in DEA}
#'   \item{layer}{Band or layer name}
#'   \item{start_date}{Start date for layer}
#'   \item{type}{File type of `path`.}
#'   \item{path}{Full (relative) path including `file_type`.}
#' }
#'
#' If `!parse`, `df` with additional column `out_file`
#'
#' @export
#'
#' @examples
name_env_tif <- function(x
                         , dir_only = FALSE
                         , prefixes = c("sat", "use")
                         , file_type = c("\\.tif", "\\.nc")
                         , parse = FALSE
                         , fill_null = FALSE
                         , x_null = 3
                         , ...
                         ) {

  context_defn <- c("vector"
                   , "filt_col"
                   , "level"
                   , "buffer"
                   )

  cube_defn <- c("period"
                 , "res"
                 )

  source_defn <- c("source"
                  , "collection"
                  )

  layer_defn <- c("layer"
                  , "season"
                  , "start_date"
                  , "file_type"
                  )


  df <- if(!"data.frame" %in% class(x)) {

    if("character" %in% class(x)) {
      
      if(dir.exists(x)) {

        dir(x
            , full.names = TRUE
            , recursive = TRUE
            ) %>%
          tibble::enframe(name = NULL
                          , value = "path"
                          ) %>%
          dplyr::filter(grepl(paste0(file_type, collapse = "|")
                              , path
                              )
                        )
        
      } else {
        
        tibble::tibble(path = x)
        
      }

    } else if("list" %in% class(x)) {

      get_names <- gsub(paste0("^", prefixes, "_", collapse = "|")
                        , ""
                        , names(x)
                        ) %in% c(context_defn, cube_defn
                                 , source_defn, layer_defn
                                 )

      x[get_names] %>%
        purrr::map(paste0, collapse = "--") %>%
        list2DF() %>%
        tibble::as_tibble() %>%
        purrr::set_names(gsub(paste0("^", prefixes, "_", collapse = "|"), "", names(.)))

    }

  } else x

  if(parse) {

      res <- df %>%
        dplyr::mutate(context = if(!dir_only) basename(dirname(dirname(dirname(path)))) else basename(dirname(dirname(path)))
                      , cube = if(!dir_only) basename(dirname(dirname(path))) else  basename(dirname(path))
                      , source = if(!dir_only) basename(dirname(path)) else basename(path)
                      , file = if(!dir_only) file = basename(path) else NULL
                      ) %>%
        tidyr::separate(context
                        , into = context_defn
                        , sep = "__"
                        ) %>%
        tidyr::separate(cube
                        , into = cube_defn
                        , sep = "__"
                        ) %>%
        tidyr::separate(source
                        , into = source_defn
                        , sep = "__"
                        )
      
      if(!dir_only) {
        
        res <- res %>%
          tidyr::separate(file
                        , into = layer_defn
                        , sep = "__|\\."
                        )
        
      }
      
      res <- res %>%
        dplyr::relocate(-path)

  } else {
    
    if(fill_null) {
      
      # check all names are in df
      
      missing <- setdiff(c(context_defn
                           , cube_defn
                           , source_defn
                           , if(!dir_only) layer_defn
                           )
                         , names(df) 
                         )
      
      if(length(missing) > x_null) {
        
        stop(length(missing)
             , " definitions are missing: "
             , envFunc::vec_to_sentence(missing)
             )
        
      }
      
      warning("All of "
              , envFunc::vec_to_sentence(missing)
              , " will be NULL"
              )
      
      if(length(missing)) {
        
        df <- cbind(df
                    , purrr::map(missing
                                 , \(x) tibble::tibble(!!rlang::ensym(x) := list(NULL))
                                 )
                    )
        
      }
      
    }

    res <- df %>%
      tidyr::unite("context"
                  , tidyselect::all_of(context_defn)
                   , sep = "__"
                   ) %>%
      tidyr::unite("cube"
                   , tidyselect::all_of(cube_defn)
                   , sep = "__"
                   ) %>%
      tidyr::unite("source"
                   , tidyselect::all_of(source_defn)
                   , sep = "__"
                   ) %>%
      {if(dir_only) (.) else (.) %>% tidyr::unite("layer"
                                                  , tidyselect::all_of(layer_defn[!layer_defn == "file_type"])
                                                  , sep = "__"
                                                  )
        } %>%
      dplyr::mutate(out_file = fs::path(context
                                        , cube
                                        , source
                                        )
                    ) %>%
      {if(dir_only) (.) else (.) %>%
          dplyr::mutate(out_file = fs::path(out_file
                                            , paste0(layer
                                                     , "."
                                                     , gsub("\\\\\\."
                                                            , ""
                                                            , file_type[1]
                                                            )
                                                     )
                                            )
                        )
      }
    
    res <- res %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.character)
                                  , \(x) gsub("NULL", "", x)
                                  )
                    )

  }
  
  if(dir_only) res <- res %>%
    dplyr::rename(out_dir = out_file)

  return(res)

}
