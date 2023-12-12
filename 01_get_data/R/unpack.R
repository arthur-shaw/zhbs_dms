# ==============================================================================
# Define functions
# ==============================================================================

#' Unpack zip file to a folder bearing its name
#' 
#' Rather than unpack a file to the directory in which the file sits,
#' create a folder with the file's name (minus extension) and 
#' unpack its contents there.
#' 
#' @param zipfile Character. Full file path of the zip file.
#' 
#' @return Side-effect of creating a folder and unpacking zip contents there.
#' 
#' @importFrom fs path_dir path_file path_ext_remove
#' @importFrom zip unzip
unpack_to_dir <- function(zipfile) {

    parent_dir <- fs::path_dir(zipfile)
    file_name <- fs::path_file(zipfile)
    unpack_name <- fs::path_ext_remove(file_name) 
    unpack_dir <- paste0(parent_dir, "/", unpack_name)

    zip::unzip(
        zipfile = zipfile,
        exdir = unpack_dir
    )
}

# ==============================================================================
# Unpack data
# ==============================================================================

# obtain list of zip files
files <- fs::dir_ls(
    path = downloaded_dir, 
    type = "file", 
    regexp = "\\.zip$", 
    recurse = FALSE
)

# unpack all identified zip files
purrr::walk(
    .x = files,
    .f = ~ unpack_to_dir(.x)
)
