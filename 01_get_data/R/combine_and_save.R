# ==============================================================================
# Define functions
# ==============================================================================

#' Combine and save Stata data files
#' 
#' @param file_info_df Data frame. Return value of `fs::file_info()` that contains an additioal column `file_name`.
#' @param name Character. Name of the file (with extension) to ingest from all folders where it is found.
#' @param dir Character. Directory where combined data will be saved.
#' 
#' @return Side-effect of creating data frame objects in the global environment with the name `name`.
#' 
#' @importFrom dplyr `%>%` filter pull
#' @importFrom purrr map_dfr
#' @importFrom haven read_dta
#' @importFrom fs path_ext_remove
combine_and_save <- function(
    file_info_df,
    name,
    dir
) {

    # file paths
    # so that can locate data files to combine
    file_paths <- file_info_df %>%
        dplyr::filter(.data$file_name == name) %>%
        dplyr::pull(path)

    # data frame
    # so that can assign this value to a name
    df <- purrr::map_dfr(
            .x = file_paths,
            .f = ~ haven::read_dta(file = .x)
        )

    # TODO: attempt to add tryCatch above
    # so if any fail, there will be a list of where it fails and why

    # assign df to a name in the global environment
    # so that can loop over names without
    # assign(
    #     x = fs::path_ext_remove(name),
    #     value = df,
    #     envir = .GlobalEnv
    # )

    # save to destination directory
    haven::write_dta(data = df, path = fs::path(dir, name))

}

# ==============================================================================
# Combine each same-named file
# ==============================================================================

# obtain list of all directories of unpacked zip files
dirs <- fs::dir_ls(
    path = downloaded_dir,
    type = "directory",
    recurse = FALSE
)

# compile list of all Stata files in all directories
files_df <- purrr::map_dfr(
        .x = dirs,
        .f = ~ fs::dir_info(
            path = .x, 
            recurse = FALSE,
            type = "file",
            regexp = "\\.dta$"
        )
    ) %>%
    dplyr::mutate(file_name = fs::path_file(path))

# extract a list of all unique files found in the directories
file_names <- files_df %>%
    dplyr::distinct(file_name) %>%
    dplyr::pull(file_name)

# combine and save all same-named Stata files
purrr::walk(
    .x = file_names,
    .f = ~ combine_and_save(
        file_info_df = files_df,
        name = .x,
        dir = combined_dir
    )
)
