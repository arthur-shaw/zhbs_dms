# =============================================================================
# Define paths as a function of survey type
# =============================================================================

data_dir <- fs::path(here::here(), "01_get_data")
downloaded_dir <- fs::path(data_dir, "data", survey_type, "01_downloaded")
combined_dir <- fs::path(data_dir, "data", survey_type, "02_combined")
derived_dir <- fs::path(data_dir, "data", survey_type, "03_derived")

# =============================================================================
# Purge stale data
# =============================================================================

# -----------------------------------------------------------------------------
# Downloaded data
# -----------------------------------------------------------------------------

# remove zip files
zips_to_delete <- fs::dir_ls(
    path = downloaded_dir,
    recurse = FALSE,
    type = "file",
    regexp = "\\.zip$"
)
fs::file_delete(zips_to_delete)

# remove unzipped folders and the data they contain
dirs_to_delete <- fs::dir_ls(
    path = downloaded_dir,
    recurse = FALSE,
    type = "directory"
)
fs::dir_delete(dirs_to_delete)

# -----------------------------------------------------------------------------
# Combined data
# -----------------------------------------------------------------------------

# remove zip files
dta_to_delete <- fs::dir_ls(
    path = combined_dir,
    recurse = FALSE,
    type = "file",
    regexp = "\\.dta$"
)
fs::file_delete(dta_to_delete)

# =============================================================================
# Purge output from past sessions
# =============================================================================

if (survey_type == "hbs") {

    output_dir <- fs::path(here::here(), "03_check_issues", "output")

    files_to_delete <- fs::dir_ls(
        path = output_dir,
        type = "file",
        regexp = "(\\.xlsx|\\.dta)"
    )

    fs::file_delete(files_to_delete)

}
