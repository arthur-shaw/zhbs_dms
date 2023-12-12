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
