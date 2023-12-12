# =============================================================================
# Write decisions to disk in Excel and Stata format
# =============================================================================

# -----------------------------------------------------------------------------
# Define function
# -----------------------------------------------------------------------------

#' Write Excel and Stata files with data
#' 
#' @param data Data frame. Data to write.
#' @param dir Character. File path where file should be written.
#' @param name Character. File name. By default, the name of the data frame.
#' 
#' @importFrom writexl write_xlsx
#' @importFrom haven write_dta
#' @importFrom fs path
write_to_excel_and_stata <- function(
    data,
    dir,
    name = deparse(substitute(data))
) {

    # Excel
    writexl::write_xlsx(
        x = data,
        path = fs::path(dir, paste0(name, ".xlsx")),
        col_names = TRUE
    )

    # Stata
    haven::write_dta(
        data = data,
        path = fs::path(dir, paste0(name, ".dta"))
    )

}

# -----------------------------------------------------------------------------
# Attributes
# -----------------------------------------------------------------------------

# data
write_to_excel_and_stata(
    data = attribs, 
    dir = output_dir, 
    name = "attributes"
)

# -----------------------------------------------------------------------------
# Issues
# -----------------------------------------------------------------------------

# data
write_to_excel_and_stata(
    data = issues_plus_miss_and_suso, 
    dir = output_dir, 
    name = "issues"
)

# -----------------------------------------------------------------------------
# To reject
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_reject_ids, to_reject_issues, to_reject_api),
    .y = c("to_reject_ids", "to_reject_issues", "to_reject_api"),
    .f = ~ write_to_excel_and_stata(
        data = .x, 
        dir = output_dir, 
        name = .y
    )
)

# -----------------------------------------------------------------------------
# To review
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_review_ids, to_review_issues, to_review_api),
    .y = c("to_review_ids", "to_review_issues", "to_review_api"),
    .f = ~ write_to_excel_and_stata(
        data = .x, 
        dir = output_dir, 
        name = .y
    )
)

# -----------------------------------------------------------------------------
# To follow up
# -----------------------------------------------------------------------------

purrr::walk2(
    .x = list(to_follow_up_ids, to_follow_up_issues, to_follow_up_api),
    .y = c("to_follow_up_ids", "to_follow_up_issues", "to_follow_up_api"),
    .f = ~ write_to_excel_and_stata(
        data = .x,
        dir = output_dir,
        name = .y
    )
)
