# =============================================================================
# Confirm that inputs provided
# =============================================================================

#' Check whether the object exists
#' 
#' @param object Character. Name of the object whose existence to check.
#' 
#' @importFrom glue glue
object_exists <- function(object) {
    if(!exists(object)) {
        stop(glue::glue("No object named `{object}` exists. Please provide it above."))
    } 
}

# -----------------------------------------------------------------------------
# Project directory
# -----------------------------------------------------------------------------

object_exists("proj_dir")

dir.exists(proj_dir)

# -----------------------------------------------------------------------------
# Program behavior parameters
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Statuses to reject
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# exists
object_exists("statuses_to_reject")

# contains only expected values
allowed_statuses <- c(100, 120, 130)
if (any(!statuses_to_reject %in% allowed_statuses)) {
    stop(
        glue::glue(
            "Unexpected values found in `statuses_to_reject`.",
            "Expected: {allowed_statuses}",
            "Found: {statuses_to_reject}",
            .sep = "\n"
        )
    )
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Statuses to reject
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# exists
object_exists("should_reject")

# contains expected values
if (!is.logical(should_reject)) {
    stop(
        glue::glue(
            "Unexpected values found in `should_reject`.",
            "Expected: TRUE/FALSE",
            "Found: {should_reject}",
            .sep = "\n"
        )
    )
}

# -----------------------------------------------------------------------------
# Survey Solutions details
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Specified
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

suso_details <- c(
    "server",
    "workspace",
    "user",
    "password"
)

purrr::walk(
    .x = suso_details,
    .f = ~ object_exists(.x)
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Valid
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

susoapi::check_credentials(
    workspace = workspace,
    verbose = TRUE
)
