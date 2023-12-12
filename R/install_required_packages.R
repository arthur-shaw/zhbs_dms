# =============================================================================
# Install necessary packages
# =============================================================================

# -----------------------------------------------------------------------------
# Install packages used for provisioning other packages
# -----------------------------------------------------------------------------

# for package installation
if (!require("pak")) {
    install.packages("pak")
}

# for iteration over list of packages
if (!require("purrr")) {
    install.packages("purrr")
}

# -----------------------------------------------------------------------------
# Install any missing requirements
# -----------------------------------------------------------------------------

# enumerate needed packages
required_packages <- c(
    "httr",
    "glue",
    "here",
    "haven",
    "purrr",
    "dplyr",
    "arthur-shaw/susoreview",
    "rlang",
    "stringr",
    "lubridate",
    "tidyr",
    "arthur-shaw/susoapi",
    "writexl"
)

#' Install package if missing on system
#' 
#' @param package Character. Name of package to install.
install_if_missing <- function(package) {

    # strip out package name from repo address
    slash_pattern <- "\\/"
    if (stringr::str_detect(string = package, pattern = slash_pattern) ) {
        slash_position <- stringr::str_locate(
            string = package,
            pattern = slash_pattern
        )
        package <- stringr::str_sub(
            string = package,
            start = slash_position[[1]] + 1
        )
    }

    if (!require(package, character.only = TRUE)) {
        pak::pak(package)
    }

}

# install any missing requirements
purrr::walk(
    .x = required_packages,
    .f = ~ install_if_missing(.x)
)
