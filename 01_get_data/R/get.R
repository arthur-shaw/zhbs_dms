# determine the pattern to use
qnr_expr <- dplyr::if_else(
    condition = survey_type == "hbs",
    true = qnr_expr_hbs,
    false = qnr_expr_drb
)

# download all data matching that patterns
susoflows::download_matching(
    matches = qnr_expr,
    export_type = "STATA",
    path = downloaded_dir
)

# get the file name for the main file
# take `title` of first entry of matching questionnaires
main_file_name <- susoapi::get_questionnaires() %>%
    dplyr::filter(stringr::str_detect(title, qnr_expr_hbs)) |>
    dplyr::slice(1) %>%
    dplyr::pull(variable) %>%
    paste0(".dta")
