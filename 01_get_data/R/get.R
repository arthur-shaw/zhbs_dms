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
