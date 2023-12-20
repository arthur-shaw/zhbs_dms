library(dplyr)

# =============================================================================
# Load data
# =============================================================================

# -----------------------------------------------------------------------------
# SuSo microdata and metadata
# -----------------------------------------------------------------------------

files <- c(
    main_file_name,
    "hhmembers.dta",
    "nfe_roster.dta",
    "Mod17_Crops.dta",
    "livestock.dta",
    "FAFH_roster.dta",
    "M22A_Roster_A.dta",
    "M22A_Roster_B.dta",
    "M22B_Roster_A.dta",
    "M22B_Roster_B.dta",
    "M24_Q07_gifts.dta",
    "M25_Product.dta",
    "M25b_Product.dta",
    "M26_Product.dta",
    "M27_Product.dta",
    "M28a_Product.dta",
    "M28b_Product.dta",
    "M29_Product.dta",
    "M30_Product.dta",
    "M31_Product.dta",
    "interview__diagnostics.dta",
    "interview__errors.dta",
    "interview__comments.dta"
)

names <- c(
    "households",
    "members",
    "enterprises",
    "crops",
    "livestock",
    "fafh",
    "food_purchase_1",
    "food_purchase_2",
    "food_conso_1",
    "food_conso_2",
    "nf_mod24",
    "nf_mod25",
    "nf_mod25b",
    "nf_mod26",
    "nf_mod27",
    "nf_mod28a",
    "nf_mod28b",
    "nf_mod29",
    "nf_mod30",
    "nf_mod31",
    "suso_diagnostics",
    "suso_errors",
    "suso_comments"
)

#' Load data from path to object
#' 
#' @param from Character. File path to data file
#' @param to Character. Name of object to contain data frame
#' 
#' @importFrom haven read_dta
load_data <- function(from, to) {

    df <- haven::read_dta(file = from)

    assign(
        x = to,
        value = df,
        envir = .GlobalEnv
    )

}

# load data
purrr::walk2(
    .x = files,
    .y = names,
    .f = ~ load_data(
        from = fs::path(combined_dir, .x),
        to = .y
    )
)

cases_to_review <- households %>%
    dplyr::mutate(interview_complete = M1_Q03 == 1) %>%
    dplyr::select(
        interview__id, interview__key,
        interview_complete, interview__status
    )

# -----------------------------------------------------------------------------
# Conversion tables
# -----------------------------------------------------------------------------

# currency conversion factors
# one row per week (or relevant time period)
currency_factors <- tibble::tribble(
    ~week,          ~zwd_to_zwd,    ~usd_to_zwd,    ~rand_to_zwd,   ~pula_to_zwd,
    "2023-09-11",   1,              361.9,          18.837785,      26.463538
) %>%
dplyr::mutate(week = as.Date(week))

# area conversion factors
area_factors <- tibble::tribble(
    ~unit,  ~to_hect,
    1,      3,          # acres
    2,      1,          # hectares
    3,      100,        # square meters
    4,      10,         # square meters
    5,      20         # ropes
)
