# =============================================================================
# Create attributes
# =============================================================================
 
# =============================================================================
# Define functions
# =============================================================================

library(dplyr)

#' Count variables in nested rosters
#' 
#' @param df Data frame. Household-level microdata that contains the variables to count.
#' @param var_pattern Character Regular expression used to select variables to be counted.
#' @param var_val Numeric. Value(s) to count in columns identified by `var_pattern`.
#' @param attrib_name Character. Name of attribute.
#' @param attrib_vars Character. Regular expression that identifies the variable(s) in `var_pattern`.
#' 
#' @importFrom susoreview count_vars
#' @importFrom dplyr `%>%` group_by summarise
count_vars_nested <- function(
    df,
    var_pattern,
    var_val = 1,
    attrib_name,
    attrib_vars = var_pattern
) {

    df_out <- susoreview::count_vars(
            df = df,
            var_pattern = var_pattern,
            var_val = var_val,
            attrib_name = attrib_name,
            attrib_vars = attrib_vars
        ) %>%
        dplyr::group_by(
            interview__id, interview__key,
            attrib_name, attrib_vars
        ) |>
        dplyr::summarise(attrib_val = sum(attrib_val, na.rm = TRUE)) %>%
        dplyr::ungroup()

}

#' Check whether any variables in nested roster have specified value
#' 
#' @param df Data frame. Household-level microdata that contains the variables to count.
#' @param var_pattern Character Regular expression used to select variables to be counted.
#' @param var_val Numeric. Value(s) to count in columns identified by `var_pattern`.
#' @param attrib_name Character. Name of attribute.
#' @param attrib_vars Character. Regular expression that identifies the variable(s) in `var_pattern`.
#' 
#' @importFrom susoreview count_vars
#' @importFrom dplyr `%>%` group_by summarise
any_vars_nested <- function(
    df,
    var_pattern,
    var_val = 1,
    attrib_name,
    attrib_vars = var_pattern
) {

    df_out <- susoreview::any_vars(
        df = df,
        var_pattern = var_pattern,
        var_val = var_val,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
    ) %>%
    dplyr::group_by(
        interview__id, interview__key,
        attrib_name, attrib_vars
    ) |>
    dplyr::summarise(attrib_val = any(attrib_val = TRUE, na.rm = TRUE)) %>%
    dplyr::ungroup()

}

#' Convert monetary values to ZWD
#' 
#' @param df Data frame that contains `*_amt`, `*_curr`, and `*_to_zwd` columns
#' @param amt_var Character. Name of amount column
#' 
#' @importFrom rlang ensym enquo as_name data_sym
#' @importFrom stringr str_replace
#' @importFrom dplyr `%>%` mutate case_when
conv_to_zwd <- function(
    df,
    amt_var
) {
  
  # construct currency variable name
  amt_var <- rlang::ensym(amt_var)
  
  curr_var <- rlang::enquo(amt_var) |> 
    rlang::as_name() |>
    stringr::str_replace(
      pattern = "_amt",
      replacement = "_curr"
    ) |>
    rlang::data_sym()
  
  df <- df %>%
    dplyr::mutate(
      !!amt_var := dplyr::case_when(
        # zwd to zwd
        !!curr_var == 1 ~ !!amt_var * zwd_to_zwd,
        # zwd to usd
        !!curr_var == 2 ~ !!amt_var * usd_to_zwd,
        # rand to zwd
        !!curr_var == 3 ~ !!amt_var * rand_to_zwd,
        # pula to zwd
        !!curr_var == 4 ~ !!amt_var * pula_to_zwd,
      )
    )
  
  return(df)
  
}

interview_week <- households %>%
    dplyr::mutate(
        date = stringr::str_extract(
            string = M2_START_DATE,
            pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}"
        ),
        week = lubridate::floor_date(
            x = as.Date(date), 
            unit = "week", 
            week_start = 1
        )
    ) %>%
    dplyr::select(interview__id, week)

# -----------------------------------------------------------------------------
# Household
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. International migration
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

attrib_n_migrated <- susoreview::count_list(
    df = households,
    var_pattern = "M3_Q01",
    attrib_name = "n_migrated"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Housing
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# get water from network
attrib_water_from_network <- susoreview::create_attribute(
    df = households,
    condition = (M4_Q16 %in% c(1, 2)) | (M4_Q19 %in% c(1, 2)),
    attrib_name = "water_from_network",
    attrib_vars = "M4_Q1[69]"
)

attrib_use_electricity <- susoreview::create_attribute(
    df = households,
    condition = dplyr::if_any(
        .cols = dplyr::matches("M4_Q08__[1-8]{1}"),
        .fns = ~ .x == 1
    ),
    attrib_name = "use_electricity",
    attrib_vars = "M4_Q08__[1-8]"
)

attrib_toilet_on_sewer <- susoreview::create_attribute(
    df = households,
    condition = M4_30 == 1,
    attrib_name = "toilet_on_sewer",
    attrib_vars = "M4_Q30"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 8. Durables
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number assets owned
attrib_any_asset_owned <- susoreview::any_vars(
    df = households,
    var_pattern = "M8_Q01",
    attrib_name = "any_asset_owned"
)

# number assets requiring electricity
elect_assets <- c(
    5, # TV
    6, # DVD player
    7, # home theater
    9, # radio
    10, # computer
    12:22, 24, # small electric appliances
    25:27, # electric climate conditioner
    28:30 # appliances
)

# owns assets that use electricity
attrib_owns_electric_assets <- susoreview::any_vars(
    df = households,
    var_pattern = "M8_Q01__([5-6]$|9$|10$|1[2-9]$|2[12]$|2[4-9]$|30$)",
    attrib_name = "owns_electric_assets",
    attrib_vars = "M8_Q01"
)

# owns means of transport
attrib_owns_transport <- susoreview::create_attribute(
    df = households,
    condition = dplyr::if_any(
        .cols = dplyr::matches("M8_Q01__[1-4]$"),
        .fns = ~ .x == 1
    ),
    attrib_name = "owns_vehicle",
    attrib_vars = "M8_Q01"
)

# owns cell phone
attrib_owns_cell <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__12 == 1,
    attrib_name = "owns_cell",
    attrib_vars = "M8_Q01"
)

# owns DVD player
attrib_owns_dvd_player <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__6 == 1,
    attrib_name = "owns_dvd_player",
    attrib_vars = "M8_Q01"
)

# owns fixed line phone
attrib_owns_fixed_line <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__11 == 1,
    attrib_name = "owns_fixed_line",
    attrib_vars = "M8_Q01"
)

# owns computer
attrib_owns_computer <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__10 == 1,
    attrib_name = "owns_computer",
    attrib_vars = "M8_Q01"
)

# owns TV
attrib_owns_tv <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__5 == 1,
    attrib_name = "owns_tv",
    attrib_vars = "M8_Q01"
)

# owns motor vehicle
attrib_owns_motor_vehicle <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__1 == 1,
    attrib_name = "owns_motor_vehicle",
    attrib_vars = "M8_Q01"
)

# owns motorcycle
attrib_owns_motorcycle <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__2 == 1,
    attrib_name = "owns_motorcycle",
    attrib_vars = "M8_Q01"
)

# owns bicycle
attrib_owns_bicycle <- susoreview::create_attribute(
    df = households,
    condition = M8_Q01__4 == 1,
    attrib_name = "owns_bicycle",
    attrib_vars = "M8_Q01"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 14. Non-farm enterprise filter
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-farm enterprises
attrib_n_enterprises <- susoreview::count_list(
    df = households,
    var_pattern = "M15_Q02a__[0-9]+$",
    attrib_name = "n_enterprises" 
)

# -----------------------------------------------------------------------------
# 16. Land area
# -----------------------------------------------------------------------------

attrib_land_area <- susoreview::extract_attribute(
    df = households,
    var = M16_Q1a,
    attrib_name = "land_area",
    attrib_vars = "M16_Q1a"
)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 21. Food away from home
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# any FAFH
attrib_any_fafh <- susoreview::any_vars(
    df = households,
    var_pattern = "M21_Q01",
    attrib_name = "any_fafh"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 22/23. Food items purchased
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 22b/23b. Food items consumed
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of food items
attrib_n_food_consumed <- households %>%
    # remove screener questions
    dplyr::select(-dplyr::matches("M22B_Q01_EXPENDITURE")) %>%
    # count columns of food items
    susoreview::count_vars(
        var_pattern = "M22B_Q01",
        var_val = 1,
        attrib_name = "n_food_consumed"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 24. Commodity and service expenditure
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m24 <- count_vars_nested(
    df = nf_mod24,
    var_pattern = "M24_Q02_ItemList",
    attrib_name = "n_nf_m24" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 25. Clothing expenditure
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m25 <- count_vars_nested(
    df = nf_mod25,
    var_pattern = "M25_Q02_ItemList",
    attrib_name = "n_nf_m25"
)

# male clothing expenditure
attrib_male_clothing_exp <- count_vars_nested(
    df = nf_mod25,
    var_pattern = "M25_Q02_ItemList__([6-9]$|1[0-9]$|7[0-3]$|11[4-9]$|120$)",
    attrib_name = "male_clothing_exp",
    attrib_vars = "M25_Q02_ItemList"
)

# female clothing expenditure
attrib_female_clothing_exp <- count_vars_nested(
    df = nf_mod25,
    var_pattern = "M25_Q02_ItemList__(2[0-9]$|3[0-8]$|7[4-7]$|12[1-7])",
    attrib_name = "female_clothing_exp",
    attrib_vars = "M25_Q02_ItemList"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 25b. Housing costs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m25b <- count_vars_nested(
    df = nf_mod25b,
    var_pattern = "M25b_Q02_ItemList",
    attrib_name = "n_nf_m25b" 
)

# water expenditure
attrib_water_exp <- count_vars_nested(
    df = nf_mod25b,
    var_pattern = "M25b_Q02_ItemList__2[01]",
    attrib_name = "water_exp",
    attrib_vars = "M25b_Q02_ItemList",
)

# electricity expenditure
attrib_electricity_exp <- count_vars_nested(
    df = nf_mod25b,
    var_pattern = "M25b_Q02_ItemList__(2[5-9]$|3[01])",
    attrib_name = "electricity_exp", 
    attrib_vars = "M25b_Q02_ItemList",
)

# pay for sewage
attrib_sewage_exp <- count_vars_nested(
    df = nf_mod25b,
    var_pattern = "M25b_Q02_ItemList__24",
    attrib_name = "sewage_exp", 
    attrib_vars = "M25b_Q02_ItemList",
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 26. Furnishings and equipment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m26 <- count_vars_nested(
    df = nf_mod26,
    var_pattern = "M26_Q02_ItemList",
    attrib_name = "n_nf_m26" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 27. Health goods and services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m27 <- count_vars_nested(
    df = nf_mod27,
    var_pattern = "M27_Q02_ItemList",
    attrib_name = "n_nf_m27" 
)

# any expenditure on health services
attrib_exp_any_health_services <- any_vars_nested(
    df = nf_mod27,
    var_pattern = "M27_Q02_ItemList__(5[89]$|6[1-8]$|7[5-9]$|8[0-9]$|90$)",
    attrib_name = "exp_any_health_services",
    attrib_vars = "M27_Q02_ItemList"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 28a. Leisure
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m28a <- count_vars_nested(
    df = nf_mod28a,
    var_pattern = "M28a_Q02_ItemList",
    attrib_name = "n_nf_m28a" 
)

# purchased DVD
attrib_n_nf_m28a <- count_vars_nested(
    df = nf_mod28a,
    var_pattern = "M28a_Q02_ItemList__14[12]",
    attrib_name = "bought_dvd",
    attrib_vars = "M28a_Q02_ItemList"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 28b. Communication equipement and services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m28b <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList",
    attrib_name = "n_nf_m28b" 
)

# has cell phone expenditures
attrib_has_cell_exp <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__(24[89]$|25[0-2]$)",
    attrib_name = "has_cell_exp",
    attrib_vars = "M28b_Q02_ItemList" 
)

# bought fixed line phone
attrib_bought_fixed_line <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__(18[56]|191)",
    attrib_name = "bought_fixed_line",
    attrib_vars = "M28b_Q02_ItemList" 
)

# bought cell phone
attrib_bought_cell <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__19[245]",
    attrib_name = "bought_cell",
    attrib_vars = "M28b_Q02_ItemList" 
)

# bought computer
attrib_bought_pc <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__19[67]",
    attrib_name = "bought_computer",
    attrib_vars = "M28b_Q02_ItemList" 
)

# bought TV
attrib_bought_tv <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__213",
    attrib_name = "bought_tv",
    attrib_vars = "M28b_Q02_ItemList" 
)

# bought DVD player
attrib_bought_dvd_player <- count_vars_nested(
    df = nf_mod28b,
    var_pattern = "M28b_Q02_ItemList__214",
    attrib_name = "bought_dvd_player",
    attrib_vars = "M28b_Q02_ItemList" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 29. Transport goods and services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m29 <- count_vars_nested(
    df = nf_mod29,
    var_pattern = "M29_Q02_ItemList",
    attrib_name = "n_nf_m29" 
)

# bought motor vehicle
attrib_bought_motor_vehicle <- count_vars_nested(
    df = nf_mod29,
    var_pattern = "M29_Q02_ItemList__[1-4]$",
    attrib_name = "bought_motor_vehicle",
    attrib_vars = "M29_Q02_ItemList"
)

# bought motorcycle
attrib_bought_motorcycle <- count_vars_nested(
    df = nf_mod29,
    var_pattern = "M29_Q02_ItemList__([57]$|10)",
    attrib_name = "bought_motorcycle",
    attrib_vars = "M29_Q02_ItemList"
)

# bought bicycle
attrib_bought_bicycle <- count_vars_nested(
    df = nf_mod29,
    var_pattern = "M29_Q02_ItemList__11",
    attrib_name = "bought_bicycle",
    attrib_vars = "M29_Q02_ItemList"
)

# bought tires
attrib_bought_tires <- count_vars_nested(
    df = nf_mod29,
    var_pattern = "M29_Q02_ItemList__(1[789]|20)",
    attrib_name = "bought_tires",
    attrib_vars = "M29_Q02_ItemList"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 30. Transport of passengers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m30 <- count_vars_nested(
    df = nf_mod30,
    var_pattern = "M30_Q02_ItemList",
    attrib_name = "n_nf_m30" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 31. Other goods and services
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of non-food items
attrib_n_nf_m31 <- count_vars_nested(
    df = nf_mod31,
    var_pattern = "M31_Q02_ItemList",
    attrib_name = "n_nf_m31" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 35. Dietary diversity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# cereals consumed
attrib_consumed_cereals_1d <- susoreview::create_attribute(
    df = households,
    condition = M35_Q04 == 1,
    attrib_name = "consumed_cereals_1d",
    attrib_vars = "M35_Q04"
)

# tubers consumed
attrib_consumed_tubers_1d <- susoreview::create_attribute(
    df = households,
    condition = M35_Q05 == 1,
    attrib_name = "consumed_tubers_1d",
    attrib_vars = "M35_Q05"
)

# legumes consumed
attrib_consumed_legumes_1d <- susoreview::create_attribute(
    df = households,
    condition = M35_Q06 == 1,
    attrib_name = "consumed_legumes_1d",
    attrib_vars = "M35_Q06"
)

# vegetables consumed
attrib_consumed_veggies_1d <- susoreview::create_attribute(
    df = households,
    condition = M35_Q07 == 1,
    attrib_name = "consumed_veggies_1d",
    attrib_vars = "M35_Q07"
)

# fruit consumed
attrib_consumed_fruits_1d <- susoreview::create_attribute(
    df = households,
    condition = M35_Q08 == 1,
    attrib_name = "consumed_fruits_1d",
    attrib_vars = "M35_Q08"
)

# -----------------------------------------------------------------------------
# Member
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Household composition
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of heads
attrib_n_heads <- susoreview::count_obs(
    df = members,
    where = M2_Q06 == 1,
    attrib_name = "n_heads",
    attrib_vars = "M2_Q06"
)

# number of male members
attrib_n_male <- susoreview::count_obs(
    df = members,
    where = M2_Q07 == 1,
    attrib_name = "n_male",
    attrib_vars = "M2_Q07" 
)

# number of female members
attrib_n_female <- susoreview::count_obs(
    df = members,
    where = M2_Q07 == 2,
    attrib_name = "n_female",
    attrib_vars = "M2_Q07" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 9. Education
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# any member not attending
attrib_not_attend_bc_biz <- susoreview::any_obs(
    df = members,
    where = M9_Q06__4 == 1,
    attrib_name = "not_attend_bc_biz",
    attrib_vars = "M9_Q06"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 10. Employment
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# any member involved in household business
attrib_works_home_business <- susoreview::any_obs(
    df = members,
    where = (
        # in the past 7 days, ...
        # run/do a business
        (M10_Q02 == 1 | M10_Q07 == 1) |
        # help in hhold business
        (M10_Q03 == 1 | M10_Q10 == 1)
    ),
    attrib_name = "works_home_business",
    attrib_vars = "M10_Q0[238]|M10_Q11" 
)

# any member retired
attrib_retired_w_pension <- susoreview::any_obs(
    df = members,
    where = M10_Q14 == 12,
    attrib_name = "retired_w_pension",
    attrib_vars = "M10_Q14" 
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 11. Health
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# paid for health services
attrib_paid_health <- members %>%
    dplyr::mutate(
        paid_health = rowSums(
            dplyr::pick(M11_Q11__1, M11_Q11__3, M11_Q11__4, M11_Q11__7)
        )
    ) %>%
    susoreview::any_obs(
        where = paid_health == 1,
        attrib_name = "paid_for_health_sevices",
        attrib_vars = "M11_Q11"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 19. Non-labor income
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# received retirement pension
attrib_received_pension <- susoreview::create_attribute(
    df = households,
    condition = M19_Q01__12 == 1,
    attrib_name = "received_pension",
    attrib_vars = "M19_Q01"
)

# received money from relations outside Zimbabwe
attrib_received_intl_remittance <- susoreview::create_attribute(
    df = households,
    condition = M19_Q01__17 == 1,
    attrib_name = "received_itl_remittance",
    attrib_vars = "M19_Q01"
)

# received income from renting transport
attrib_received_transport_rental_income <- susoreview::create_attribute(
    df = households,
    condition = M19_Q01__3 == 1,
    attrib_name = "received_vehicle_rental_income",
    attrib_vars = "M19_Q01"
)

# -----------------------------------------------------------------------------
# Food purchase
# -----------------------------------------------------------------------------

perishables <- c(
    64:75, # fresh meat
    84:94, # fresh offal
    96:98, # fresh processed meat
    103:104, # fresh fish
    130:135, # fresh milk
    158:162, # eggs (?)
    177:204, # fresh fruit
    219:245, # fresh vegetables
    326:332 # juices
)

# any perishable items purchased
attrib_any_perishable_purchased <- susoreview::any_vars(
        df = households,
        var_pattern = paste(
            "M22A_Q02_Meat_fresh",
            "M22A_Q02_Offal_blood__(8[4-9]|9[1-4])",
            "M22A_Q02_meat_prep__9[6-8]",
            "M22A_Q02_Fish_fresh__10[34]",
            "M22A_Q02_milk_raw|M22A_Q02_MilkSkimm",
            "M22A_Q02_Eggs",
            "M22A_Q02_Tropi_fruit|M22A_Q02_CitrusFrt|M22A_Q02_StoneFrut|M22A_Q02_BerriyFresh|M22A_Q02_Oth_fruit_f",
            sep = "|"
        ),
        attrib_name = "any_perishable_purchased",
        attrib_vars = "M22A_Q02" 
    )

# -----------------------------------------------------------------------------
# Food consumption
# -----------------------------------------------------------------------------

# any perishable items consumed from purchase
attrib_any_perishable_purch_consumed <- households %>%
    susoreview::any_vars(
        var_pattern = paste(
            # meat
            "M22B_Q01_Meat_fresh",
            "M22B_Q01_Offal_blood__(8[4-9]|9[1-4])",
            "M22B_Q01_meat_prep__9[6-8]",
            # fish
            "M22B_Q01_Fish_fresh__10[34]",
            # milk and eggs
            "M22B_Q01_milk_raw|M22B_Q02_MilkSkimm",
            "M22B_Q01_Eggs",
            # fruit
            "M22B_Q01_Tropi_fruit|M22B_Q02_CitrusFrt|M22B_Q02_StoneFrut|M22B_Q02_BerriyFresh|M22B_Q02_Oth_fruit_f",
            sep = "|"
        ),
        attrib_name = "any_perishable_purch_consumed"
    )

# cereals consumed in past 7 days
cereals <- "M22B_Q01_(cereals|flours|bakery|BF_cereals|pasta|OthCreals)"

attrib_cereals_consumed_7d <- susoreview::any_obs(
    df = households,
    where = dplyr::if_any(
        .cols = dplyr::matches(cereals),
        .fns = ~ .x == 1
    ),
    attrib_name = "cereals_consumed_7d",
    attrib_vars = cereals
)

# tubers consumedin past 7 days
tubers <- "M22B_Q01_potatoes"

attrib_tubers_consumed_7d <- susoreview::any_obs(
    df = households,
    where = dplyr::if_any(
        .cols = dplyr::matches(tubers),
        .fns = ~ .x == 1
    ),
    attrib_name = "tubers_consumed_7d",
    attrib_vars = tubers
)

# legumes consumed in past 7 days
legumes <- "M22B_Q01_Green_leg|M22B_Q01_Pulses|M22B_Q01_Nuts"

attrib_legumes_consumed_7d <- susoreview::any_obs(
    df = households,
    where = dplyr::if_any(
        .cols = dplyr::matches(legumes),
        .fns = ~ .x == 1
    ),
    attrib_name = "legumes_consumed_7d",
    attrib_vars = legumes
)

# vegetables consumed in past 7 days
veggies <- paste0(
    "M22B_Q01_Leafy_veg|M22B_Q01_Fruit_veg|M22B_Q01_Ot_veg_tube|",
    "M22B_Q01_OtVegiDried|M22B_Q01_vegi_froze|M22B_Q01_Vegi_prep"
)

attrib_veggies_consumed_7d <- susoreview::any_obs(
    df = households,
    where = dplyr::if_any(
        .cols = dplyr::matches(veggies),
        .fns = ~ .x == 1
    ),
    attrib_name = "veggies_consumed_7d",
    attrib_vars = veggies
)

# fruits consumed in past 7 days
fruits <- paste0(
    "M22B_Q01_Tropi_fruit|M22B_Q01_CitrusFrt|M22B_Q01_StoneFrut|",
    "M22B_Q01_BerriyFresh|M22B_Q01_Oth_fruit_f|M22B_Q01_FrozFruit|",
    "M22B_Q01_Fruit_dried"
)

attrib_fruits_consumed_7d <- susoreview::any_obs(
    df = households,
    where = dplyr::if_any(
        .cols = dplyr::matches(fruits),
        .fns = ~ .x == 1
    ),
    attrib_name = "fruits_consumed_7d",
    attrib_vars = fruits
)

# -----------------------------------------------------------------------------
# Enterprise
# -----------------------------------------------------------------------------

# enterprise with conversion factors
enterprises_w_factors <- enterprises %>%
    # join interview week
    dplyr::left_join(interview_week, by = "interview__id") %>%
    # join conversion factors
    dplyr::left_join(currency_factors, by = "week") %>%
    # fill in missing weeks arbitrarily with value of neighbor in df
    tidyr::fill(week, .direction = "downup")

# profits
attrib_ent_profit <- enterprises_w_factors %>%
    # transform to zwd values
    conv_to_zwd(amt_var = "M15_Q08_curr") %>%
    conv_to_zwd(amt_var = "M15_Q09_curr") %>%
    conv_to_zwd(amt_var = "M15_Q10_curr") %>%
    dplyr::mutate(
        profit = rowSums(
            dplyr::pick(M15_Q08_curr, M15_Q09_curr, M15_Q10_curr),
            na.rm = TRUE
        )
    ) %>%
    susoreview::extract_attribute(
        var = profit,
        attrib_name = "ent_profit",
        attrib_vars = "M15_Q(0[80]|10)_curr"
    )

# total costs
attrib_ent_cost <- enterprises_w_factors %>%
    # transform to zwd values
    # raw materials
    conv_to_zwd(amt_var = "M15_Q11_a_amt") %>%
    # good for resale
    conv_to_zwd(amt_var = "M15_Q11_b_amt") %>%
    # utilities
    conv_to_zwd(amt_var = "M15_Q11_c_amt") %>%
    # transport
    conv_to_zwd(amt_var = "M15_Q11_d_amt") %>%
    # fuel
    conv_to_zwd(amt_var = "M15_Q11_e_amt") %>%
    # packaging
    conv_to_zwd(amt_var = "M15_Q11_f_amt") %>%
    # rental of equipment
    conv_to_zwd(amt_var = "M15_Q11_g_amt") %>%
    # maintenance of equipment
    conv_to_zwd(amt_var = "M15_Q11_h_amt") %>%
    # rental of facilities
    conv_to_zwd(amt_var = "M15_Q11_j_amt") %>%
    # maintenance of facilities
    conv_to_zwd(amt_var = "M15_Q11_k_amt") %>%
    # interest
    conv_to_zwd(amt_var = "M15_Q11_l_amt") %>%
    # payroll
    conv_to_zwd(amt_var = "M15_Q11_m_amt") %>%
    # insurance
    conv_to_zwd(amt_var = "M15_Q11_n_amt") %>%
    # other
    conv_to_zwd(amt_var = "M15_Q11_o_amt") %>%
    # tax
    conv_to_zwd(amt_var = "M15_Q11_p_amt") %>%
    # compute costs
    dplyr::mutate(
        cost = rowSums(
            x = dplyr::pick(dplyr::matches("M15_Q11_[a-p]_amt")),
            na.rm = TRUE
        )
    ) %>%
    susoreview::extract_attribute(
        var = cost,
        attrib_name = "ent_cost",
        attrib_vars = "cost" 
    )

# -----------------------------------------------------------------------------
# Livestock
# -----------------------------------------------------------------------------

# N/A: cannot compare sales and cost, since some value may have been realized through consumption

# -----------------------------------------------------------------------------
# Crop roster
# -----------------------------------------------------------------------------

# area planted
attrib_cropped_area <- crops %>%
    # convert area to hectares
    # join conversion factors
    dplyr::left_join(area_factors, by = c("M17_Q02_UNIT" = "unit")) %>%
    # convert
    dplyr::mutate(M17_Q02 = M17_Q02 * to_hect) %>%
    # extract attributes
    susoreview::extract_attribute(
        var = M17_Q02,
        attrib_name = "cropped_area",
        attrib_var = "M17_Q02"
    )

# =============================================================================
# Combine attributes
# =============================================================================

# combine all attribute data sets whose names match the pattern below
attribs <- dplyr::bind_rows(mget(ls(pattern = "^attrib_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^attrib_"))
