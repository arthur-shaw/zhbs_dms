# =============================================================================
# Flag errors
# =============================================================================

# -----------------------------------------------------------------------------
# Household heads
# -----------------------------------------------------------------------------

# no head
issue_no_head <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "n_heads",
    where = n_heads == 0,
    type = 1,
    desc = "No head",
    comment = paste0(
        "ERROR: No household member designated as head. ",
        "Please identify the member who is the household head."
    )
)

# more than 1 head
issue_too_many_heads <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "n_heads",
    where = n_heads > 1,
    type = 1,
    desc = "More than 1 head",
    comment = paste0(
        "ERROR: More than one person designated as head. ",
        "Please identify the member who is the household head."
    )
)

# -----------------------------------------------------------------------------
# Food consumption
# -----------------------------------------------------------------------------

# no food consumption inside household
issue_no_home_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "n_food_consumed",
    where = n_food_consumed == 0,
    type = 1,
    desc = "No home food consumption",
    comment = paste0(
        "ERROR: No home food consumption reported. ",
        "The household did not consume any household at home. ",
        "This is highly unlikely. ",
        "Please confirm all the food consumption questions again."
    )
)

# no food consumed--neither inside nor outside the household
issue_no_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("any_fafh", "n_food_consumed"),
    where = any_fafh == 0 & n_food_consumed == 0,
    type = 1,
    desc = "No food consumption",
    comment = paste0(
        "ERROR: No food consumption reported. ",
        "The household did not consume any food--neither inside nor outside the household. ",
        "This is not possible. Please ask the food consumption questions again."
    )
)

# -----------------------------------------------------------------------------
# Total food consumption == sum(all sources of food consumption)
# -----------------------------------------------------------------------------

# TODO: do for each food item, potentially in different rosters, 
# indicating the variable to which comment should be attached

# -----------------------------------------------------------------------------
# Non-food consumption
# -----------------------------------------------------------------------------

issue_no_non_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c(
        "nf_mod24", "nf_mod25", "nf_mod25b", "nf_mod26", "nf_mod27",
        "nf_mod28a", "nf_mod28b", "nf_mod29", "nf_mod30", "nf_mod31"
    ),
    where = (
        # sum of counts...
        rowSums(
            x = dplyr::pick(
                nf_mod24, nf_mod25, nf_mod25b, nf_mod26, nf_mod27,
                nf_mod28a, nf_mod28b, nf_mod29, nf_mod30, nf_mod31
            ),
            na.rm = TRUE
        )
        # ... is either NA or 0 
        %in% c(NA, 0)
    ),
    type = 1,
    desc = "No non-food consumption",
    comment = paste0(
        "ERROR: No non-food consumption reported. ",
        "The household did not consume any non-food item whatsoever. ",
        "This is impossible. Please ask the non-food consumption questions ",
        "again in modules 24 to 31."
    )

)

# -----------------------------------------------------------------------------
# Income
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Assets
# -----------------------------------------------------------------------------

# no assets owned
issue_no_assets <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "any_asset_owned",
    where = any_asset_owned == 0,
    type = 1,
    desc = "No assets",
    comment = paste0(
        "ERROR: No assets reported. ",
        "No assets reported in section 14. ",
        "This is very unlikely. Please ask the household again."
    )
)

# =============================================================================
# Flag critical inconsistencies
# =============================================================================


# -----------------------------------------------------------------------------
# Not attending school because of hhold biz responsibilities (educ, q6 == 4), 
# but no household business reported (nfe, E1 != 1) 
# -----------------------------------------------------------------------------

issue_no_school_bc_biz <- susoreview::create_issue(
    df = attribs,
    vars = c("not_attend_bc_biz", "n_enterprises"),
    where = not_attend_bc_biz == 1 & n_enterprises == 0,
    type = 1,
    desc = paste0(
        "Member not attending school because of hhold business, ",
        "but no business reported."
    ),
    comment = paste0(
        "ERROR: not at school b/c of hhold business, but no business reported",
        "In the education module, at least one member is not attending schoold ",
        "because of household business obligations.",
        "But in the household business module, no business is reported. ",
        "These cannot be true at the same time. Please check information in ",
        "in both modules."
    )
)

# -----------------------------------------------------------------------------
# Not seeking work because retired with pension (labor, M10_Q14==12), 
# but no pension income reported 
# -----------------------------------------------------------------------------

issue_retired_but_no_pension <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("received_pension", "retired_w_pension"),
    where = retired_w_pension == 1 & received_pension == 0, 
    type = 1,
    desc = paste0(
        "Not seeking work because retired with pension, ",
        "but no retirement pension reported"
    ),
    comment = paste0(
        "ERROR: At least one member not seeking employment because ",
        "retired with a pension, but no pension income reported. ",
        "In the employment module, at least one member reported being retired ",
        "and receiving a pension income. ",
        "But in the non-labor income module, no retirement pension income ",
        "is reported. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# Not seeking work due to disability (labor, M10_Q14==15), 
# but no disability reported (functioning, q1>1 | q2>1 | q3>1 | q4>1 | q5>1 | q6>1 )
# -----------------------------------------------------------------------------

issue_not_seek_bc_disability <- members %>%
    dplyr::mutate(
        stop_work_bc_disability = M10_Q14 == 15,
        has_disability = dplyr::if_any(
            .cols = dplyr::matches("M12_Q0[1-6]"),
            .fns = ~ .x > 1
        )
    ) %>%
    susoreview::make_issue_in_roster(
        where = stop_work_bc_disability == 1 & has_disability == 0,
        roster_vars = "hhmembers__id",
        type = 1,
        desc = "Not working due to disability, but no disability reported",
        comment = paste0(
            "ERROR: Member not working due to disability, but no disability reported",
            "In the employment module, reported not seeking work due to disability. ",
            "But in the functioning module, reported not having any difficulties ",
            "or disabilities. ",
            "These things cannot be true at the same time. Please check and correct."
        ),
        issue_vars = "M12_Q0[1-6]"
    )

# -----------------------------------------------------------------------------
# Main job is in household enterprise (labor, q23 == 1), 
# but no non-farm enterprise reported (nfe, E1 != 1) 
# -----------------------------------------------------------------------------

issue_work_in_hh_biz_but_no_biz <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("works_home_business", "n_enterprises"),
    where = works_home_business == 1 & n_enterprises == 0,
    type = 1,
    desc = "Member works in hhold biz, but no biz reported",
    comment = paste0(
        "ERROR: At least one member works in a household business, ",
        "but no household business reported.",
        "In the labor module, at least one member works in a household business.",
        "But in the enterprise module, no household business is reported. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# TODO: resume execution to check starting with next block 👇

# -----------------------------------------------------------------------------
# Paid for health services in the past 30 days (health, q11 %in% c(1, 3, 4, 7)), 
# but no health service expenditures reported in past 12 months (q1 with codes %in% c(58:61, 62:68, 75:79, 80:82, 83:90)) 
# -----------------------------------------------------------------------------

issue_no_health_expenditures <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("paid_for_health_sevices", "exp_any_health_services"),
    where = paid_for_health_sevices == 1 & exp_any_health_services == 0,
    type = 1,
    desc = "Paid for health services, but no health expenditures reported",
    comment = paste0(
        "ERROR: At least one member paid for health services, ",
        "but no health expenditures reported. ",
        "In the health module, at least one member reported paying ",
        "for health services in the past 30 days. ",
        "But in the health expenditure module, no expenditures were reported ",
        "for health services in the past 12 months. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# Employed in past 12 months (employment, q27 %in% c(1:3)), 
# but did not receive any labor income (labor income, q2 == 2)
# -----------------------------------------------------------------------------

issue_employed_no_labor_income <- susoreview::make_issue_in_roster(
    df = members,
    where = (
        # employed in income-earning role
        (M10_Q20 %in% c(1:3)) &
        # not earning a salary
        (M13_Q02 == 2)
    ),
    roster_vars = "hhmembers__id",
    type = 1,
    desc = "Employed in income-earning role, but not earning income",
    comment = paste0(
        "ERROR: Member employed in income-earning role, ",
        "but not earnig an income",
        "In the employment module, member reports working in an ",
        "income-earning role in the past 12 months. ",
        "But in the labor income module, that member reports not earning ",
        "any labor income, in any form, in the past 12 months. ",
        "These things cannot be true at the same time. Please check and correct."
    ),
    issue_vars = "M10_Q20|M13_Q02"
)

# -----------------------------------------------------------------------------
# At least one member involved a household business, 
# but no non-farm enterprise reported
# -----------------------------------------------------------------------------

issue_work_in_hhbiz_no_hhbiz <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("works_home_business", "n_enterprises"),
    where = works_home_business == 1 & n_enterprises == 0,
    type = 1,
    desc = "Work in household business, but no business reported",
    comment = paste0(
        "ERROR: At least one member works in a household business, ",
        "but no household business reported. ",
        "In the employment module, at least one household member reports ",
        "working in a household business in the past 12 months. ",
        "Meanwhile, in the non-farm enterprise module, no household ",
        "business is reported. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# Income from individuals and relatives outside Zimbabwe (non-labor income, q1 == 1 for item 16), 
# but no former members in migration (international migration, q1.Length == 0)
# -----------------------------------------------------------------------------

issue_intl_remit_no_migrants <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("received_itl_remittance", "n_migrated"),
    where = received_itl_remittance == 1 & n_migrated == 0,
    type = 1,
    desc = "Received remittance from abroad, but no relations live abroad",
    comment = paste(
        "ERROR: Received remittance from abroad, but no relations live abroad. ",
        "In the non-labor income module, the household reports receiving ",
        "transfers from outside of Zimbabwe. ",
        "Yet in the migration module, the household does not report any ",
        "former members living outside of the household. ",
        "This situation is possible, but should be verified. Please ",
        "check that the transfers from outside Zimbabwe came from ",
        "people that are not former members of the household."
    )
)

# -----------------------------------------------------------------------------
# Income from renting out means of transport, but no tranport assets declared
# -----------------------------------------------------------------------------

issue_rental_income_but_no_asset <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("received_vehicle_rental_income", "owns_vehicle"),
    where = received_vehicle_rental_income == 1 & owns_vehicle == 0,
    type = 1,
    desc = "Gets rental income from transport, but owns no transport assets",
    comment = paste(
        "ERROR: Household gets rental income from a means of transport, ",
        "but no means of transport is owned. ",
        "In the non-labor income module, te household reports income from ",
        "renting a means of transport. ",
        "Yet in the durable module, the household does not report owning ",
        "any means of transport. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# Has cell phone expenditures, but does not own a cell phone
# -----------------------------------------------------------------------------

issue_cell_exp_no_cell <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("has_cell_exp", "owns_cell"),
    where = has_cell_exp == 1 & owns_cell == 0,
    type = 1,
    desc = "Has cell phone expenditures, but does not own a cell phone",
    comment = paste(
        "ERROR: Household has cell phone expenditures, ",
        "but does not own a cell phone. ",
        "In communications expenditure module, the household reports ",
        "one or more cell phone expenditure in the past 12 months. ",
        "But in the durables module, the household does not report owning ",
        "a cell phone. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If any male clothes (clothing exp, q1 %in% c(6:19, 70:73, 114:120)), 
# then must have male member (hhroster, q7 == 1) 
# -----------------------------------------------------------------------------

issue_no_male <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("male_clothing_exp", "n_male"),
    where = male_clothing_exp == 1 & n_male == 0,
    type = 1,
    desc = "Male clothes expenditure, but no male members",
    comment = paste(
        "ERROR: Male clothes expenditure, but no male members ",
        "In the clothing expenditure module, the household reports ",
        "expenditures on male clothing. ",
        "Yet in the household characteristics module, the household does not ",
        "report any male members. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If any female clothes (clothing exp, q1 %in% c(20:38, 74:77, 121:127)), 
# then must have female member (hhroster, q7 == 2) 
# -----------------------------------------------------------------------------

issue_no_female <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("female_clothing_exp", "n_female"),
    where = female_clothing_exp == 1 & n_female == 0,
    type = 1,
    desc = "Female clothes expenditure, but no female members",
    comment = paste(
        "ERROR: Female clothes expenditure, but no female members ",
        "In the clothing expenditure module, the household reports ",
        "expenditures on female clothing. ",
        "Yet in the household characteristics module, the household does not ",
        "report any female members. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If drinking/washing water from network (housing, q16 %in% c(1, 2) | q19 %in% c(1, 2)), 
# should have some expenditure (house exp, q1 %in% c(20, 21)) 
# -----------------------------------------------------------------------------

issue_water_utility_no_exp <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("water_exp", "water_from_network"),
    where = water_from_network == 1 & water_exp == 0,
    type = 1,
    desc = "Uses tap water, but has no water expenditure",
    comment = paste(
        "ERROR: Household uses tap water, but has not water expenditure. ",
        "In the housing characterstics module, the household reports using ",
        "public tap water. ",
        "But in the housing expenditure module, it does not report any water ",
        "expenditures. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If use electricity (housing, q8 %in% c(1:8)), 
# should have electricity expenditures (q1 %in% c(28:34)) 
# -----------------------------------------------------------------------------

issue_electricity_use_no_exp <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("grid_electricity_exp", "use_gride_elec"),
    where = use_grid_elec == 1 & grid_electricity_exp == 0,
    type = 1,
    desc = "Use electricity, but do not have any electricity expenditure",
    comment = paste(
        "ERROR: Household uses electricity, but does not report any electricity expenditure. ",
        "In the housing characteristics module, the household reports using ",
        "electricity from a fee-based source. ",
        "But in the housing cost module, the household does not report any ",
        "electricity expenditures.",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If paid for electricity (housing exp, q1 %in% c(28:34)), 
# then have access to electricity (housing, q8 %in% c(1:8, 96)) 
# -----------------------------------------------------------------------------

issue_electricity_exp_no_use <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("electricity_exp", "use_electricity"),
    where = electricity_exp == 1 & use_electricity == 0,
    type = 1,
    desc = "Household has electricty expenditures, but does not have electricity.",
    comment = paste(
        "ERROR: Household has electricty expenditures, ", 
        "but does not have electricity. ",
        "In the housing cost module, the household reports electicity expenditure. ",
        "But in the housing characteristics module, the household reports not ",
        "using electricity from a fee-based source. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If pay for sewage (house exp, q1 %in% c(20, 21)), 
# then have toilet hooked to sewer (housing, q28 == 1) 
# -----------------------------------------------------------------------------

issue_pay_sewage_no_toilet <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("toilet_on_sewer", "sewage_exp"),
    where = sewage_exp == 1 & toilet_on_sewer == 0,
    type = 1,
    desc = "Household pays for sewage, but does not have toilet hooked to sewer.",
    comment = paste(
        "ERROR: Household pays for sewage, but does not have toilet ",
        "hooked to sewer. ",
        "In the housing costs module, the household reports expenditure on sewage. ",
        "But in the housing characteristics module, the household does not report ",
        "a toilet piped to the sewer. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If purchased durable in last 12 months, then likely should still own it. 
# -----------------------------------------------------------------------------

# TODO: create loop linking durable to asset

# -----------------------------------------------------------------------------
# If purchased DVD (leisure exp, q1 %in% c(141, 142)), 
# then likely own DVD player (assets, q1 == 1 for item 6) 
# -----------------------------------------------------------------------------

issue_bought_dvd_no_player <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_dvd", "owns_dvd_player"),
    where = bought_dvd == 1 & owns_dvd_player == 0,
    type = 1,
    desc = "Bought DVD, but does not own DVD player",
    comment = paste(
        "ERROR: Household bought a DVD, but does not own a DVD player. ",
        "In the leisure expenditure module, the household reports having ",
        "bought a DVD in the past 12 months. ",
        "But in the durable ownership module, the household reports not ",
        "owning a DVD player. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought fixed phone (comm exp, q1 == 1 for items %in% c(1, 2)), 
# then likely should own cell phone (assets, q1 == 1 for item 11) 
# -----------------------------------------------------------------------------

issue_bought_fixed_no_fixed <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_fixed_line", "owns_fixed_line"),
    where = bought_fixed_line == 1 & owns_fixed_line == 0,
    type = 1,
    desc = "Household bought fixed line phone equipment, but does not own fixed line. ",
    comment = paste(
        "ERROR: Household bought fixed line phone equipment, but does not own fixed line. ",
        "In the communication equipment and services module, the household ",
        "reports expenditures on fixed line telephoe equipment. ",
        "Yet in the durable ownership module, the household reports not owning ",
        "a fixed line telephone",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought cell phone (comm exp, q1 == 1 for items %in% c(8, 10, 11)), 
# then likely should own cell phone (assets, q1 == 1 for item 12) 
# -----------------------------------------------------------------------------

issue_bought_cell_no_cell <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("owns_cell", "bought_cell"),
    where = bought_cell == 1 & owns_cell ==0,
    type = 1,
    desc = "Bought cell/cell equipment, but does not own cell",
    comment = paste(
        "ERROR: Household bought cell phone/cell equipment but ",
        "does not own a cell phone. ",
        "In the communication equipment and services module, the household ",
        "reports expenditures on a cell phone and/or cell equipment. ",
        "But in the durable ownership module, the household reports not ",
        "a cell phone",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought computer (leisure, q1 == 1 for items %in% c(12, 13)), 
# then likely should own computer (assets, q1 == 1 for item 10) 
# -----------------------------------------------------------------------------

issue_bought_pc_no_pc <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_computer", "owns_computer"),
    where = bought_computer == 1 & owns_computer == 0,
    type = 1,
    desc = "Bought computer, but does not own a computer",
    comment = paste(
        "ERROR: Household bought a computer, but does not own one. ",
        "In the communication equipment and services module, the household ",
        "reports expenditures on a computer/laptop in the last 12 months. ",
        "Meanwhile, in the durable ownership module, the household reports ",
        "not owning a computer/laptop. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought TV (leisure, q1 == 1 for item 29), 
# then likely should own TV (assets, item 5)
# -----------------------------------------------------------------------------

issue_bought_tv_no_tv <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("owns_tv", "bought_tv"),
    where = bought_tv == 1 & owns_tv == 0,
    type = 1,
    desc = "bought tv, but does not own one",
    comment = paste(
        "ERROR: Household bought a TV, but does not own one. ",
        "In the communication equipment and services module, the household ",
        "reports expenditures on a TV set in the past 12 months. ",
        "Yet in the durable ownership module, the household reports not ",
        "owning a tv. ",
        "these things are unlikely to be true at the same time. please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought DVD player (leisure, q1 == 1 for item 30), 
# then likely should own DVD player (assets, item 6) 
# -----------------------------------------------------------------------------

issue_bought_dvd_no_dvd <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("owns_dvd_player", "bought_dvd_player"),
    where = bought_dvd_player == 1 & owns_dvd_player == 0,
    type = 1,
    desc = "Bought DVD player, but does not own one.",
    comment = paste(
        "ERROR: household bought a DVD player, but does not own one. ",
        "In the communication equipment and services module, the household ",
        "reports expenditures on a DVD player in the past 12 months. ",
        "Yet in the durable ownership module, the household reports not ",
        "owning a DVD player. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought motor vehicle (transport exp, q1 in c(1:4)), 
# then likely owns motor car (assets q1 == 1 for 1)
# -----------------------------------------------------------------------------

issue_bought_vehicle_not_own <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_motor_vehicle", "owns_motor_vehicle"),
    where = bought_motor_vehicle == 1 & owns_motor_vehicle == 0,
    type = 1,
    desc = "Bought motor vehicle, but does not own one",
    comment = paste(
        "ERROR: Household bought motor vehicle, but does not own one",
        "In the transport expenditure module, the household reports buying ",
        "a motor vehicle. ",
        "But in the durable ownership module, the household reports not ",
        "owning a motor vehicle. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought motorcycle (transport exp, q1 in c(5:10)), 
# then likely own motorcycle (assets q1 == 1 for item 2) 
# -----------------------------------------------------------------------------

issue_bought_motorcycle_not_own <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_motorcycle", "owns_motorcycle"),
    where = bought_motorcycle == 1 & owns_motorcycle == 0,
    type = 1,
    desc = "Bought motor vehicle, but does not own one",
    comment = paste(
        "ERROR: Household bought motorcycle, but does not own one",
        "In the transport expenditure module, the household reports buying ",
        "a motorcycle. ",
        "But in the durable ownership module, the household reports not ",
        "owning a motorcycle. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If bought bicycle (transport exp, q1 in c(11:14)), 
# then likely own bicycle (assets, q1 in c(11:14)) 
# -----------------------------------------------------------------------------

issue_bought_bicycle_not_own <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_bicycle", "owns_bicycle"),
    where = bought_bicycle == 1 & owns_bicycle == 0,
    type = 1,
    desc = "Bought bicycle, but does not own one",
    comment = paste(
        "ERROR: Household bought bicycle, but does not own one",
        "In the transport expenditure module, the household reports buying ",
        "a bicycle. ",
        "But in the durable ownership module, the household reports not ",
        "owning a bicycle. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If spent money on tires or parts (transport exp, q1 in c(17:20)), 
# then likely own vehicle (assets, q1, c(1:4)) 
# -----------------------------------------------------------------------------

issue_bought_tires_not_own_vehicle <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("bought_tires", "owns_vehicle"),
    where = bought_tires == 1 & owns_vehicle == 0,
    type = 1,
    desc = "Bought tires, but does not own a vehicle",
    comment = paste(
        "ERROR: Household bought tires, but does not own one",
        "In the transport expenditure module, the household reports buying ",
        "a tires. ",
        "But in the durable ownership module, the household reports not ",
        "owning a vehicle with tires. ",
        "These things are unlikely to be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# Perishable item consumed from purchase, but no purchase of that item reported
# -----------------------------------------------------------------------------

# TODO: construct indicator about whether consumed from purchase
# "any_perishable_purchased"

# -----------------------------------------------------------------------------
# If cereals consumed yesterday (diet, q4 == 1), 
# then ingredients in past 7 days (food, q1 == 1 for items %in% c(1:11, 12:21, 22:35, 36:42, 43: 48, 49:54)) 
# -----------------------------------------------------------------------------

issue_cereals_1d_not_7d <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("consumed_cereals_1d", "cereals_consumed_7d"),
    where = consumed_cereals_1d == 1 & cereals_consumed_7d == 0,
    type = 1,
    desc = "Consumed cereals in past 1 day, but not past 7 days",
    comment = paste(
        "ERROR: Household consumed cereals in past 1 day, but not past 7 days",
        "In the dietary diversity module, the household reported consuming ",
        "cereals yesterday. ",
        "But in the food consumption module, the household did not report ",
        "consuming any cereals in the past 7 days. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If tubers consumed yesterday (diet, q5 == 1), 
# then ingredients in past 7 days (food, q1 == 1 for items %in% c(246:249, 260)) 
# -----------------------------------------------------------------------------

issue_tubers_1d_not_7d <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("consumed_tubers_1d", "tubers_consumed_7d"),
    where = consumed_tubers_1d == 1 & tubers_consumed_7d == 0,
    type = 1,
    desc = "Consumed tubers in past 1 day, but not past 7 days",
    comment = paste(
        "ERROR: Household consumed tubers in past 1 day, but not past 7 days",
        "In the dietary diversity module, the household reported consuming ",
        "tubers yesterday. ",
        "But in the food consumption module, the household did not report ",
        "consuming any tubers in the past 7 days. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If legumes consumed yesterday (diet, q6 == 1), 
# then ingredients in past 7 days (food, q1 == 1 for items %in% c(250:255, 256, 258, 210:215)) 
# -----------------------------------------------------------------------------

issue_legumes_1d_not_7d <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("consumed_legumes_1d", "legumes_consumed_7d"),
    where = consumed_legumes_1d == 1 & legumes_consumed_7d == 0,
    type = 1,
    desc = "Consumed legumes in past 1 day, but not past 7 days",
    comment = paste(
        "ERROR: Household consumed legumes in past 1 day, but not past 7 days",
        "In the dietary diversity module, the household reported consuming ",
        "legumes yesterday. ",
        "But in the food consumption module, the household did not report ",
        "consuming any legumes in the past 7 days. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If vegetable consumed yesterday (diet, q7 == 1), 
# then ingredients in past 7 days (food, q1 == 1 for items %in% c(219:225, 226:233, 240:245, 259)) 
# -----------------------------------------------------------------------------

issue_veggies_1d_not_7d <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("consumed_veggies_1d", "veggies_consumed_7d"),
    where = consumed_veggies_1d == 1 & veggies_consumed_7d == 0,
    type = 1,
    desc = "Consumed veggies in past 1 day, but not past 7 days",
    comment = paste(
        "ERROR: Household consumed veggies in past 1 day, but not past 7 days",
        "In the dietary diversity module, the household reported consuming ",
        "veggies yesterday. ",
        "But in the food consumption module, the household did not report ",
        "consuming any veggies in the past 7 days. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# -----------------------------------------------------------------------------
# If fruits consumed yesterday (diet, q8 == 1), 
# then ingredients in past 7 days (food, q1 == 1 for items %in% c(177:182, 183:186, 187:209)) 
# -----------------------------------------------------------------------------

issue_fruits_1d_not_7d <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("consumed_fruits_1d", "fruits_consumed_7d"),
    where = consumed_fruits_1d == 1 & fruits_consumed_7d == 0,
    type = 1,
    desc = "Consumed fruits in past 1 day, but not past 7 days",
    comment = paste(
        "ERROR: Household consumed fruits in past 1 day, but not past 7 days",
        "In the dietary diversity module, the household reported consuming ",
        "fruits yesterday. ",
        "But in the food consumption module, the household did not report ",
        "consuming any fruits in the past 7 days. ",
        "These things cannot be true at the same time. Please check and correct."
    )
)

# =============================================================================
# Combine all issues
# =============================================================================

# combine all issues
issues <- dplyr::bind_rows(mget(ls(pattern = "^issue_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^issue_"))

# =============================================================================
# Add issues from interview metadata
# =============================================================================

# -----------------------------------------------------------------------------
# ... if questions left unanswered
# -----------------------------------------------------------------------------

# extract number of questions unanswered
interview_stats <- suso_diagnostics %>%
    # rename to match column names from GET /api/v1/interviews/{id}/stats
    dplyr::rename(
        NotAnswered = n_questions_unanswered,
        WithComments = questions__comments,
        Invalid = entities__errors
    ) %>%
    dplyr::select(interview__id, interview__key, NotAnswered, WithComments, Invalid)

# add error if interview completed, but questions left unanswered
# returns issues data supplemented with unanswered question issuesissues_plus_unanswered <- susoreview::add_issue_if_unanswered(
issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = cases_to_review,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 0,
    issue_desc = "Questions left unanswered",
    issue_comment = glue::glue("ERROR: The interview was marched at completed, but {NotAnswered} questions were left unanswered. Please answer these questions.")
)

# -----------------------------------------------------------------------------
# ... if any SuSo errors
# -----------------------------------------------------------------------------

# add issue if there are SuSo errors
# issues_plus_miss_and_suso 
issues_plus_miss_and_suso <- susoreview::add_issues_for_suso_errors(
    df_cases_to_review = cases_to_review,
    df_errors = suso_errors,
    issue_type = 3,
    df_issues = issues_plus_unanswered
)
