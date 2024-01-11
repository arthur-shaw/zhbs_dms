# =============================================================================
# Run setup
# =============================================================================

source("setup.R")

survey_type <- "hbs"

# =============================================================================
# Get HBS data
# =============================================================================

# set file paths
# data
data_dir <- fs::path(here::here(), "01_get_data")
downloaded_dir <- fs::path(data_dir, "data", survey_type, "01_downloaded")
combined_dir <- fs::path(data_dir, "data", survey_type, "02_combined")
derived_dir <- fs::path(data_dir, "data", survey_type, "03_derived")
# check issues
check_issues_dir <- fs::path(here::here(), "03_check_issues")
output_dir <- fs::path(check_issues_dir, "output")

# execute actions
source(fs::path(data_dir, "R", "delete.R"))
source(fs::path(data_dir, "R", "get.R"))
source(fs::path(data_dir, "R", "unpack.R"))
source(fs::path(data_dir, "R", "combine_and_save.R"))
source(fs::path(data_dir, "R", "reconstruct_rosters.R"))

# =============================================================================
# Run scripts in order
# =============================================================================

source(fs::path(check_issues_dir, "R", "_run_check_issues.R"))
