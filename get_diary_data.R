# =============================================================================
# Run setup
# =============================================================================

source("setup.R")

survey_type <- "drb"

# =============================================================================
# Get HBS data
# =============================================================================

data_dir <- fs::path(here::here(), "01_get_data")

source(fs::path(data_dir, "R", "delete.R"))
source(fs::path(data_dir, "R", "get.R"))
source(fs::path(data_dir, "R", "unpack.R"))
source(fs::path(data_dir, "R", "combine_and_save.R"))

