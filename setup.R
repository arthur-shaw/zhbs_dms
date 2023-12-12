# =============================================================================
# Load required packages 
# =============================================================================

source(fs::path(here::here(), "R/install_required_packages.R"))

# =============================================================================
# Provide Survey Solutions server connection details
# =============================================================================

server      <- "" # provide the complete URL of the server
workspace   <- "" # use the name that is an ID rather than the display name
user        <- "" # note: user must be either admin or API user
password    <- "" # password of the user indicated above

susoapi::set_credentials(
    server = server,
    workspace = workspace,
    user = user,
    password = password
)

# =============================================================================
# Identify questionnaires whose data to fetch
# =============================================================================

# provide a string that uniquely identifies the questionnaire. this can be:
# - full name
# - sub-string
# - regular expression

# recall data
qnr_expr_hbs <- ""

# diary record book data
qnr_expr_drb <- ""

# =============================================================================
# 03 - Check issues parameters
# =============================================================================

# Provide a comma-separated list of interview statuses to review.
# See status values here: 
# https://docs.mysurvey.solutions/headquarters/export/system-generated-export-file-anatomy/#coding_status
#
# Statuses supported by this script include: 
# - Completed: 100
# - ApprovedBySupervisor: 120
# - ApprovedByHeadquarters: 130
statuses_to_reject <- c(100, 120)

# Provide a comma-separated list of issue types to reject
# {susoreview} uses the following codes:
# - 1 = Reject
# - 2 = Comment to post
# - 3 = Survey Solutions validation error
# - 4 = Review
issues_to_reject <- c(1)

# Whether to reject interviews recommended for rejection
# - If TRUE, the program will instruct the server to reject these interviews.
# - If FALSE, the program will not.
# - In either case, the interviews recommended for rejection, 
# and the reasons why, are saved in `/output/`
should_reject <- FALSE

# =============================================================================
# Check user inputs
# =============================================================================

source(fs::path(here::here(), "R/check_user_provided_params.R"))
