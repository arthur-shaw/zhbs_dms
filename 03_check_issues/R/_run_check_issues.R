# =============================================================================
# Run scripts in order
# =============================================================================

# prepare
print("Loading data")
source(fs::path(check_issues_dir, "R", "00_prep.R"))

# compile attributes
print("Compiling data attributes ...")
source(fs::path(check_issues_dir, "R", "01_compile_attributes.R"))

# compile issues
print("Identifying data issues ...")
source(fs::path(check_issues_dir, "R", "02_compile_issues.R"))

# make decisions
print("Making approve/reject/review decisions ...")
source(fs::path(check_issues_dir, "R", "03_make_decisions.R"))

# execute decisions, sending comment", "reject instructions to the server
# but only if indicated that rejection should occur
if (should_reject == TRUE) {
    print("Rejecting interviews ...")
    source(fs::path(check_issues_dir, "R", "04_execute_decisions.R"))
}

# save results of actions to disk
print("Saving results to disk ...")
source(fs::path(check_issues_dir, "R", "05_save_results.R"))
