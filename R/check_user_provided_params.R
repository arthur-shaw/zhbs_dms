# ==============================================================================
# Server connection details
# ==============================================================================

# ------------------------------------------------------------------------------
# Connection details provided
# ------------------------------------------------------------------------------

missing_server_params <- c(server, workspace, user, password) == ""

if (any(missing_server_params)) {

    connection_params <- c("server", "workspace", "user", "password")

    missing_server_params_txt <- connection_params[missing_server_params]

    stop(
        glue::glue(
            "Server connection details missing.",
            paste0(
                "The following details were left empty in setup.R:",
                glue::glue_collapse(
                    glue::backtick(missing_server_params_txt), 
                    last = ", and "
                )
            ),
            .sep = "\n"
        )
    )

}

# ------------------------------------------------------------------------------
# Server exists at specified URL
# ------------------------------------------------------------------------------

server_exists <- function(url) {

	tryCatch(
		expr = httr::status_code(httr::GET(url = url)) == 200,
		error = function(e) {
			FALSE
		}
	)
}

if (!server_exists(url = server)) {
	stop(paste0("Server does not exist at address provided: ", server))
}

# ------------------------------------------------------------------------------
# Credentials valid
# ------------------------------------------------------------------------------

credentials_valid <- suppressMessages(
	susoapi::check_credentials(
		verbose = TRUE
	)
)

if (credentials_valid == FALSE) {

	stop(glue::glue(
        "Credentials invalid for API user.",
        "One of the following issues may present.",
        paste0(
            "1. The credentials provided may be invalid",
            "(e.g., wrong user, password, etc.)."
        ),
        paste0(
            "2. The credentials may not be for the right kind of user",
            "(i.e., admin or API)."
        ),
        "3. the user may not have access to the targeted workspace.",
        "Please check and revise the credentials.",
        .sep = "\n"
    ))

}

# ------------------------------------------------------------------------------
# Server exists
# ------------------------------------------------------------------------------


# =============================================================================
# 03 - Check issues parameters
# =============================================================================

if (!all(statuses_to_reject %in% c(100, 120, 130))) {

    stop(
        "Invalid status code provided.",
        paste0(
            "Please provide code(s) 100, 120, 130 for the `statuses_to_reject`",
            "parameter in setup.R."
        ),
        .sep = "\n"
    )

}

if (!all(issues_to_reject %in% c(1, 2, 3, 4))) {

    stop(
        "Invalid issue type.",
        paste0(
            "Please provide code(s) 1, 2, 3, 4 for the `issues_to_reject`",
            "parameter in setup.R."
        ),
        .sep = "\n"

    )

}

if (!all(should_reject %in% c(TRUE, FALSE))) {

    stop(
        "Invalid instruction for whether or not to reject interviews.",
        paste0(
            "Please provide `TRUE` or `FALSE` for the `should_reject`",
            "parameter in setup.R."
        ),
        .sep = "\n"

    )

}
