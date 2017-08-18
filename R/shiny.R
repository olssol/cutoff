
#' Run Web-Based application
#'
#'
#' @details
#'
#' A web browser will be brought up for users to access the GUI
#'
#'
#' @export
#'
cfShiny <- function() {

    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Shiny needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("shinythemes", quietly = TRUE)) {
        stop("shinythemes needed for this function to work. Please install it.",
             call. = FALSE)
    }

    if (!requireNamespace("DT", quietly = TRUE)) {
        stop("DT needed for this function to work. Please install it.",
             call. = FALSE)
    }

    appDir <- system.file("shiny", package = "cutoff")
    if (appDir == "") {
        stop("Could not find Shiny directory. Try re-installing `idem`.",
             call. = FALSE)
    }


    shiny::runApp(appDir, display.mode = "normal");
}
