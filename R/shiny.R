#' Function to run the Shiny App
#'
#' @export
run_gotedna_app <- function() {
    shiny::runApp(fs::path_package("GOTeDNA", "app"), launch.browser = TRUE)
}
