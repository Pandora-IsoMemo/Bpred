#' Start Application
#'
#' @param port port of web application
#' @param launch.browser whether to launch the browser
#'
#' @export
startApplication <- function(
  port = 4242,
  launch.browser = getOption("shiny.launch.browser", interactive())
) {
  shiny::runApp(
    system.file("app", package = "Bpred"),
    port = port,
    host = "0.0.0.0",
    launch.browser = launch.browser
  )
}
