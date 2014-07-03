
#' Launches the RHRV graphical interface opening a web browser
#' @param port the port where to listen; 1971 by default.
#' @author Patricia Guillin
#' @export loadInterface
#' @import shiny
#' @import RHRV
#' @import knitr
loadInterface <- function(port = 1971L){
  shiny::runApp(system.file("www",package="interfaceHRV"), port = port)
}