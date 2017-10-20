
#' @export

set_shinydegs_theme <- function(...){

  theme <- list(...)

  theme <- papaja:::defaults(
    ellipsis = theme
    , set.if.null = list(
      col = "#75AADB"
      , border = "white"
    )
  )
  options(shinydegs.theme = theme)
}
