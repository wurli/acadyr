#' Set the boundary when a new year begins
#'
#' @param x An academic/financial year object
#' @param value A date
#'
#' @export
set_year_boundary <- function(x, value) {
  stopifnot(
    inherits(value, "Date"),
    length(value) == 1L
  )
  attr(x, "boundary") <- value
  x
}

#' @export
#' @rdname set_year_boundary
`year_boundary<-` <- set_year_boundary
