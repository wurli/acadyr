# Creation ----

#' Utilities for working with academic years
#'
#' @details
#' Note that many countries use a slightly different academic year start date 
#' each year, meaning that most use-cases for these utilities will necessitate
#' some approximation.
#' 
#' @param x * For `academic_year()`: A numeric vector
#' * For `as_academic_year()`: An object to coerce
#' * For `is_academic_year()`: An object to test
#' @param boundary A boundary date where the new academic year should begin.
#'   Note that only the month/day of this date is used. This can be set using
#'   the global option `acadyr.academic_year_boundary`. E.g. to use academic
#'   years starting on August 1, use the following:
#'   ``` r
#'   options(acadyr.academic_year_boundary = as.Date("2020-08-01"))
#'   ```
#' @param ... Passed on to methods
#'   
#' * For `academic_year()`: A `<financial_year>` vector
#' * For `as_academic_year()`: A `<financial_year>` vector
#' * For `is_academic_year()`: A `<logical>` vector
#' 
#' @export
#' 
#' @examples
#' # Numbers are treated as if they represent years
#' academic_year(2005:2015)
#' 
#' # Dates are mapped to the year they fall within
#' dates <- as.Date(c("2020-08-01", "2020-09-01"))
#' academic_year(dates)
#' 
#' # The year they fall within depends on the year boundary, which
#' # defaults to September 01
#' academic_year(dates, boundary = as.Date("2020-08-01"))
#' 
#' # as_academic_year() also understands character vectors:
#' as_academic_year(c("2020/21", "2021/22"))
academic_year <- function(x = integer(), boundary = getOption("acadyr.academic_year_boundary", as.Date("2020-09-01"))) {
  if (is.Date(x) || is.POSIXct(x) || is.POSIXlt(x)) {
    return(as_academic_year(x, boundary))
  }
  x <- vec_cast(x, integer()) 
  new_academic_year(x, boundary)
}

new_academic_year <- function(x = integer(), boundary = getOption("acadyr.academic_year_boundary", as.Date("2020-09-01"))) {
  stopifnot(
    is.integer(x), 
    all(0L <= x, x < 9999, na.rm = TRUE),
    inherits(boundary, "Date"),
    length(boundary) == 1L
  )
  new_vctr(x, class = "academic_year", boundary = boundary)
}

#' @export
#' @rdname academic_year
is_academic_year <- function(x) {
  inherits(x, "academic_year")
}

#' @export
#' @rdname academic_year
as_academic_year <- function(x, boundary = getOption("acadyr.academic_year_boundary", as.Date("2020-09-01")), ...) {
  UseMethod("as_academic_year")
}

#' @export
as_academic_year.default <- function(x, boundary = getOption("acadyr.academic_year_boundary", as.Date("2020-09-01")), ...) {
  vec_cast(x, new_academic_year(boundary = boundary))
}

#' @export
as_academic_year.character <- function(x, boundary = getOption("acadyr.academic_year_boundary", as.Date("2020-09-01")), ...) {
  correct_format <- grepl("^\\d{4}/\\d{2}$", x) 
  correct_digits <- ifelse(
    correct_format,
    suppressWarnings(as.integer(substr(x, 3, 4)) == as.integer(substr(x, 6, 7)) - 1L),
    FALSE
  )
  parsable <- correct_format & correct_digits
  if (!all(parsable)) {
    cli::cli_warn(c(
      "Malformed academic years detected",
      i = "Check {.val {unique(x[!parsable])}}"
    ))
  }
  x[!parsable] <- NA_character_
  academic_year(as.integer(substr(x, 1, 4)), boundary = boundary)
}

# Printing ----

#' @export
format.academic_year <- function(x, ...) {
  out1 <- formatC(vec_data(x), width = 4L, flag = "0")
  out2 <- formatC(vec_data(x) + 1L, width = 4L, flag = "0")
  out <- paste0(out1, "/", substr(out2, 3, 4)) 
  out[is.na(x)] <- NA_character_
  out
}

# Coercion ----

#' @export
vec_ptype_abbr.academic_year <- function(x, ...) "ay"

## academic_year ----

#' @export
vec_ptype2.academic_year.academic_year <- function(x, y, ...) {
  check_academic_years_have_same_boundary(x, y)
  new_academic_year(boundary = attr(x, "boundary"))
}

## integer ----

#' @export
vec_ptype2.academic_year.integer <- function(x, y, ...) integer()
#' @export
vec_ptype2.integer.academic_year <- function(x, y, ...) integer()

## double ----

#' @export
vec_ptype2.academic_year.double <- function(x, y, ...) double()
#' @export
vec_ptype2.double.academic_year <- function(x, y, ...) double()

# Casting ----

## academic_year ----

#' @export
vec_cast.academic_year.academic_year <- function(x, to, ...) {
  check_academic_years_have_same_boundary(x, to, call = rlang::caller_call())
  x
}

check_academic_years_have_same_boundary <- function(x, y, trying_to = "combine", call = rlang::caller_call()) {
  x <- attr(x, "boundary") |> strftime("%d %b")
  y <- attr(y, "boundary") |> strftime("%d %b")
  if (!identical(x, y)) {
    cli::cli_abort(
      c(
        "Cannot {trying_to} academic years with different boundaries",
        i = "Boundaries are {.val {x}} and {.val {y}}",
        i = "Use {.fun set_year_boundary} to update a boundary manually"
      ),
      call = call
    )
  }
}

## character ----

#' @export
vec_cast.character.academic_year <- function(x, to, ...) format(x)
#' @export
vec_cast.academic_year.character <- function(x, to, ...) format(x)

## integer ----

#' @export
vec_cast.integer.academic_year <- function(x, to, ...) vec_data(x)
#' @export
vec_cast.academic_year.integer <- function(x, to, ...) academic_year(x)

## double ----

#' @export
vec_cast.double.academic_year <- function(x, to, ...) as.double(vec_data(x))
#' @export
vec_cast.academic_year.double <- function(x, to, ...) {
  if (any(x - floor(x) != 0, na.rm = TRUE)) {
    cli::cli_abort(
      "Cannot coerce decimal to academic year", 
      call = rlang::caller_fn()
    )
  }
  academic_year(x)
}

## Date ----

#' @export
vec_cast.Date.academic_year <- function(x, to, ...) {
  if (length(x) == 0L) {
    return(new_date())
  }
  boundary <- strftime(attr(x, "boundary"), "-%m-%d")
  as.Date(paste0(as.integer(x), boundary))
}

#' @export
vec_cast.academic_year.Date <- function(x, to, ...) {
  boundary       <- attr(to, "boundary") 
  fmt            <- function(x, f) as.integer(strftime(x, f))
  boundary_month <- fmt(boundary, "%m")
  boundary_day   <- fmt(boundary, "%d")
  x_year         <- fmt(x, "%Y")
  x_month        <- fmt(x, "%m")
  x_day          <- fmt(x, "%d")
  
  out_year <- ifelse(
    x_month > boundary_month | (x_month == boundary_month & x_day >= boundary_day),
    x_year, x_year - 1L
  )
  
  academic_year(out_year, boundary = boundary)
}

## POSIXct ----

#' @export
vec_cast.academic_year.POSIXct <- function(x, to, ...) as_academic_year(as.Date(x))
#' @export
vec_cast.POSIXct.academic_year <- function(x, to, ...) as.POSIXct(as.Date(x))

## POSIXlt ----

#' @export
vec_cast.academic_year.POSIXlt <- function(x, to, ...) as_academic_year(as.Date(x))
#' @export
vec_cast.POSIXlt.academic_year <- function(x, to, ...) as.POSIXlt(as.Date(x))

# Math ----

#' @export
vec_math.academic_year <- function(.fn, .x, ...) {
  switch(
    .fn,
    prod = , sqrt = , log = , log10 = , log2 = , log1p = , acos = , acosh = ,
    asin = , asinh = ,  atan = , atanh = , exp = , expm1 = , cos= , cosh = , 
    cospi = , sin = , sinh = , sinpi = , tan = , tanh = , tanpi = , gamma = , 
    lgamma = , digamma = , trigamma = , 
    mean = cli::cli_abort(
      "{.fun { .fn}} not implemented for {.class {vec_ptype_full(.x)}}",
      call = rlang::caller_call()
    ),
    academic_year(vec_math_base(.fn, .x, ...), boundary = attr(.x, "boundary"))
  )
}

# Arithmetic ----

#' @export
#' @method vec_arith academic_year
vec_arith.academic_year <- function(op, x, y, ...) {
  UseMethod("vec_arith.academic_year", y)
}

## default ----

#' @export
#' @method vec_arith.academic_year default
vec_arith.academic_year.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

## academic_year ----

#' @export
#' @method vec_arith.academic_year academic_year
vec_arith.academic_year.academic_year <- function(op, x, y, ...) {
  check_academic_years_have_same_boundary(x, y, trying_to = "compare", call = rlang::caller_call())
  switch(
    op,
    "-" = new_academic_year(vec_arith_base(op, x, y), attr(x, "boundary")),
    # new_academic_year(vec_arith_base(op, x, y), boundary = attr(x, "boundary"))
    stop_incompatible_op(op, x, y)
  )
}

## numeric ----

#' @export
#' @method vec_arith.academic_year numeric
vec_arith.academic_year.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_academic_year(vec_arith_base(op, x, vec_cast(y, integer())), attr(x, "boundary")),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.numeric academic_year
vec_arith.numeric.academic_year <- function(op, x, y, ...) {
  vec_arith.academic_year.numeric(op, y, x, ...)
}

## logical ----

#' @export
#' @method vec_arith.academic_year logical
vec_arith.academic_year.logical <- function(op, x, y, ...) {
  switch(
    op,
    "!" = , "&" = , "|" = vec_arith_base(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.logical academic_year
vec_arith.logical.academic_year <- vec_arith.academic_year.logical
  
# Misc ----

## Ops ----

# This tells R to use Ops generic for <academic_year> instead of base classes,
# e.g. when doing academic_year(2021) + Sys.time(). This means that, in 
# such cases, you reliably get an informative error instead of a warning 
# and a nonsensical value. Only works in R >=4.3.0.
#' @export
chooseOpsMethod.academic_year <- function(x, y, mx, my, cl, reverse) TRUE

## ggplot2 ----

# Registered in `.onLoad()`
scale_type.academic_year <- function(x) c("academic_year", "continuous")

#' Position scales for academic years
#'
#' @param labels,... Passed to `ggplot2::scale_(x|y)_continuous()` 
#'
#' @export
scale_x_academic_year <- function(labels = ~ format(academic_year(.)), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is not installed")
  }
  ggplot2::scale_x_continuous(labels = labels, ...)
}

#' @export
#' @rdname scale_x_academic_year
scale_y_academic_year <- function(labels = ~ format(academic_year(.)), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is not installed")
  }
  ggplot2::scale_y_continuous(labels = labels, ...)
}
