.onLoad <- function(libname, pkgname) {
  s3_register("ggplot2::scale_type", "academic_year")
  s3_register("ggplot2::scale_type", "financial_year")
}
