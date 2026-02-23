.assign_into_caller <- function(name, value, envir = parent.frame(2L)) {
  assign(name, value, envir = envir)
  invisible(value)
}
