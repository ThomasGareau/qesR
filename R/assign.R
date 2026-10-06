# Opt-in assignment (design.md sections 1.2 constraint 2, and 2.3).
#
# Every export that can assign is a thin wrapper that passes its own
# parent.frame() down to an internal `.<name>_impl()`, so opt-in assignment
# lands in the frame of whoever called the exported name: the global
# environment at top level, a function's own frame when called inside a
# function. Legacy wrappers call the impl directly, never another export.
#
# .qes_assign() is the only place in the package that calls assign(); a test
# scans the namespace for any other use.

.qes_assign <- function(name, value, envir) {
  if (!is.environment(envir)) {
    stop("qesR internal error: no environment to assign into.", call. = FALSE)
  }
  assign(name, value, envir = envir)
  invisible(value)
}

# The one-time note for the exports whose assign_global default changed from
# TRUE (v0.4.4) to FALSE. It is shown once per session per function, for a
# call made from the global environment (the console, Rscript, or a script run
# with source()) that does not pass assign_global and is not quiet. Calls
# inside a function and calls while knitr is rendering never show it. It
# cannot tell whether the result was assigned, so it is worded to make sense
# either way.
.qes_assign_default_notice <- function(fn, object_name, quiet = FALSE, envir = NULL) {
  top_level <- is.environment(envir) && identical(environmentName(envir), "R_GlobalEnv")
  knitting <- isTRUE(getOption("knitr.in.progress"))
  if (isTRUE(quiet) || !top_level || knitting) return(invisible(FALSE))
  if (!.qes_once_first(paste0("assign_default:", fn))) return(invisible(FALSE))
  .qes_inform(
    "assign_default",
    class = "qesR_message_assign_default",
    args = list(fn, object_name),
    data = list(fn = fn, object_name = object_name)
  )
  invisible(TRUE)
}
