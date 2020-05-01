assert <- function(statement, msg="")
{
  if (!statement)
  {
    stop(msg, call.=(msg==""))
  }
}

ifnull <- function(value, default)
{
  if (is.null(value)) { return(default) }

  return(value)
}

cmsg <- function(verbose, fmt, ...)
{
  if (verbose)
  {
    cat(sprintf(fmt, ...))
  }
}
