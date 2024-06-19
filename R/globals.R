# This is a workaround for the R-CMD-check error
#   no visible binding for global variable ‘.’
# which circumvents warnings caused by using '.' inside pipes.
utils::globalVariables(c("."))
