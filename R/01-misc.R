#//////////////////////////////////////////////////
#
#           Miscelleaneous functions
#
#//////////////////////////////////////////////////


#' Quick formatting of numbers
#'
#' Wrapper for \code{\link{formatC}}.
#'
#' @rdname format
#' @export
format_int <- function(x, width = 2, flag = "0")
{
  formatC(x, flag = flag, width = width)
}


#' @rdname format
#' @export
format_dec <- function(x, digits = 2)
{
  formatC(x, digits = digits, format = "f")
}


#' Prettify variable names
#'
#' Similar to \code{\link{make.names}} but does
#' more replacements and uses camel case.
#'
#' @param x A character vector or data frame
#' @export
#' @rdname make-names
#' @examples
#'  library(dplyr)
#'
#'  # workhorse: character vector
#'  "12var_äüß//__stuff" %>% make_names
#'
#'  # dataframe: make pretty colnames
#'  iris %>% make_names %>% names
#'
make_names <- function(x, ...)
{
  UseMethod("make_names")
}


#' @export
#' @rdname make-names
#' @importFrom textclean replace_non_ascii
#' @importFrom stringr str_replace_all
#'
make_names.default <- function(x)
{
  x %>%
    tolower %>%
    textclean::replace_non_ascii() %>%
    str_replace_all("^[0-9]*", "") %>%
    str_replace_all("[\\.\\(\\)=,\\/:\\-#+*]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_+|_+$", "")
}


#' @export
#' @rdname make-names
make_names.data.frame <- function(x)
{
  names(x) %<>% make_names
  x
}




