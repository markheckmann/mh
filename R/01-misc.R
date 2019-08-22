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
    str_replace_all(" +", "_") %>%
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


#' @export
#' @rdname make-names
make_names.data.table <- function(x)
{
  nms_old <- names(x)
  nms_new <- nms_old %>% make_names
  setnames(x, nms_old, nms_new)
  invisible(x)
}


#' Set encoding for entire columns in data.frame or data.table
#'
#' Wrapper around \code{\link{Encoding}}. Sets all character columns to given encoding.
#' more replacements and uses camel case. Caveat: Factors are ignored.
#'
#' @param x A data.frame or data.table object.
#' @param value Encoding to set.
#' @export
#' @rdname encode_chr_vars
#' @examples
#'  xx <- data.frame(a = LETTERS, b = letters, c = 1:26, stringsAsFactors = F)

`encode_chr_vars<-` <- function(x, value)
{
  UseMethod("encode_chr_vars<-")
}


#' @export
#' @rdname encode_chr_vars
`encode_chr_vars<-.data.frame`  <- function(x, value)
{
  jj <- which(sapply(x, is.character))  # position index of character columns
  for(j in jj) {
    Encoding(x[[j]]) <- value
  }
  x
}


#' @export
#' @rdname encode_chr_vars
`encode_chr_vars<-.data.table` <- function(x, value)
{
  jj <- which(sapply(x, is.character))  # position index of character columns
  for(j in jj) {
    Encoding(x[[j]]) <- value
  }
  invisible(x)
}

