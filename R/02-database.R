#//////////////////////////////////////////////////
#
#           Data base related functions
#
#//////////////////////////////////////////////////


#' Fetch data from query in chunks
#'
#' @param res Object returned by \code{DBI::dbSendQuery}
#' @param progress Show progress in console?
#' @param chunk_size Number of rows to be fethced in each loop. Number smaller
#'   than 1 are itertreted as fractions of total number of affected rows.
#' @return data.table
#' @export
#'
db_fetch_chunks <- function(res, progress = TRUE, chunk_size = 0.01)
{
  # determine chunk size
  rows_total <- DBI::dbGetRowsAffected(res)
  if (chunk_size < 1) {
    chunk_size <- round(chunk_size * rows_total)
  }

  # fetch data in loop
  l <- list()
  i <- 1
  while( !DBI::dbHasCompleted(res) ) {

    l[[i]] <- DBI::dbFetch(res, n = chunk_size)
    rows_fetched <- DBI::dbGetRowCount(res)
    if (progress) {
      cat("\rfetching chunk", i,
          "fetched:", rows_fetched, "/", rows_total, "=",
          scales::percent(rows_fetched / rows_total))
    }
    i <- i + 1
  }
  DBI::dbClearResult(res)
  data.table::rbindlist(l)
}

