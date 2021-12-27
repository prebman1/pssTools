
#' String Split 1
#'
#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit(x, split = ",")
str_split_one <- function(x, split) {
  strsplit(x, split = split)[[1]]
}
