#' @title Display genus and species names in italics
#' @description Display genus and species names in italics
#' @name scientific_name_formatter
#' @rdname scientific_name_formatter
#' @keywords internal
#' @param raw_name Raw name.
# 
#' @export
scientific_name_formatter <- function(raw_name) {
  # strsplit returns a list but we are passing in only
  # one name so we just take the first element of the list
  words <- strsplit(raw_name, " ", fixed = TRUE)[[1]]
  stopifnot(length(raw_name) > 0)
  # to italicize if only genus present
  if (length(words) == 1) {
    return(bquote(paste(italic(.(words[1])))))
  } else if (length(words) > 1) {
    return(bquote(paste(italic(.(words[1])) ~ italic(.(words[2])))))
  }
}
