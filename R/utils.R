# Internal ---------------------------------------------------------------------
# replace_to_snakecase------------------------------------------------------------
#' @title replace_to_snakecase
#' @description Transform CamelCase to snake_cases
#' @name replace_to_snakecase
#' @rdname replace_to_snakecase
#' @keywords internal
#' @export
replace_to_snakecase <- function(x) {
  x <- base::gsub(pattern = "([A-Za-z])([A-Z])([a-z])", replacement = "\\1_\\2\\3", x = x) %>%
    base::gsub(pattern = ".", replacement = "_", x =  ., fixed = TRUE) %>%
    base::gsub(pattern = "([a-z])([A-Z])", replacement = "\\1_\\2", x = .) %>%
    stringi::stri_trans_toupper(str = .)
  return(x)
}#End replace_to_snakecase
