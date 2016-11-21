buildQueryBody <- function(...) {
  docs = list(...)
  sprintf('{"documents": [%s]}', paste(docs, sep = ",", collapse = ","))
}

#' Import API keys
#'
#' @param path Path to API keys (yaml format). See Details.
#'
#' @details
#' Default path to API keys file is ~/cognitive_api_keys.txt.
#'
#' \preformatted{
#' Text-Analytics: xxxxxx
#' Face: xxxxxxx
#' }
#'
#'
#' @import yaml
#' @export
load_api_key <- function(path = normalizePath("~/cognitive_api_keys.txt"),
                         product = c("Text-Analytics", "Face")) {
  product <- match.arg(product)
  keys <- yaml::yaml.load_file(path)
  keys[[pruduct]]
}
