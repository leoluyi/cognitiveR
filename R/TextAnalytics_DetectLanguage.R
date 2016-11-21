#' Text Analytics: Detect Language
#'
#' @references
#' \url{https://westus.dev.cognitive.microsoft.com/docs/services/TextAnalytics.V2.0/operations/56f30ceeeda5650db055a3c7}
#'
#' @examples
#' \dontrun{
#' text_detect_languages(load_api_key(product = "Text-Analytics"),
#'                   list(text= "nice, it's perfect!"),
#'                   list(text= "今天天氣很好"),
#'                   list(text= "ཀོང་དགས་།"))
#' }
#' @export
text_detect_languages <- function(apiKey,
                                  ...,
                                  numberOfLanguagesToDetect = 1L) {

  listOfDocData = list(...)
  listOfDocDataStrings = lapply(listOfDocData, function(args) {
    do.call(detectLanguage.buildOneDocument, args)
  })

  url <- modify_url("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages",
                    query = list(numberOfLanguagesToDetect = numberOfLanguagesToDetect))
  headers = add_headers(`Content-Type` = 'application/json',
                        `Ocp-Apim-Subscription-Key` = apiKey)
  postBody = do.call(buildQueryBody, listOfDocDataStrings)

  res = POST(url,
             headers,
             body = postBody)

  if (http_type(res) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  if (status_code(res) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(res),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  resData = res %>% content(as = "text") %>% jsonlite::fromJSON()
  result <- resData$documents$detectedLanguages %>% do.call(rbind, .)
  post_body <- postBody %>% jsonlite::fromJSON()


  structure(
    list(
      documents = data.frame(text = post_body$documents$text,
                             result),
      post_body = postBody %>% jsonlite::fromJSON()
    ),
    class = "text_analytics_api"
  )
}

print.text_analytics_api <- function(x) {
  print(x$documents)
}

detectLanguage.buildOneDocument = function(id = NULL, text= "nice!") {
  if (is.null(id)) {
    id = uuid::UUIDgenerate(TRUE)
  }
  return(sprintf('{"id": "%s","text": "%s"}', id, text))
}
