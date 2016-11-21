#' Text Analytics: Detect Language
#'
#' @references
#' \url{https://westus.dev.cognitive.microsoft.com/docs/services/TextAnalytics.V2.0/operations/56f30ceeeda5650db055a3c7}
#'
#' @examples
#' text_detectLanguages("yourApiKey",
#'                   list(text= "nice, it's perfect!"),
#'                   list(text= "今天天氣很好"),
#'                   list(text= "ཀོང་དགས་།"))
#' @export
text_detect_languages <- function(apiKey, ...) {
  listOfDocData = list(...)
  listOfDocDataStrings = lapply(listOfDocData, function(args) {
    do.call(detectLanguage.buildOneDocument, args)
  })

  postBody = do.call(detectLanguage.buildQueryBody, listOfDocDataStrings)
  headers = add_headers('Content-Type' = 'application/json',
                        'Ocp-Apim-Subscription-Key' = apiKey)

  res = POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/languages",
             headers,
             body = postBody)

  resData = res %>% content()
  resData$postBody = postBody
  return(resData)
}


detectLanguage.buildOneDocument = function(id = NULL, text= "nice!") {
  if (is.null(id)) {
    id = uuid::UUIDgenerate(T)
  }

  return(sprintf('{"id": "%s","text": "%s"}', id, text))
}


detectLanguage.buildQueryBody = function(...){
  docs = list(...)
  docs$sep = ","
  sprintf('{"documents": [%s]}', do.call(paste, docs))
}
