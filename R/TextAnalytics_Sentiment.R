#' Text Analytics: Sentiment
#'
#' @references
#' \url{https://westus.dev.cognitive.microsoft.com/docs/services/TextAnalytics.V2.0/operations/56f30ceeeda5650db055a3c7}
#'
#' @examples
#' text_sentiment("yourApiKey", list(lang="en", id="", text= "nice!"),list(lang="en", id="", text= "nice!"),list(lang="en", id="", text= "nice!"))
#' @import uuid
#' @export
text_sentiment = function(apiKey, ...){
  listOfDocData = list(...)
  listOfDocDataStrings = lapply(listOfDocData,function(args){
    do.call(sentiment.buildOneDocument,args)
  })

  postBody = do.call(buildQueryBody, listOfDocDataStrings)

  headers = add_headers('Content-Type'='application/json',
                        'Ocp-Apim-Subscription-Key' = apiKey)

  res = POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
             headers,
             body = postBody)

  resData = res %>% content()
  resData$postBody = postBody
  # resData$postBodyDf = data.frame(do.call(rbind,rjson::fromJSON(postBody)))
  return(resData)
}

sentiment.buildOneDocument = function(lang="en", id=NULL, text= "nice!"){
  if (id==""){
    id = UUIDgenerate(T)
  }
  return(sprintf('{"language": "%s","id": "%s","text": "%s"}', lang, id, text))
}

