library(httr2)
library(xml2)

#' Endpoint
#'
#' Executes http requests against the specified URL.
#'
#' @examples
#' endpoint <- Endpoint("http://my-api-url.com")
#'
#' @import methods
#' @importFrom httr2 request req_perform
#' @export Endpoint
#' @exportClass Endpoint
#' @field base_url character. Base URL.
Endpoint <- setRefClass(
  "Endpoint",
  fields = list(base_url = "character")
)


Endpoint$methods(
  make_api_call = function(url = "") {
    url <- paste0(base_url, url)
    url |> request() |> req_perform()
  }
)


# Creates instances for :class:`Endpoint`.
# These will be used throughout the project for making api calls.
docEndpoint <- Endpoint(base_url = "https://data.riksdagen.se/dokument/")
queryEndpoint <- Endpoint(base_url = "http://data.riksdagen.se/dokumentlista/")


#' Auxiliary function for counting the number of words in a string.
#'
#' @param text Input text.
#'
#' @return data.frame
#' @export
wordsFrequency <- function(text) {


  wordsList <- strsplit(text, split = " ")
  freqDF <- as.data.frame(table(unlist(wordsList)))

  names(freqDF) <- c("word","times")
  freqDF$rank <- rank(-freqDF$times, ties.method = "min")
  freqDF <- freqDF[order(freqDF$rank, decreasing = F),]
  return(freqDF)
}


#' Searches documents in the Riksdagen database
#'
#' @param query Search query (e.g., "budget").
#' @return data.frame
#'
#' @importFrom httr2 resp_body_json
#' @export
#' @examples
#' result <- queryDocuments(query = "budget")
queryDocuments <- function(query) {

  include <- c(
    "traff", "domain", "database", "datum", "id", "publicerad",
    "systemdatum","dok_id", "titel", "score"
    )


  cleanDocument <- function(document) {
    document <- document[include]
    nullmask <- as.vector(sapply(document, is.null))
    document[nullmask] <- NA
    return(document)
  }

  # Construct URL and make api call.
  url <- paste0("?sok=", query, "&doktyp=SFS&utformat=json&sort=datum&sortorder=desc")
  jsonResp <- url |> queryEndpoint$make_api_call() |> resp_body_json()

  # Extract list of documents from json response.
  docs <- jsonResp$dokumentlista$dokument

  # Return data.frame
  t(data.frame(sapply(docs, cleanDocument)))
}


#' Retrieves document content as HTML string
#'
#' @param document_id Document id
#' @return character
#' @examples
#' document <- getDocument(document_id = "sfs-2016-1091")
#'
#' @export
#' @importFrom httr2 resp_body_xml
#' @importFrom xml2 as_list
#' @references https://www.riksdagen.se/sv/dokument-och-lagar/riksdagens-oppna-data/anvandarstod/sa-fungerar-dokument-id/
getDocument <- function (document_id) {

  resp <- docEndpoint$make_api_call(url = document_id)

  extract_text <- function(resp) {
    xmldata <- resp_body_xml(resp)
    xmllist <- as_list(xmldata)
    return(xmllist$dokumentstatus$dokument$text[[1]])
  }

  return (resp |> extract_text())
}
