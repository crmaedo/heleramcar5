library(httr2)


make_api_call <- function(url) url |> request() |> req_perform()


#' Searches documents in the Riksdagen database
#'
#' @param query Search query (e.g., "budget").
#'
#' @return data.frame
#' @export
#' @examples
#' result <- searchdocs(query = "budget")
searchdocs <- function(query) {

  base_url <- "http://data.riksdagen.se/dokumentlista/"

  include <- c(
    "traff", "domain", "database", "datum", "id", "rdrest", "slutdatum",
    "rddata", "plats", "klockslag", "publicerad", "systemdatum", "undertitel",
    "kalla", "kall_id", "dok_id", "inlamnad", "motionstid", "tilldelat", "lang",
    "titel", "dokumentnamn", "score", "url"
    )


  clean_document <- function(document) {
    document <- document[include]
    nullmask <- as.vector(sapply(document, is.null))
    document[nullmask] <- NA
    return(document)
  }


  url <- paste0(
    base_url,
    "?sok=",
    query,
    "&utformat=json&sort=datum&sortorder=desc"
    )

  # Extract list of documents from json response.
  jsonresp <- url |> make_api_call() |> resp_body_json()
  docs <- jsonresp$dokumentlista$dokument

  # Return data.frame
  t(data.frame(sapply(docs, clean_document)))
}



#' Retrieves document content as HTML string
#'
#' @param document_id Document id
#'
#' @return character
#' @export
#' @references https://www.riksdagen.se/sv/dokument-och-lagar/riksdagens-oppna-data/anvandarstod/sa-fungerar-dokument-id/
#' @examples
#' document <- get_document("GZ01MJU21")
getdocument <- function (document_id) {
  base_url <- "https://data.riksdagen.se/dokument/"
  url <- paste0(base_url, document_id)
  make_api_call(url)
}









