library(httr2)
library(xml2)

#' Endpoint
#'
#' Executes http requests against the specified URL.
#'
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
#'
#' @return data.frame
#' @export
#' @examples
#' result <- queryDocs(query = "budget")
queryDocuments <- function(query) {

  include <- c(
    "traff", "domain", "database", "datum", "id",
    "publicerad", "systemdatum", "undertitel",
    "kalla", "kall_id", "dok_id", "lang",
    "titel", "dokumentnamn", "score"
    )


  cleanDocument <- function(document) {
    document <- document[include]
    nullmask <- as.vector(sapply(document, is.null))
    document[nullmask] <- NA
    return(document)
  }

  # Construct URL and make api call.
  url <- paste0("?sok=", query, "&utformat=json&sort=datum&sortorder=desc")
  jsonResp <- url |> queryEndpoint$make_api_call() |> resp_body_json()

  # Extract list of documents from json response.
  docs <- jsonResp$dokumentlista$dokument

  # Return data.frame
  t(data.frame(sapply(docs, cleanDocument)))
}


#' Retrieves document content as HTML string
#'
#' @param document_id Document id
#'
#' @return character
#' @export
#' @references https://www.riksdagen.se/sv/dokument-och-lagar/riksdagens-oppna-data/anvandarstod/sa-fungerar-dokument-id/
#' @examples
#' document <- getDocument(document_id = "sfs-2016-1091")
getDocument <- function (document_id) {

  resp <- docEndpoint$make_api_call(url = document_id)

  extract_text <- function(resp) {
    xmldata <- resp_body_xml(resp)
    xmllist <- as_list(xmldata)
    return(xmllist$dokumentstatus$dokument$text[[1]])
  }

  return (resp |> extract_text())
}


# ------------------------------
#           SHINY APP
# ------------------------------


library(shiny)
library(bslib)



# Define UI for app.
ui <- page_sidebar(
  fillable = TRUE,
  # App title
  title = "Wordcount Analysis for Riksdag's documents",

  # Sidebar panel for text input.
  sidebar = sidebar(
    h3('Start here'),

    textInput("queryInput", "Search a document"),

    helpText("Insert document ID to trigger the word count analysis."),
    textInput("documentID", "Insert document ID"),

    card_header('About'),
    helpText(
    "
    This app demonstrates a simple workflow of fethcing data
    from SVERIGES RIKSGAG's database and displaying a simple wordcount analysis.
    You can install it in the usual way from your R console:
    "
    ),

    code('devtools::install_github(crmaedo, heleramcar5)')
  ),


  layout_columns(

    card(card_header("Document text"),
         textOutput("documentID"),
         textOutput("documentText"),
         max_height = 500
    ),

    card(card_header("Word count analysis"))
  ),

  card(card_header("Search results"),
       DT::dataTableOutput("queryResult")
  )
)



# Define server logic required.
server <- function(input, output) {


  # Render output table from :func:`queryDocuments`
  output$queryResult <- DT::renderDataTable({

    queryResult <- queryDocuments(query = input$queryInput)
    dt <- DT::datatable(
      queryResult,
      options = list(scrollX = TRUE,
                     pageLength = 10,
                     autoWidth = TRUE
      ),
      rownames = FALSE
    )

    return(dt)
  })

  # Render user selected document ID.
  output$documentID <- renderText({
    paste("You have selected document ID:", input$documentID)
  })

  # Render document text.
  output$documentText <- renderText({
    getDocument(input$documentID)

  })
}


shinyApp(ui = ui, server = server)
