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


# ------------------------------
#           SHINY APP
# ------------------------------


library(shiny)
library(bslib)
library(DT)
library(ggplot2)


#' @title Graphical User Interface defintion for app
#'
#' @description appGUI provides a graphical user interface to this app.
#' @return Shiny UI object.
#'
#' @importFrom shiny h3 textInput textOutput helpText code
#' @export
appUI <- function() {

  ui <- page_sidebar(
    fillable = TRUE,
    # App title
    title = "Wordcount Analysis for Riksdag's documents",

    # Sidebar panel for text input.
    sidebar = sidebar(
      h3('Start here'),

      textInput("queryInput", "Search a document"),

      helpText("Insert document ID to trigger the word count analysis."),
      textInput("documentID", "Document ID"),

      card_header('About'),
      helpText(
        "This app demonstrates a simple workflow of fethcing data from
        SVERIGES RIKSGAG's database and displaying a simple wordcount analysis.
        You can install it in the usual way from your R console: "
      ),
      code('devtools::install_github(crmaedo, heleramcar5)')
    ),


    # Upper double column layout.
    layout_columns(
      card(
        card_header("Document text"),
        textOutput("documentID"),
        textOutput("documentText"),
        max_height = 500
      ),
      card(card_header("Word count analysis"),
           plotOutput("wordPlot", width = "100%"))
    ),

    # Bottom single card.
    card(
      card_header("Search results"),
      DT::dataTableOutput("queryResult")
    )
  )

  return(ui)
}


#' Define Shiny server logic.
#'
#' @param input Shiny input environment
#' @param output Shiny output environment
#'
#' @importFrom shiny renderText
#' @importFrom DT renderDataTable datatable
#' @export
appServer <- function(input, output) {


  # Render output table from :func:`queryDocuments`
  output$queryResult <- renderDataTable({

    queryResult <- queryDocuments(query = input$queryInput)
    dt <- datatable(
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
  
  # Render barplot for word count analysis
  output$wordPlot <- renderPlot({
    # Fetch the document text
    document_text <- getDocument(input$documentID)
    
    # Perform word frequency analysis
    freqDF <- wordsFrequency(document_text)
    
    # Filter top 10 words
    top10 <- freqDF[1:10, ]
    
    # Create ggplot barplot
    ggplot(top10, aes(x = reorder(word, -times), y = times)) +
      geom_bar(stat = "identity", fill = "#C4D8F3") +
      #coord_flip() +  # use if we want our plot to have horizontal lines
      labs(x = "Words", y = "Frequency") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
    

  })
}


#' Creates Shiny app.
#'
#' @export
createShiny <- function() {
  shinyApp(ui = appUI(), server = appServer)
}


createShiny()


