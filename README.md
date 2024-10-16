<!-- badges: start -->
[![R-CMD-check](https://github.com/crmaedo/heleramcar5/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crmaedo/heleramcar5/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# `heleramcar5` Package


## Overview

The `heleramcar5` package provides tools to interact with the Swedish Parliament (Riksdagen) data API, enabling users to query documents, retrieve document content, and perform basic text analysis, such as word frequency counting.

In this vignette, we will walk through the core functionality of the package, including:

- Querying documents from the Riksdagen API
- Retrieving and processing document content
- Counting word frequencies in text data

## Installation

You can install the `heleramcar5` package by running:

```r
# Install the heleramcar5 package
devtools::install_github("crmaedo/heleramcar5")
```

Once installed, load the package:

```r
library(heleramcar5)
```

## The `Endpoint` Class

The `Endpoint` class is used to manage HTTP requests to the Riksdagen API. It simplifies the process of making requests by allowing you to define a base URL and append relative paths as needed.

### Example

To create an `Endpoint` object with a base URL:

```r
# Create an endpoint for querying documents
endpoint <- Endpoint(base_url = "http://data.riksdagen.se/dokumentlista/")
```

The `make_api_call()` method performs the HTTP request and returns the response.

```r
# Make an API call using the endpoint
response <- endpoint$make_api_call("?sok=budget&doktyp=SFS")
```

## Querying Documents with `queryDocuments()`

The `queryDocuments()` function allows you to search for documents in the Riksdagen database using a specific query term, such as "budget."

### Usage

```r
# Search for documents related to "budget"
result <- queryDocuments(query = "budget")
head(result)
```

This function returns a data frame of documents matching your query, including metadata like document title, publication date, and more.

## Retrieving Document Content with `getDocument()`

The `getDocument()` function retrieves the content of a document by its ID. The content is returned as an HTML string.

### Usage

```r
# Retrieve document content by document ID
document <- getDocument(document_id = "sfs-2016-1091")
print(document)
```

## Analyzing Text with `wordsFrequency()`

The `wordsFrequency()` function is a helper that counts the frequency of words in a given text and returns a data frame with the word counts and rankings.

### Usage

```r
# Example text
text <- "This is a sample text with several words. This text is just a sample."

# Get word frequency
freq <- wordsFrequency(text)
print(freq)
```

This function is useful for basic text analysis, providing insights into the most frequent words in a document or text string.

## Conclusion

The `heleramcar5` package simplifies the process of interacting with the Riksdagen API and performing basic text analysis. With tools like `queryDocuments()`, `getDocument()`, and `wordsFrequency()`, you can easily search for, retrieve, and analyze legislative documents.

For further details, consult the package documentation and explore additional examples.

