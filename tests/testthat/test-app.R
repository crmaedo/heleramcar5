test_that("wordsFrequency works", {

  expected_output <- data.frame(
    word=factor(c("a", "is", "no", "repeating", "sample", "text", "this", "with", "words")),
    times=rep(1, 9),
    rank=rep(1, 9))

  text <- "this is a sample text with no repeating words"
  output <- wordsFrequency(text)

  expect_equal(expected_output, output)

})


test_that("queryDocuments works", {

  output <- queryDocuments("sfs-2016-1091")

  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 10)
})




test_that("getDocument works", {

  expected_output <- c("1", "kap.", "Inledande", "bestämmelser\n\nLagens",
                       "tillämpningsområde\n\n1")

  output <- getDocument(document_id = "sfs-2016-1091")

  # Test first five words match.
  first_five_words <- as.vector(unlist(strsplit(output, " "))[1:5])

  expect_equal(first_five_words, expected_output)

})




