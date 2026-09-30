library(quanteda)
library(wordvector)
library(AWE)
options(wordvector_threads = 2)

# corp <- wordvector::data_corpus_news2014
# corp_test <- corpus_reshape(corp)
#
# toks_test <- tokens(corp_test, remove_punct = TRUE,
#                     remove_symbols = TRUE, remove_numbers = TRUE,
#                     concatenator = " ") |>
#              tokens_remove(stopwords("en"), min_nchar = 2) |>
#              tokens_subset(min_ntoken = 2)

test_that("c.textmodel_word2vec works", {

  mat1 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("a", "b")))
  mat2 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("b", "c")))
  mat3 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("d", "e")))

  wov1 <- as.textmodel_word2vec(mat1)
  wov2 <- as.textmodel_word2vec(mat2)
  wov3 <- as.textmodel_word2vec(mat3)

  # no frequency
  wov_nf <- c(wov1, wov2, wov3)

  expect_equal(
    dim(wov_nf$values$word),
    c(5, 6)
  )
  expect_null(
    wov_nf$frequency
  )

  # with frequency
  wov1$frequency <- c("a" = 10, "b" = 5)
  wov2$frequency <- c("b" = 5, "c" = 1)
  wov3$frequency <- c("d" = 3, "e" = 2)
  wov_fq <- c(wov1, wov2, wov3)

  expect_equal(
    dim(wov_fq$values$word),
    c(5, 6)
  )
  expect_equal(
    wov_fq$frequency,
    c("a" = 10, "b" = 10, "c" = 1, "d" = 3, "e" = 2)
  )

  # no centering
  wov_ad <- c(wov1, wov2, wov3, center = TRUE)
  wov_na <- c(wov1, wov2, wov3, center = FALSE)
  expect_false(identical(
    wov_ad$values$word,
    wov_na$values$word
  ))

  # errors
  expect_error(
    c(wov1, wov2, list()),
    "All the objects must be textmodel_word2vec"
  )

})

test_that("c.textmodel_doc2vec works", {

  mat1 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("doc1", "doc2")))
  mat2 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("doc3", "doc4")))
  mat3 <- matrix(rnorm(12), nrow = 2, dimnames = list(c("doc5", "doc6")))

  dov1 <- wordvector::as.textmodel_doc2vec(mat1)
  dov2 <- wordvector::as.textmodel_doc2vec(mat2)
  dov3 <- wordvector::as.textmodel_doc2vec(mat3)

  # no frequency
  dov_nf <- c(dov1, dov2, dov3)

  expect_equal(
    dim(dov_nf$values$doc),
    c(6, 6)
  )
  expect_null(
    dov_nf$frequency
  )

  # with frequency
  dov1$frequency <- c("a" = 10, "b" = 5)
  dov2$frequency <- c("b" = 5, "c" = 1)
  dov3$frequency <- c("d" = 3, "e" = 2)
  dov_fq <- c(dov1, dov2, dov3)

  expect_equal(
    dim(dov_fq$values$doc),
    c(6, 6)
  )
  expect_equal(
    dov_fq$frequency,
    c("a" = 10, "b" = 10, "c" = 1, "d" = 3, "e" = 2)
  )

  # no centering
  dov_ad <- c(dov1, dov2, dov3, center = TRUE)
  dov_na <- c(dov1, dov2, dov3, center = FALSE)
  expect_false(identical(
    dov_ad$values$doc,
    dov_na$values$doc
  ))

  # errors
  expect_error(
    c(dov1, dov2, list()),
    "All the objects must be textmodel_doc2vec"
  )
  expect_error(
    c(dov1, dov2, center = c(TRUE, FALSE)),
    "The length of center must be 1"
  )

})
