library(quanteda)
library(wordvector)
library(AWE)
options(wordvector_threads = 2)

corp <- wordvector::data_corpus_news2014
corp_test <- corpus_reshape(corp)

toks_test <- tokens(corp_test, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_numbers = TRUE,
                    concatenator = " ") |>
             tokens_remove(stopwords("en"), min_nchar = 2) |>
             tokens_subset(min_ntoken = 2)

test_that("c works", {

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

  # errors
  expect_error(
    c(wov1, wov2, list()),
    "All the objects must be textmodel_word2vec"
  )

})
