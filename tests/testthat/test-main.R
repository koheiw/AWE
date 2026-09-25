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

test_that("prep_data works", {

  d <- tempfile()

  expect_error(
    prep_data(list(), data_anchors_topics$en, "en", dir = d),
    "data must be a tokens object"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics["en"], "en", dir = d),
    "anchor must be a named character vector"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, c("en", "ja"), dir = d),
    "The length of lang must be 1"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "", dir = d),
    "The value of lang must be between 1 and 10 character"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              dim = "xxx"),
    "dim must be coercible to integer"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              dim = "xxx"),
    "dim must be coercible to integer"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              vocab_size = "xxx"),
    "vocab_size must be coercible to integer"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              min_simil = -1),
    "The value of min_simil must be between 0 and 1"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              max_anchors = 0),
    "The value of max_anchors must be between 1 and 100"
  )

  expect_error(
    prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
              compound = NULL),
    "compound cannot be NULL"
  )

})

test_that("combine works", {

  withr::local_options(list(AWE.word2vec.iter = 1,
                            AWE.word2vec.verbose = FALSE))

  d <- tempfile()
  prep_data(toks_test, dim = 10, data_anchors_topics$en, "en", dir = d)
  map1 <- readRDS(file.path(d, "map_en_k10.rds"))
  expect_true(
    "social media" %in% map1$word
  )

  d <- tempfile()
  prep_data(toks_test, dim = 10, data_anchors_topics$en, "en", dir = d,
            compound = FALSE)
  map2 <- readRDS(file.path(d, "map_en_k10.rds"))
  expect_false(
    "social media" %in% map2$word
  )

})

test_that("prep_data and train_models work", {

  skip_on_cran()

  withr::local_options(list(AWE.word2vec.iter = 1,
                            AWE.word2vec.verbose = FALSE))

  d <- tempfile()

  # prepare
  f <- prep_data(toks_test, data_anchors_topics$en, dim = 10, "en", dir = d)
  map <- readRDS(f)

  expect_equal(
    names(map),
    c("tag", "word", "weight", "freq")
  )
  expect_equal(
    attr(map, "k"),
    10
  )
  expect_equal(
    attr(map, "language"),
    "en"
  )
  expect_equal(
    attr(map, "concatenator"),
    " "
  )
  expect_equal(
    attr(map, "version"),
    utils::packageVersion("AWE")
  )

  # train
  g <- train_models(lang = "en", dir = d, dim = 10)
  wov <- readRDS(g)

  expect_identical(
    class(wov),
    c("textmodel_word2vec", "textmodel_wordvector")
  )
  expect_equal(
    wov$concatenator,
    " "
  )

  expect_error(
    train_models(lang = c("en", "ja"), dir = d),
    "Cannot find tokens"
  )

  expect_error(
    train_models(lang = "", dir = d),
    "The value of lang must be between 1 and 10 character"
  )

  expect_error(
    train_models(lang = c("en", "ja"), dim = "xxx", dir = d),
    "dim must be coercible to integer"
  )

  expect_error(
    train_models(lang = c("en", "ja"), dir = tempfile()),
    "does not exist"
  )
})

