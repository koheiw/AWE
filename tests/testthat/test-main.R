library(quanteda)
library(wordvector)
library(AWE)
options(wordvector_threads = 2)

corp <- wordvector::data_corpus_news2014
corp_test <- corpus_reshape(corp)

toks_test <- tokens(corp_test, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_numbers = TRUE) |>
             tokens_remove(stopwords("en"), min_nchar = 2) |>
             tokens_subset(min_ntoken = 2)

test_that("combine works", {

  withr::local_options(list(AWE.word2vec.iter = 1))

  d <- tempfile()
  prep_data(toks_test, data_anchors_topics$en, "en", dir = d)
  map1 <- readRDS(file.path(d, "map_en_k100.rds"))
  expect_true(
    "social_media" %in% map1$word
  )

  d <- tempfile()
  prep_data(toks_test, data_anchors_topics$en, "en", dir = d,
            compound = FALSE)
  map2 <- readRDS(file.path(d, "map_en_k100.rds"))
  expect_false(
    "social_media" %in% map2$word
  )

})

