library(quanteda)
library(AWE)
options(wordvector_threads = 2)

toks_en <- tokens(data_corpus_commentary_en) |>
  tokens_select("^\\p{Latin}+$", valuetype = "regex")
toks_de <- tokens(data_corpus_commentary_de) |>
  tokens_select("^\\p{Latin}+$", valuetype = "regex")
toks_ja <- tokens(data_corpus_commentary_ja) |>
  tokens_select("^[\\p{Han}\\p{Kana}\\p{Hira}]+$", valuetype = "regex")

d <- tempfile()

test_that("prep_data works", {

  withr::local_options(list(AWE.word2vec.iter = 1,
                            AWE.word2vec.verbose = FALSE))

  # en
  expect_message(
    f_en <- prep_data(toks_en, data_anchors_topics$en, lang = "en", dir = d),
    "Mapping words to anchors (en)", fixed = TRUE
  )
  expect_match(
    f_en,
    "tokens_en_k100.rds", fixed = TRUE
  )
  expect_message(
    prep_data(toks_en, data_anchors_topics$en, lang = "en", dir = d),
    paste0("Abort (", f_en[1], " already exists)"), fixed = TRUE
  )

  # de
  expect_message(
    f_de <- prep_data(toks_de, data_anchors_topics$de, lang = "de", dir = d),
    "Mapping words to anchors (de)", fixed = TRUE
  )
  expect_match(
    f_de,
    "tokens_de_k100.rds", fixed = TRUE
  )
  expect_message(
    prep_data(toks_de, data_anchors_topics$de, lang = "de", dir = d),
    paste0("Abort (", f_de[1], " already exists)"), fixed = TRUE
  )

  # ja
  expect_message(
    f_ja <- prep_data(toks_ja, data_anchors_topics$ja, lang = "ja", dir = d),
    "Mapping words to anchors (ja)", fixed = TRUE
  )
  expect_match(
    f_ja,
    "tokens_ja_k100.rds", fixed = TRUE
  )
  expect_message(
    prep_data(toks_ja, data_anchors_topics$ja, lang = "ja", dir = d),
    paste0("Abort (", f_ja, " already exists)"), fixed = TRUE
  )

})

test_that("train_models works", {

  withr::local_options(list(AWE.word2vec.iter = 1,
                            AWE.word2vec.verbose = FALSE))

  expect_message(
    f1 <- train_models(c("en", "de", "ja"), dir = d),
    "Training embeddings with anchors (en, de, ja)", fixed = TRUE
  )
  expect_message(
    train_models(c("en", "de", "ja"), dir = d),
    paste0("Abort (", d, " contains all the models)"), fixed = TRUE
  )
  expect_match(
    f1,
    "word2vec_(en|de|ja)_k100.rds",
  )

  # use lang0
  expect_message(
    f2 <- train_models(c("en", "de", "ja"), dir = d, lang0 = c("en", "de")),
    "Training embeddings with anchors (en, de, ja)", fixed = TRUE
  )
  expect_match(
    f2,
    "word2vec_(en|de|ja)_k100_\\[de\\+en\\].rds",
  )
  expect_message(
    train_models(c("en", "de", "ja"), dir = d, lang0 = c("en", "de")),
    paste0("Abort (", d, " contains all the models)"), fixed = TRUE
  )

})

