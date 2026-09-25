#' Prepare data for training word embeddings
#'
#' Create tokens objects and mapping files for training aligned word embeddings.
#' @param data a [quanteda::tokens] object.
#' @param lang a language code of the documents in `data`.
#' @param dir the path a directory to data and embeddings.
#' @param anchor words used as anchors to align embeddings.
#' @param dir the path to a directory in which models will be saved.
#' @param vocab_size the number of unique types of words in resulting embeddings.
#' @param dim the size of the word vectors.
#' @param min_simil the minimum similarity to anchor words.
#' @param max_anchors the maximum number of anchors for each word.
#' @param compound if `TRUE`, compound multi-word expressions in `data` using
#'   `anchor`. When `lang` is one of "zh", "zh_cn", "zh_tw", "ja", `quanteda::tokens()`
#'   is applied to `anchor` to detect boundaries.
#' @returns an invisible path to the resulting mapping file.
#' @export
#' @import quanteda
#' @importFrom utils head
#' @importFrom stats sd
prep_data <- function(data, anchor, lang, dir, dim = 100, vocab_size = 20000,
                      min_simil = 0, max_anchors = 10, compound = TRUE) {

  if (!is.tokens(data))
    stop("data must be a tokens object")
  if (!is.character(anchor) || is.null(names(anchor)))
    stop("anchor must be a named character vector")

  lang <- check_character(lang, min_nchar = 1, max_nchar = 10)
  dim <- check_integer(dim)
  vocab_size <- check_integer(vocab_size)
  min_simil <- check_double(min_simil, min = 0, max = 1)
  max_anchors <- check_integer(max_anchors, min = 1, max = 100)
  compound <- check_logical(compound)

  message(msg("Mapping words to anchors (%s)", lang))

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, paste0("tokens_", lang, "_k", dim, ".rds"))
  if (file.exists(f)) {
    message(msg("Abort (%s already exists)", f))
    return(invisible(f))
  }

  data <- as.tokens_xptr(data)
  if (compound) {
    if (is_cj(lang)) {
      a <- as.list(tokens(anchor, verbose = FALSE))
    } else {
      a <- phrase(anchor)
    }
    data <- tokens_compound(data, a, verbose = FALSE)
  }

  if (concat(data) != " ")
    anchor[] <- stringi::stri_replace_all_fixed(anchor, " ", concat(data))

  # NOTE: consider using tokens_annotate() to insert anchor tags.

  # cluster words around anchors
  wov <- train_word2vec(data, dim)
  a <- anchor[anchor %in% names(wov$frequency)]
  w <- head(names(sort(wov$frequency, decreasing = TRUE)), vocab_size)
  sim <- proxyC::simil(wov$value$word[a,], wov$value$word[w,],
                       rank = max_anchors, min_simil = min_simil)

  # map words to anchors
  tri <- Matrix::mat2triplet(sim)
  map <- data.frame("anchor" = paste0("#", names(a))[tri$i],
                    "word" = colnames(sim)[tri$j],
                    "weight" = tri$x, row.names = NULL)
  map$freq <- wov$frequency[map$word]

  map <- map[order(map$anchor, map$freq),]
  attr(map, "k") <- dim
  attr(map, "language") <- lang
  attr(map, "concatenator") <- concat(data)
  #attr(map, "vocab_size") <- vocab_size
  #attr(map, "min_simil") <- min_simil
  attr(map, "version") <- utils::packageVersion("AWE")
  rownames(map) <- NULL

  g <- file.path(dir, paste0("map_", lang, "_k", dim, ".rds"))
  message(msg(" ...mapped %s words to %s anchors (sigma: %s)",
              length(unique(map$word)), length(unique(map$anchor)),
              sd(map$weight)))
  message(msg(" ...saving map (%s)", g))
  saveRDS(map, g)

  # replace words with anchors
  lis <- lapply(split(map$word, map$anchor), sort)
  toks <- tokens_lookup(data, dictionary(lis), valuetype = "fixed", verbose = FALSE)
  toks <- tokens(toks, concatenator = "", verbose = FALSE) # to combine tokens
  message(msg(" ...saving tokens (%s)", f))
  saveRDS(as.tokens(toks), f)

  return(invisible(g))
}

# get_sigma <- function(x) {
#   sim <- as.matrix(proxyC::simil(x, sparse = FALSE))
#   diag(sim) <- NA
#   sd(sim, na.rm = TRUE)
# }

#' Train aligned word embeddings
#'
#' Train aligned word embeddings using files produced by `prep_data`.
#' @param lang language codes for which aligned models are trained.
#' @param sample the proportion of the corpus used for training.
#' @inheritParams prep_data
#' @export
#' @returns a invisible list of paths to the trained models.
#' @import quanteda
train_models <- function(lang, dir, dim = 100, sample = 0.1) {

  if (!dir.exists(dir))
    stop(dir, " does not exist")
  lang <- check_character(lang, min_len = 1, max_len = 1000, min_nchar = 1, max_nchar = 10)
  dim <- check_integer(dim)
  sample <- check_double(sample, min = 0, max = 1)

  message(msg("Training aligned models (%s)", paste0(lang, collapse = ", ")))
  param <- expand.grid(lang = lang, dim = dim)

  file <- file.path(dir, paste0("word2vec_", param$lang, "_k", param$dim, ".rds"))
  if (length(file) && all(file.exists(file))) {
    message(msg("Abort (%s contains all the models)", dir))
    return(invisible(file))
  }

  # combine all the objects
  file0 <- file.path(dir, paste0("tokens_", param$lang, "_k", param$dim, ".rds"))
  toks0 <- do.call(c, lapply(file0, function(f) {
    if (!file.exists(f))
      stop(msg("Cannot find tokens (%s)", f))
    message(msg(" ...loading data (%s)", f))
    as.tokens_xptr(readRDS(f))
  }))
  toks0 <- tokens_sample(toks0, ndoc(toks0) * sample, verbose = FALSE) # randomize
  wov0 <- train_word2vec(toks0, dim)
  for (i in seq_len(nrow(param))) {
    p <- param[i,]
    f <- file[i]

    # create word vectors from anchors
    map <- readRDS(file.path(dir, paste0("map_", p$lang, "_k", p$dim, ".rds")))
    wov <- wordvector::as.textmodel_word2vec(weight_vector(wov0, map))
    wov$concatenator <- attr(map, "concatenator") # TODO: use dots in as.textmodel_word2vec()
    wov$frequency <- get_freq(map)

    message(msg(" ...saving %s model (%s)", p$lang, f))
    saveRDS(wov, f)
  }
  return(invisible(file))
}

weight_vector <- function(wov, map) {
  map <- map[map$anchor %in% rownames(wov$values$word),]
  w <- wov$values$word
  w <- w / rowSums(abs(w))
  w <- w[map$anchor,] * map$weight
  w <- group_matrix(w, map$word) # sum over anchors
  w <- w / rowSums(abs(w))
  return(w)
}

train_word2vec <- function(x, dim) {
  wordvector::textmodel_word2vec(x, dim,
     verbose = getOption("AWE.word2vec.verbose", TRUE),
     iter = getOption("AWE.word2vec.iter", 10),
     type = getOption("AWE.word2vec.type", "sg")
  )
}

