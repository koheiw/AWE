#' Prepare data for training word embeddings
#' @param data a [quanteda::tokens] object.
#' @param lang a language code of the documents in `data`.
#' @param dir the path to the data directory.
#' @param anchor words used as anchors to align models.
#' @param dir the path to a directory in which models will be saved.
#' @param dim the size of the word vectors.
#' @param limit the minimum similarity to anchor words.
#' @param n the maximum number of anchors for each word.
#' @returns an invisible path to the resulting data file.
#' @export
#' @import quanteda
prep_data <- function(data, anchor, lang, dir, dim = 100, vocab_size = 20000,
                      min_simil = 0, max_anchors = 10) {

  message(msg("Mapping words to anchors (%s)", lang))

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, paste0("tokens_", lang, "_k", dim, ".rds"))
  if (file.exists(f)) {
    message(msg("Abort (%s already exists)", f))
    return(invisible(f))
  }

  data <- as.tokens_xptr(data)
  data <- tokens_compound(data, phrase(anchor), verbose = FALSE)

  if (concatenator(data) != " ")
    anchor[] <- stringi::stri_replace_all_fixed(anchor, " ", concatenator(data))

  if (is.null(names(anchor)))
    stop("word must be a named vector")

  # cluster words
  wov <- wordvector::textmodel_word2vec(data, dim, type = "sg", verbose = TRUE)
  a <- anchor[anchor %in% names(wov$frequency)]
  w <- head(names(sort(wov$frequency, decreasing = TRUE)), vocab_size)
  sim <- proxyC::simil(wov$value$word[a,], wov$value$word[w,], rank = max_anchors,
                       min_simil = min_simil)

  # link words to tags
  tri <- Matrix::mat2triplet(sim)
  map <- data.frame(tag = paste0("#", names(a))[tri$i],
                    word = colnames(sim)[tri$j],
                    weight = tri$x, row.names = NULL)
  map$freq <- wov$frequency[map$word]

  # limit the size of vocabulary
  # w <- aggregate(weight ~ word, map, max)$weight
  # q <- quantile(w, 1 - pmin(vocab_size / length(w), 1))
  # map <- subset(map, weight > q)

  map <- map[order(map$tag, map$freq),]
  attr(map, "k") <- dim
  attr(map, "lang") <- lang
  rownames(map) <- NULL

  g <- file.path(dir, paste0("map_", lang, "_k", dim, ".rds"))
  message(msg(" ...mapped %s words to %s anchors (sigma: %s)",
              length(unique(map$word)), length(unique(map$tag)),
              sd(map$weight)))
  message(msg(" ...saving map (%s)", g))
  saveRDS(map, g)

  # convert words to tags
  lis <- lapply(split(map$word, map$tag), sort)
  toks <- tokens_lookup(data, dictionary(lis), valuetype = "fixed", verbose = FALSE)
  toks <- tokens(toks, concatenator = "", verbose = FALSE) # to combine tokens

  message(msg(" ...saving tokens (%s)", f))
  saveRDS(as.tokens(toks), f)
  return(invisible(f))
}

# get_sigma <- function(x) {
#   sim <- as.matrix(proxyC::simil(x, sparse = FALSE))
#   diag(sim) <- NA
#   sd(sim, na.rm = TRUE)
# }

#' Train aligned word embeddings
#' All the values should be the same as in `prep_data()`.
#' @param lang language codes for which aligned models are trained.
#' @inheritParams prep_data
#' @export
#' @returns a invisible list of paths to the trained models.
#' @import quanteda
train_models <- function(lang, dir, dim = 100) {

  message(msg("Training aligned models (%s)", paste0(lang, collapse = ", ")))
  param <- expand.grid(lang = lang, dim = dim)

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
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
  toks0 <- tokens_sample(toks0, verbose = FALSE) # randomize
  wov0 <- wordvector::textmodel_word2vec(toks0, dim, type = "sg", verbose = TRUE)

  for (i in seq_len(nrow(param))) {
    p <- param[i,]
    f <- file[i]

    map <- readRDS(file.path(dir, paste0("map_", p$lang, "_k", p$dim, ".rds")))
    map <- subset(map, tag %in% rownames(wov0$values$word))

    m <- wov0$values$word
    m <- m / rowSums(abs(m))
    m <- m[map$tag,] * map$weight
    m <- group_matrix(m, map$word) # sum over tags
    m <- m / rowSums(abs(m))

    message(msg(" ...saving %s model (%s)", p$lang, f))
    wov <- wordvector::as.textmodel_word2vec(m)
    saveRDS(wov, f)
  }
  return(invisible(file))
}

#' Read text fastText or MUSE embedding files
#' @param file the path to the embedding file.
#' @details
#' Files can be downloaded from the [fastText website](https://fasttext.cc/docs/en/aligned-vectors.html).
#' @export
#' @return a dense matrix with word vectors in rows
read_fasttext <- function(file) {
  tmp <- data.table::fread(file, data.table = FALSE, sep = " ", quote = "", skip = 1)
  rownames(tmp) <- tmp[,1]
  as.matrix(tmp[,-1])
}


