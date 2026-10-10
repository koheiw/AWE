#' Train aligned word embeddings
#'
#' Train aligned word embeddings using files produced by `prep_data`.
#' @param lang language codes for which aligned models are trained.
#' @param lang0 language codes on which embeddings are trained. If `lang0` is not
#'   different from `lang`, it is included in the file names.
#' @param sample the proportion of the corpus used for training.
#' @param model a trained word2vec model to update.
#' @inheritParams prep_data
#' @export
#' @returns a invisible list of paths to the trained models.
#' @import quanteda
train_models <- function(lang, dir, dim = 100, sample = 0.1, lang0 = lang, model = NULL) {

  if (!dir.exists(dir))
    stop(dir, " does not exist")

  lang <- check_character(lang, min_len = 1, max_len = 1000, min_nchar = 1, max_nchar = 10)
  lang0 <- check_character(lang0, min_len = 1, max_len = 1000, min_nchar = 1, max_nchar = 10)
  dim <- check_integer(dim)
  sample <- check_double(sample, min = 0, max = 1)

  if (any(duplicated(lang)) || any(duplicated(lang0)))
    stop("The values of lang and lang0 must be unique")

  message(msg("Training embeddings with anchors [%s]...", paste0(lang, collapse = ", ")))
  param <- expand.grid(lang = lang, dim = dim)

  # add lang0 to file names
  if (!setequal(lang, lang0)) {
    suffix <- paste0("_[", paste0(sort(lang0), collapse = "+"), "]")
  } else {
    suffix <- ""
  }

  file <- file.path(dir, paste0("word2vec_", param$lang, "_k", param$dim, suffix, ".rds"))
  if (length(file) && all(file.exists(file))) {
    message(msg(" ...abort (%s contains all the models).", dir))
    message("Finished training embeddings with anchors.")
    return(invisible(file))
  }

  # combine all the objects
  toks <- do.call(c, lapply(lang0, function(l) {
    f <- file.path(dir, paste0("tokens_", l, "_k", dim, ".rds"))
    if (!file.exists(f))
      stop(msg("Cannot find '%s' tokens (%s)", l, f))
    message(msg(" ...loading '%s' tokens (%s)", l, f))
    x <- as.tokens_xptr(readRDS(f))
    x <- tokens_sample(x, ndoc(x) * sample, verbose = FALSE)
    docnames(x) <- paste0(l, "_", docnames(x))
    message(msg(" ......%s documents (%s tokens, %s types)",
                ndoc(x), sum(ntoken(x)), length(types(x))))
    return(x)
  }))
  toks <- tokens_sample(toks, ndoc(toks), verbose = FALSE) # randomize
  if (!is.null(model)) {
    message(msg(" ...initializing word2vec with an existing model"))
    if (dim != model$dim)
      stop("The values of dim must match between the model and data")
  }
  wov <- train_word2vec(toks, dim, model)

  if (getOption("AWE.save.internal", FALSE)) {
    e <- file.path(dir, paste0("word2vec_internal", "_k", dim, suffix, ".rds"))
    saveRDS(wov, e)
  }

  for (i in seq_len(nrow(param))) {
    p <- param[i,]
    f <- file[i]

    # create word vectors from anchors
    m <- readRDS(file.path(dir, paste0("map_", p$lang, "_k", p$dim, ".rds")))
    w <- create_word2vec(wov, m)

    message(msg(" ...saving '%s' model (%s)", p$lang, f))
    saveRDS(w, f)
  }
  message(" ...complete.")
  message("Finished training embeddings with anchors.")
  return(invisible(file))
}

train_word2vec <- function(x, dim, model = NULL) {
  wordvector::textmodel_word2vec(
    x,
    dim,
    tolower = getOption("AWE.word2vec.tolower", TRUE),
    verbose = getOption("AWE.word2vec.verbose", TRUE),
    iter = getOption("AWE.word2vec.iter", 10),
    type = getOption("AWE.word2vec.type", "sg"),
    model = model
  )
}

create_word2vec <- function(x, map) {

  freq <- get_freq(map)
  map <- map[map$anchor %in% rownames(x$values$word),]

  w <- x$values$word
  w <- normalize(w)
  w <- w[map$anchor,] * map$weight
  w <- group_matrix(w, map$word) # sum over anchors
  w <- normalize(w)

  wordvector::as.textmodel_word2vec(
    w,
    frequency = freq,
    concatenator = attr(map, "concatenator"),
    tolower <- getOption("AWE.word2vec.tolower", TRUE)
  )
}

get_freq <- function(x) {
  x <- x[!duplicated(x$word),]
  structure(x$freq, names = x$word)
}

