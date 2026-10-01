#' Prepare data for training word embeddings
#'
#' Create tokens objects and mapping files for training aligned word embeddings.
#' @param data a [quanteda::tokens] object.
#' @param lang a language code of the documents in `data`. User-defined code is
#'   accepted because it is used only to create file names for resulting objects.
#' @param dir the path a directory to data and embeddings.
#' @param anchor words used as anchors to align embeddings.
#' @param dir the path to a directory in which models will be saved.
#' @param vocab_size the number of unique types of words in resulting embeddings.
#' @param dim the size of the word vectors.
#' @param min_simil the minimum similarity to anchor words.
#' @param max_anchors the maximum number of anchors for each word.
#' @param compound if `TRUE`, compound multi-word expressions in `data` using
#'   `anchor`. When the concatenation of `data` is "", `quanteda::tokens()`
#'   is applied to `anchor` to detect word boundaries.
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
    if (identical(concat(data), "")) {
      a <- as.list(tokens(anchor, verbose = FALSE))
    } else {
      a <- phrase(anchor)
    }
    data <- tokens_compound(data, a, verbose = FALSE)
  }

  if (concat(data) != " ")
    anchor[] <- stringi::stri_replace_all_fixed(anchor, " ", concat(data))

  # NOTE: consider using tokens_annotate() to insert anchor tags.
  wov <- train_word2vec(data, dim)
  map <- create_map(wov, anchor, vocab_size, max_anchors, min_simil)
  if (nrow(map) == 0)
    stop("Failed in mapping words to anchors")

  attr(map, "k") <- dim
  attr(map, "language") <- lang
  attr(map, "concatenator") <- concat(data)
  #attr(map, "vocab_size") <- vocab_size
  #attr(map, "min_simil") <- min_simil
  attr(map, "version") <- utils::packageVersion("AWE")
  rownames(map) <- NULL

  g <- file.path(dir, paste0("map_", lang, "_k", dim, ".rds"))
  message(msg(" ...mapped %s words to %s anchors (n: %s, sigma: %s)",
              length(unique(map$word)), length(unique(map$anchor)),
              nrow(map), sd(map$weight)))
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

create_map <- function(wov, anchor, vocab_size, max_anchors, min_simil) {

  # cluster words around anchors
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

  map[order(map$anchor, map$freq),]
}
