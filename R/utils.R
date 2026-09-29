# copied from quanteda
msg <- function(format, ..., prepend = "", append = "") {
  args <- list(...)
  args <- lapply(args, function(x) {
    if (is.numeric(x)) {
      prettyNum(x, big.mark = ",", digits = 3)
    } else {
      as.character(x)
    }
  })
  args$format <- format
  paste0(prepend, do.call(stringi::stri_sprintf, args), append)
}

# copied from GMTM
group_matrix <- function(x, factor) {

  if (!is.matrix(x))
    stop("x must be a matrix")
  if (length(factor) != nrow(x))
    stop("the length of the factor does not much nrow(x)")

  lis <- split(x, factor, drop = FALSE)
  t(sapply(lis, function(y) {
    if (length(y) == 0)
      y <- rep(0, ncol(x))
    p <- matrix(y, ncol = ncol(x))
    colSums(p, na.rm = TRUE)
  }))

}

# copied from wordvector
is_word2vec <- function(x) {
  identical(class(x), c("textmodel_word2vec", "textmodel_wordvector"))
}

# copied from wordvector
is_doc2vec <- function(x) {
  identical(class(x), c("textmodel_doc2vec", "textmodel_wordvector"))
}


is_cj <- function(lang) {
  lang %in% c("zh", "zh_cn", "zh_tw", "ja")
}

get_freq <- function(x) {
  x <- x[!duplicated(x$word),]
  structure(x$freq, names = x$word)
}

#' Combine aligned word and document embeddings
#'
#' Combine embeddings from multiple models. When models have the same words,
#' their vectors are averaged while frequencies are summed.
#' @param ... [wordvector::textmodel_word2vec] or [wordvector::textmodel_doc2vec]
#'   objects to combine.
#' @return a [wordvector::textmodel_word2vec] or [wordvector::textmodel_doc2vec]
#'   object.
#' @export
#' @method c textmodel_word2vec
c.textmodel_word2vec <- function(..., center = TRUE) {

  lis <- list(...)

  if (!all(sapply(lis, is_word2vec)))
    stop("All the objects must be textmodel_word2vec")
  center <- check_logical(center)

  v <- do.call(rbind, lapply(lis, function(x) {
    x <- as.matrix(x, normalize = FALSE)
    if (center)
      x <- scale(x, center = TRUE, scale = FALSE)
    return(x)
  }))
  v <- group_matrix(v, rownames(v))
  wov <- wordvector::as.textmodel_word2vec(v)

  if (all(sapply(lis, function(x) !is.null(x$frequency)))) {
    f <- do.call(c, lapply(lis, function(x) names(x$frequency)))
    f <- unique(f)
    m <- do.call(cbind, lapply(lis, function(x) x$frequency[f]))
    rownames(m) <- f
    wov$frequency <- rowSums(m, na.rm = TRUE)
  }
  return(wov)
}

#' @rdname c.textmodel_word2vec
#' @param center if `TRUE`, column vectors are adjusted to centered around zero
#'   in each object.
#' @export
#' @method c textmodel_doc2vec
c.textmodel_doc2vec <- function(..., center = TRUE) {

  lis <- list(...)

  if (!all(sapply(lis, is_doc2vec)))
    stop("All the objects must be textmodel_doc2vec")
  center <- check_logical(center)

  v <- do.call(rbind, lapply(lis, function(x) {
    x <- as.matrix(x, normalize = FALSE)
    if (center)
      x <- scale(x, center = TRUE, scale = FALSE)
    return(x)
  }))

  # TODO: replace with wordvector::as.textmodel_doc2vec()
  dov <- as.textmodel_doc2vec(v)

  if (all(sapply(lis, function(x) !is.null(x$frequency)))) {
    f <- do.call(c, lapply(lis, function(x) names(x$frequency)))
    f <- unique(f)
    m <- do.call(cbind, lapply(lis, function(x) x$frequency[f]))
    rownames(m) <- f
    dov$frequency <- rowSums(m, na.rm = TRUE)
  }
  dov$docvar <- do.call(rbind, lapply(lis, function(x) x$docvars))
  return(dov)
}

as.textmodel_doc2vec <- function(x) {
  result <- list(
    "values" = list("doc" = x),
    "weights" = NULL,
    "dim" = ncol(x),
    "frequency" = NULL,
    "tolower" = NULL,
    "concatenator" = "_",
    "docvars" = NULL,
    "normalize" = NULL,
    "call" = try(match.call(sys.function(-1), call = sys.call(-1)), silent = TRUE),
    "version" = utils::packageVersion("wordvector")
  )
  class(result) <- c("textmodel_doc2vec", "textmodel_wordvector")
  return(result)
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

# copy from wordvector
normalize <- function(x) {
  s <- rowSums(abs(x))
  l <- s == 0
  x <- x / (s / ncol(x))
  x[l,] <- 0 # replace NA with zero
  return(x)
}

