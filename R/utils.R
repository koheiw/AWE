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

#' Combine aligned word and document embeddings
#'
#' Combine embeddings from multiple models. When models have the same words,
#' their vectors are averaged while frequencies are summed.
#' @param ... [wordvector::textmodel_word2vec] or [wordvector::textmodel_doc2vec]
#'   objects to combine.
#' @return a [wordvector::textmodel_word2vec] or [wordvector::textmodel_doc2vec]
#'   object.
#' @export
#' @method rbind textmodel_word2vec
#' @importFrom wordvector as.textmodel_word2vec is_word2vec
rbind.textmodel_word2vec <- function(..., center = TRUE, scale = TRUE) {

  lis <- list(...)

  if (!all(sapply(lis, is_word2vec)))
    stop("All the objects must be textmodel_word2vec")
  center <- check_logical(center)
  scale <- check_logical(scale)

  v <- do.call(rbind, lapply(lis, function(x) {
    x <- as.matrix(x, normalize = FALSE)
    x <- scale(x, center = center, scale = scale)
    return(x)
  }))
  v <- group_matrix(v, rownames(v))
  v <- normalize(v)
  wov <- as.textmodel_word2vec(v)

  if (all(sapply(lis, function(x) !is.null(x$frequency)))) {
    f <- do.call(c, lapply(lis, function(x) names(x$frequency)))
    f <- unique(f)
    m <- do.call(cbind, lapply(lis, function(x) x$frequency[f]))
    rownames(m) <- f
    wov$frequency <- rowSums(m, na.rm = TRUE)
  }
  return(wov)
}

#' @rdname rbind.textmodel_word2vec
#' @param center,scale `base::scale()` is applied to each object before combining.
#' @export
#' @method rbind textmodel_doc2vec
#' @importFrom wordvector as.textmodel_doc2vec is_doc2vec
rbind.textmodel_doc2vec <- function(..., center = TRUE, scale = TRUE) {

  lis <- list(...)

  if (!all(sapply(lis, is_doc2vec)))
    stop("All the objects must be textmodel_doc2vec")
  center <- check_logical(center)
  scale <- check_logical(scale)

  v <- do.call(rbind, lapply(lis, function(x) {
    x <- as.matrix(x, normalize = FALSE)
    x <- scale(x, center = center, scale = scale)
    return(x)
  }))

  # TODO: replace with wordvector::as.textmodel_doc2vec()
  v <- normalize(v)
  dov <- wordvector::as.textmodel_doc2vec(v)

  if (all(sapply(lis, function(x) !is.null(x$frequency)))) {
    f <- do.call(c, lapply(lis, function(x) names(x$frequency)))
    f <- unique(f)
    m <- do.call(cbind, lapply(lis, function(x) x$frequency[f]))
    rownames(m) <- f
    dov$frequency <- rowSums(m, na.rm = TRUE)
  }
  dov$docvars <- do.call(rbind, lapply(lis, function(x) x$docvars))
  return(dov)
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

#' Find semantically equivalent words across languages
#'
#' @param x a word to translate.
#' @param ... aligned word embedding models.
#' @param n the number of words to be returned.
#' @param source the model from which the word vector of `x` is extracted.
#' @export
translate <- function(x, ..., n = 10, source = 1) {

  lis <- list(...)

  x <- check_character(x)
  source <- check_integer(source, min = 1, max = length(lis))

  if (!all(sapply(lis, wordvector::is_word2vec)))
    stop("All the objects must be textmodel_word2vec")
  if (!x %in% rownames(lis[[source]]$values$word))
    stop('"', x, '" is not found')

  w <- lis[[source]]$values$word[x,,drop = FALSE]
  sapply(lis, function(y) {
    sim <- proxyC::simil(y$values$word, w)
    head(names(sort(Matrix::rowSums(sim), decreasing = TRUE)), n)
  })

}


