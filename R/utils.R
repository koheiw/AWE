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

#' Combine aligned word embeddings
#' @export
c.textmodel_word2vec <- function(...) {

  lis <- list(...)

  if (!all(sapply(lis, is_word2vec)))
    stop("All the objects must be textmodel_word2vec")

  v <- do.call(rbind, lapply(lis, as.matrix))
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



