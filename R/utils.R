# copied from quanteda
msg <- function(format, ..., prepend = "", append = "") {
  args <- list(...)
  args <- lapply(args, function(x) {
    if (is.numeric(x)) {
      prettyNum(x, big.mark = ",", digit = 3)
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


is_cj <- function(lang) {
  lang %in% c("zh", "zh_cn", "zh_tw", "ja")
}
