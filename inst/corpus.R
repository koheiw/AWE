library(stringi)
library(quanteda)

copy <- function(file, lang) {
  all(sapply(file, function(f) {
    from <- file.path("D:/Research/Multilingual/raw/documents/split", lang, f)
    to <- file.path("inst/corpus", lang, f)
    file.copy(from ,to)
  }))
}

verify <- function(file, lang) {
  sapply(file, function(f) {
    txt <- readLines(file.path(file.path("inst/corpus", lang, f)))
    txt <- paste0(txt, collapse = " ")
    length(unlist(stringi::stri_split_fixed(txt, "<P>")))
  })
}

load <- function(file, lang) {
  lis <- lapply(file, function(f) {
    txt <- readLines(file.path(file.path("inst/corpus", lang, f)))
    txt <- paste0(txt, collapse = "\n")

    title <- stri_match_first_regex(txt, "<HEADLINE>(.*?)<P>", dot_all = TRUE)[1,2]
    body <- stri_match_first_regex(txt, "<P>(.*)<P>", dot_all = TRUE)[1,2]
    body <- unlist(stri_split_fixed(body, "<P>"))
    body <- stri_trans_nfkc(body)
    body <- stri_trim(body)

    data.frame(title = stri_trim(title),
               text = stri_trim(body),
               doc_id = paste0(lang, "_", stri_replace_last_fixed(f, ".txt", "")))
  })
  dat <- do.call(rbind, lis)
  corpus(dat, unique_docnames = FALSE)
}

file_en <- list.files("D:/Research/Multilingual/raw/documents/split/en")
file_de <- list.files("D:/Research/Multilingual/raw/documents/split/de")
file_ja <- list.files("D:/Research/Multilingual/raw/documents/split/ja")

file <- unique(c(file_en, file_de, file_ja))
file <- intersect(file, file_en)
file <- intersect(file, file_de)
file <- intersect(file, file_ja)

# copy common files
# copy(file, "en")
# copy(file, "de")
# copy(file, "ja")

# compare lengths
len_en <- verify(file, "en")
len_de <- verify(file, "de")
len_ja <- verify(file, "ja")

identical(len_en, len_de)
identical(len_en, len_ja)

# make corpus
data_corpus_commentary_en <- load(file, "en")
data_corpus_commentary_de <- load(file, "de")
data_corpus_commentary_ja <- load(file, "ja")

# compare docnames
identical(stri_sub(docnames(data_corpus_commentary_en), 4, -1),
          stri_sub(docnames(data_corpus_commentary_de), 4, -1))
identical(stri_sub(docnames(data_corpus_commentary_en), 4, -1),
          stri_sub(docnames(data_corpus_commentary_ja), 4, -1))

usethis::use_data(data_corpus_commentary_en, overwrite = TRUE)
usethis::use_data(data_corpus_commentary_de, overwrite = TRUE)
usethis::use_data(data_corpus_commentary_ja, overwrite = TRUE)
