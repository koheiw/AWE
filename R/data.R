#' Multilingual anchor words for news
#'
#' English topic words extracted from newspaper sections. They are machine
#' translated to other languages.
"data_anchors_topics"

#' MUSE bilingual dictionaries
#'
#' Ground-truth bilingual dictionaries from the Multilingual Unsupervised and
#' Supervised Embeddings (MUSE).
#' @source https://github.com/facebookresearch/MUSE
"data_lexicon_muse"

#' Parallel corpora of news commentary
#'
#' Paragraphs in the corpus are aligned with the English version to allow direct
#' comparison of texts across languages.
#' @details
#' J. Tiedemann (2012) Parallel Data, Tools and Interfaces in OPUS.
#' In Proceedings of the 8th International Conference on Language Resources and
#' Evaluation (LREC 2012)
#' @source http://data.statmt.org/news-commentary/v16/documents.tgz
"data_corpus_commentary_en"

#' @rdname data_corpus_commentary_en
"data_corpus_commentary_de"

#' @rdname data_corpus_commentary_en
"data_corpus_commentary_ja"
