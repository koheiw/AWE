data_lexicon_muse <- list()

data_lexicon_muse[["de"]] <- read.delim("inst/lexicon/en-de.txt", sep = " ", header = FALSE,
                                        col.names = c("en", "de"))
data_lexicon_muse[["ja"]] <- read.delim("inst/lexicon/en-ja.txt", sep = " ", header = FALSE,
                                        col.names = c("en", "ja"))

save(data_lexicon_muse, file = "data/data_lexicon_muse.rda")
