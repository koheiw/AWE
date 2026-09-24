lang <- c("en", "de", "ja")

data_anchors_topics <- lapply(lang, function(l) {
    v <- unlist(yaml::read_yaml(file.path("inst/anchor", paste0(l, ".yml"))))
    if (l == "en")
      names(v) <- v
    return(v)
})
names(data_anchors_topics) <- lang

usethis::use_data(data_anchors_topics, overwrite = TRUE)
