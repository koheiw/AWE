lang <- c("en", "de", "ja", "zh_cn", "id", "ms", "vi", "ar", "fa", "he")

data_anchors_topics <- lapply(lang, function(l) {
    v <- unlist(yaml::read_yaml(file.path("inst/anchor", paste0(l, ".yml"))))
    if (l == "en")
      names(v) <- v
    return(v)
})
names(data_anchors_topics) <- lang

b <- sapply(data_anchors_topics, function(x) {
            identical(names(data_anchors_topics$en),
                      names(x))
            })
stopifnot(all(b))

usethis::use_data(data_anchors_topics, overwrite = TRUE)
