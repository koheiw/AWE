lang <- c("en", "de", "ja")

lis <- lapply(lang, function(l) {
    v <- unlist(yaml::read_yaml(file.path("inst/anchor", paste0(l, ".yml"))))
    if (l == "en")
      names(v) <- v
    return(v)
})
names(lis) <- lang

data_anchors_topics <- lis
save(data_anchors_topics, file = "data/data_anchors_topics.rda")
