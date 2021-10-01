res <- sapply(1:9, function(i){
    data <- readRDS(sprintf("comparison_filters/data_x11/x13_tp_%02.f_fixedylin.RDS",i))
    res <- rep(i, length(data))
    names(res) <- names(data)
    res
})
res <- unlist(res)
saveRDS(res, file = "comparison_filters/series_to_file.RDS")
