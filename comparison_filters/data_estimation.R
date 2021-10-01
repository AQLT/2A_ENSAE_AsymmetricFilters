# Export de certaines metadata
liste_dates <- 
    do.call(c, 
            lapply(sprintf("comparison_filters/train/part%02.f.RDS",1:9),
                   function(nom_f){
                       data <- readRDS(nom_f)
                       lapply(data, names)
                   }))
names(liste_dates)
saveRDS(liste_dates, "comparison_filters/liste_date_est.RDS")


liste_trend_length <- 
    do.call(c, 
            lapply(sprintf("comparison_filters/train/part%02.f.RDS",1:9),
                   function(nom_f){
                       print(nom_f)
                       data <- readRDS(nom_f)
                       lapply(data, function(x){
                           unlist(sapply(x, `[[`, "decomposition.tlen"))
                       })
                   }))
saveRDS(liste_trend_length, "comparison_filters/trend_length.RDS")

decomposition <- 
    do.call(c, 
            lapply(sprintf("comparison_filters/train/part%02.f.RDS",1:9),
                   function(nom_f){
                       print(nom_f)
                       data <- readRDS(nom_f)
                       lapply(data, function(x){
                           unlist(sapply(x, `[[`, "mode"))
                       })
                   }))
saveRDS(decomposition, "comparison_filters/decomposition.RDS")

