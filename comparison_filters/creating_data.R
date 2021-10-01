library(RJDemetra)

liste_indicators <- c("preprocessing.model.y_lin",
                      "preprocessing.model.y_lin_f",
                      "sa","s","t","i", "sa_f","s_f","t_f","i_f", "mode",
                      "decomposition.d9filter", "decomposition.slen",
                      "decomposition.d12filter", "decomposition.tlen",
                      "diagnostics.ic-ratio-henderson", "diagnostics.ic-ratio")
sa_spec <- x13_spec("RSA3")
na.fail(y_)
n_series <- length(data)
i <- 0

# data_sa <- lapply(data, function(x){
#     res <- jx13(x, sa_spec)
# })
# get_indicators(res,"decomposition.d11")
# saveRDS(data_sa, "comparison_filters/sa_full.RDS")

pb <- tkProgressBar("progress bar",
                    sprintf("Série %i/%i",i,n_series),0, n_series, 0)
data_sa <- lapply(data, function(x){
    i <<- i + 1
    date_deb <- start(x)+c(3,0)
    time <- as.numeric(window(time(x),start = date_deb))
    res <- lapply(time,function(end_){
        res <- jx13(window(x,end = end_), sa_spec)
        get_indicators(res, liste_indicators)
    })
    names(res) <- time
    res
})
setTkProgressBar(pb, 200,sprintf("Série %i/%i",i,n_series))

close(pb)
saveRDS(data_sa, "comparison_filters/sa_full.RDS")

print(Sys.time())
data_sa <- lapply(data[1], function(x){
    date_deb <- start(x)+c(3,0)
    time <- as.numeric(window(time(x),start = date_deb))
    res <- lapply(time,function(end_){
        res <- jx13(window(x,end = end_), sa_spec)
        get_indicators(res, liste_indicators)
    })
    names(res) <- time
    res
})
print(Sys.time())

# On ne garde que les informations nécessaires pour faire tourner le modèle
liste_f <- sprintf("comparison_filters/data_x11/sa_full_part%02.f.RDS",1:13)
res_complete <- do.call(c,lapply(liste_f, function(f_nom){
    f <- readRDS(f_nom)
    lapply(f, function(x){
        lapply(x,function(y){
            y[c("preprocessing.model.y_lin",
                "preprocessing.model.y_lin_f","mode",
                "decomposition.d9filter", "decomposition.tlen",
                "diagnostics.ic-ratio-henderson")]
        })
    })
}))
length(res_complete)
saveRDS(res_complete,
        "comparison_filters/train/full_train_data.RDS")

data <- readRDS("comparison_filters/train/full_train_data.RDS")
division <- round(seq(0,length(data), length.out = 10))
for (i in 1:9){
    print(i)
    data_ <- data[(division[i]+1):division[i+1]]
    saveRDS(data_,
            file = sprintf("comparison_filters/train/part%02.f.RDS",i))
}

liste_f <- sprintf("comparison_filters/data_x11/sa_full_part%02.f.RDS",1:13)

# On ne garde que la tendance
trend_complete <- do.call(c,lapply(liste_f, function(f_nom){
    f <- readRDS(f_nom)
    lapply(f, function(x){
        lapply(x,function(y){
            y$t
        })
    })
}))
length(trend_complete)
saveRDS(trend_complete,
        "comparison_filters/data_x11/full_trend_data.RDS")
