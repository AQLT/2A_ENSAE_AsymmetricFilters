library(rjdfilters)
library(zoo)
library(RJDemetra)
upturn <- function(x, start_ = start(x)){
    if(is.null(x))
        return(NULL)
    res <- rollapply(x, width=5,
                     function(x){
                         (x[1]>=x[2]) & (x[2]>=x[3]) &
                             (x[3]<x[4]) & (x[4]<=x[5])
                     })
    res <- window(res, start = start_, extend = TRUE)
    res <- time(res)[which(res)]
    res
}
downturn <- function(x, start_ = start(x)){
    if(is.null(x))
        return(NULL)
    res <- rollapply(x, width=5,
                     function(x){
                         (x[1]<=x[2]) & (x[2]<=x[3]) &
                             (x[3]>x[4]) & (x[4]>=x[5])
                     })
    res <- window(res, start = start_, extend = TRUE)
    res <- time(res)[which(res)]
    res
}
turning_points <- function(x, start_ = start(x)){
    if(is.null(x))
        return(NULL)
    list(upturn = upturn(x, start = start_),
         downturn = downturn(x, start = start_))
}
dir_d <- "comparison_filters/train/"
dir_exp <- "comparison_filters/lp/"
i <- 1
nom_f <- sprintf("%spart%02.f.RDS", dir_d, i)
data <- readRDS(nom_f)
j <- 0

series_s <- lapply(data, function(x_all){
    j <<- j+1
    print(j)
    y_all <- tail(x_all,1)[[1]]$preprocessing.model.y_lin
    lapply(names(x_all), function(date_fin){
        x <- x_all[[date_fin]]
        y <- window(y_all, end = as.numeric(date_fin))
        
        tryCatch({
            spec <- x13_spec("RSA0",
                             automdl.enabled = TRUE,
                             x11.trendAuto = FALSE,
                             x11.trendma = x$decomposition.tlen,
                             x11.seasonalma = paste0("S", toupper(x$decomposition.d9filter)),
                             transform.function = ifelse(x$mode== "Additive",
                                                         "None", "Log"),
            )
            jmod <- jx13(y, spec)
            do.call(cbind, get_indicators(jmod, c("sa", "t", "s", "i")))
        },
                 error = function(e) NULL)
    })
})
nom_f_s <- sprintf("%sx13_%02i_fixedylin.RDS", dir_exp,i)
saveRDS(series_s, nom_f_s)

j <- 0
tp_lp <- lapply(series_s, function(series){
    j <<- j+1
    print(j)
    lapply(series, function(x) turning_points(x[,"t"]))
})
saveRDS(tp_lp,
        sprintf("%sx13_tp_%02i_fixedylin.RDS", dir_exp, i))
