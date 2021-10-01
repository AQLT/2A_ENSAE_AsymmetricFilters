library(rjdfilters)
library(zoo)
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
method <- c("LC", "QL", "CQ", "CC", "DAF", "CN")[2]
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
        
        decomposition <- x$mode
        horizon <- (x$decomposition.tlen-1)/2
        ic <- x$`diagnostics.ic-ratio-henderson`
        seas.s1 <- paste0("S", toupper(x$decomposition.d9filter))
        tryCatch(x11_lp(y, horizon = horizon,
                        endpoints = method,
                        decomposition = decomposition,
                        seas.s1 = seas.s1,
                        kernel = "Henderson", degree = 2,
                        ic = ic, passband = 2*pi/12, tweight = 0),
                 error = function(e) NULL)
    })
})
nom_f_s <- sprintf("%slp_%s_%02i.RDS", dir_exp, tolower(method),i)
saveRDS(series_s, nom_f_s)


j <- 0
tp_lp <- lapply(series_s, function(series){
    j <<- j+1
    print(j)
    lapply(series, function(x) turning_points(x[,"t"]))
})
saveRDS(tp_lp,
        sprintf("%slp_%s_tp_%02i.RDS", dir_exp, tolower(method), i))


dir_d <- "comparison_filters/train/"
dir_exp <- "comparison_filters/lp/"
i <- 7
method <- c("LC", "QL", "CQ", "CC", "DAF", "CN")[2]
nom_f <- sprintf("%spart%02.f.RDS", dir_d, i)
series_s <- readRDS(nom_f_s)
j <- 0


