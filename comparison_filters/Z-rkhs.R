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
    c(upturn(x, start = start_), downturn(x, start = start_))
}
i <- 3
nom_f <- sprintf("comparison_filters/train/part%02.f.RDS",i)
data <- readRDS(nom_f)
j <- 0
series_s <- lapply(data, function(x_all){
    j <<- j+1
    print(j)
    lapply(x_all, function(x){
        y <- x$preprocessing.model.y_lin
        y
        decomposition <- x$mode
        horizon <- (x$decomposition.tlen-1)/2
        seas.s1 <- paste0("S", toupper(x$decomposition.d9filter))
        tryCatch(x11_rkhs(y, horizon = horizon,
                          decomposition = decomposition,
                          seas.s1 = seas.s1,
                          kernel = "BiWeight", degree = 2,
                          density = "uniform", passband = 2*pi/12,
                          asymmetricCriterion = "Timeliness"),
                 error = function(e) NULL)
    })
})
nom_f_s <- sprintf("comparison_filters/rkhs/timeliness_bw_%02.f.RDS",i)
saveRDS(series_s, nom_f_s)
j <- 0
tp_rkhs <- lapply(series_s, function(series){
    j <<- j+1
    print(j)
    lapply(series, function(x) turning_points(x[,"t"]))
})
saveRDS(tp_rkhs,
        sprintf("comparison_filters/rkhs/timeliness_bw_tp_%02.f.RDS",i))


i <- 8
nom_f <- sprintf("comparison_filters/train/part%02.f.RDS",i)
data <- readRDS(nom_f)
j <- 0
series_s <- lapply(data, function(x_all){
    j <<- j+1
    print(j)
    lapply(x_all, function(x){
        y <- x$preprocessing.model.y_lin
        y
        decomposition <- x$mode
        horizon <- (x$decomposition.tlen-1)/2
        seas.s1 <- paste0("S", toupper(x$decomposition.d9filter))
        tryCatch(x11_rkhs(y, horizon = horizon,
                          decomposition = decomposition,
                          seas.s1 = seas.s1,
                          kernel = "BiWeight", degree = 2,
                          density = "rw", passband = 2*pi/12,
                          asymmetricCriterion = "Timeliness"),
                 error = function(e) NULL)
    })
})
nom_f_s <- sprintf("comparison_filters/rkhs/timeliness_bw_rw_%02.f.RDS",i)
saveRDS(series_s, nom_f_s)


j <- 0
tp_rkhs <- lapply(series_s, function(series){
    j <<- j+1
    print(j)
    lapply(series, function(x) turning_points(x[,"t"]))
})
saveRDS(tp_rkhs,
        sprintf("comparison_filters/rkhs/timeliness_bw_rw_tp_%02.f.RDS",i))

    


