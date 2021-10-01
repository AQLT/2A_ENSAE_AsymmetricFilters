library(rjdfilters)
library(zoo)
rkhs_f <- readRDS("comparison_filters/filters/rkhs_rw_p3.RDS")
rkhs_f <- lapply(rkhs_f, `[[`,"phase")
# rkhs_f$`h=6` - rkhs_filter(degree = 3)$filters.coef

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
dir_exp <- "comparison_filters/rkhs/"
i <- 9
for(i in 1:5){
    nom_f <- sprintf("%spart%02.f.RDS",dir_d, i)
    print(nom_f)
    data <- readRDS(nom_f)
    
    j <- 0
    # x_all <- data[[1]]
    series_s <- lapply(data, function(x_all){
        j <<- j+1
        print(j)
        y_all <- tail(x_all,1)[[1]]$preprocessing.model.y_lin
        lapply(names(x_all), function(date_fin){
            x <- x_all[[date_fin]]
            y <- window(y_all, end = as.numeric(date_fin))
            
            decomposition <- x$mode
            mul <- x$mode!="Additive"
            horizon <- (x$decomposition.tlen-1)/2
            if(length(horizon) == 0)
                return (NULL)
            seas.s1 <- paste0("S", toupper(x$decomposition.d9filter))
            filter <- rkhs_f[[sprintf("h=%i", horizon)]]
            tryCatch(x11(y, trend.coefs = filter,
                         mul = mul,
                         seas.s1 = seas.s1)$decomposition,
                     error = function(e) NULL)
        })
    })
    nom_f_s <- sprintf("%stimeliness_bw_%02.f_fixedylin.RDS", dir_exp, i)
    saveRDS(series_s, nom_f_s)
    print("turning points")
    j <- 0
    tp_rkhs <- lapply(series_s, function(series){
        j <<- j+1
        print(j)
        lapply(series, function(x) turning_points(x[,"t"]))
    })
    saveRDS(tp_rkhs,
            sprintf("%stimeliness_bw_tp_%02.f_fixedylin.RDS", dir_exp, i))
}

