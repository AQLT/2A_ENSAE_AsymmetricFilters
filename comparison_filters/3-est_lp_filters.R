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
method <- c("LC", "QL", "CQ", "DAF")[2]
kernel = c("Henderson", "Uniform", "Biweight", "Triweight", "Tricube",
           "Gaussian", "Triangular", "Parabolic")[1]
for (method in c("LC", "QL", "CQ", "DAF")){
    i <- 9
    for(i in 9:6){
        nom_f <- sprintf("%spart%02.f.RDS",dir_d, i)
        print(nom_f)
        data <- readRDS(nom_f)
        
        j <- 0
        # x_all <- data[[1]]
        nom_f_kernel <- ifelse(kernel == "Henderson", "",
                               sprintf("_%s", tolower(kernel)))
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
                if(! horizon %in% c(6,11))
                    return (NULL)
                seas.s1 <- paste0("S", toupper(x$decomposition.d9filter))
                ic <- x$`diagnostics.ic-ratio-henderson`
                filter <- lp_filter(horizon = horizon,
                                    kernel = kernel,
                                    endpoints = method, ic = ic)
                tryCatch(x11(y, trend.coefs = filter,
                             mul = mul,
                             seas.s1 = seas.s1)$decomposition,
                         error = function(e) NULL)
            })
        })
        # nom_f_s <- sprintf("%sfst_rkhs_timeliness_%02.f_fixedylin.RDS", dir_exp, i)
        # saveRDS(series_s, nom_f_s)
        nom_f_s <- sprintf("%slp_%s%s_%02i_fixedylin.RDS",
                           dir_exp, tolower(method), nom_f_kernel,i)
        saveRDS(series_s, nom_f_s)
        
        print("turning points")
        j <- 0
        tp_lp <- lapply(series_s, function(series){
            j <<- j+1
            print(j)
            lapply(series, function(x) turning_points(x[,"t"]))
        })
        saveRDS(tp_lp,
                sprintf("%slp_%s%s_tp_%02i_fixedylin.RDS", dir_exp, tolower(method),
                        nom_f_kernel, i))
    }
}
    


