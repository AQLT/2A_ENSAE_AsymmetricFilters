## On estime les points de retournements au niveau global
library(zoo)
y <- readRDS("comparison_filters/eurostat_db.RDS")
data <- readRDS("comparison_filters/sa_full.RDS")
data_tmp <- readRDS("comparison_filters/train/part01.RDS")
data[[i_n]]$t- trend_complete[[i_n]]$`2021.25`
data_tmp[[i_n]]$`2021.25`$preprocessing.model.y_lin

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
tp <- lapply(data, function(d){
    turning_points(d$t)
} )
saveRDS(tp, "comparison_filters/turning_points.RDS")

# tp <- readRDS("comparison_filters/turning_points.RDS")
# trend_complete <- readRDS("comparison_filters/data_x11/full_trend_data.RDS")
# series = trend_complete$B_AT
# i <- 0
# tp_x13 <- lapply(trend_complete, function(series){
#     i <<- i+1
#     print(i)
#     lapply(series, turning_points)
# })
# saveRDS(tp_x13, "comparison_filters/data_x11/turning_points_x13.RDS")

