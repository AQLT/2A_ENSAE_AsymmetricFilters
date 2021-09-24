library(ggplot2)
library(patchwork)
liste_trend_length <- readRDS("comparison_filters/trend_length.RDS")
liste_decomposition <- readRDS("comparison_filters/decomposition.RDS")
full_rev <- readRDS(file = "comparison_filters/full_revisions_fixedylin.RDS")
find_trend_length <- function(x, date_deb= NULL, date_fin= NULL,
                              is_num = TRUE){
    if(!is.null(date_fin)){
        x <- x[as.numeric(names(x))<date_fin]
    }
    if(!is.null(date_deb)){
        x <- x[as.numeric(names(x))>date_deb]
    }
    res <- names(which.max(table(x)))
    if(is_num)
        res <- as.numeric(res)
    res
}
revision_stat <- function(date_deb= NULL, date_fin= NULL,
                 trend_length = c(9, 13, 23),
                 decomposition = c("Multiplicative","Additive" )){
    
    trend_length_data <- sapply(names(full_rev),function(x){
        find_trend_length(liste_trend_length[[x]], date_deb, date_fin)
    })
    rev_tmp <- full_rev[trend_length_data %in% trend_length]
    
    decomposition_data <- sapply(names(rev_tmp),function(x){
        find_trend_length(liste_decomposition[[x]], date_deb, date_fin,
                          is_num = FALSE)
    })
    rev_tmp <- rev_tmp[decomposition_data %in% decomposition]
    
    rmse <- t(sapply(rev_tmp, function(x){
        x <- window(x, start = date_deb, end = date_fin, extend = TRUE)
        sqrt(apply(x^2,2, mean))
    }))
    rmse <- na.omit(rmse)
    mae <- t(sapply(rev_tmp, function(x){
        x <- window(x, start = date_deb, end = date_fin, extend = TRUE)
        apply(x,2, mean)
    }))
    mae <- na.omit(mae)
    list(RMSE = rmse, MAE = mae)
}

diff_x13 <- function(x){
    apply(x[,-1],2, function(y) y/x[,1])
}
rev_covid <- revision_stat(date_deb = 2020, date_fin = 2020 + 8/12, trend_length = 13)
rev_fc <- revision_stat(date_deb = 2007, date_fin = 2009, trend_length = 13)

table_jsm <- function(x, digits = 2, diff = FALSE){
    print(sprintf("nb_series : %i", nrow(x)))
    x12_name <- "X12-ARIMA"
    x <- x*100
    if(diff){
        x <- diff_x13(x)
        x12_name <- NULL
    }
    data_q <- apply(x,2,quantile,seq(0,1,0.1))
    data_q <- round(rbind(data_q,
                          apply(x, 2, mean)), digits)
    colnames(data_q) <- c(x12_name, "RKHS", "LC", "QL", "CQ", "DAF")
    rownames(data_q) <- c("Min", "D1", "D2", "D3", "D4", "Median", "D6", "D7",
                          "D8", "D9", "Max", "Mean")
    data_q
}
covid_series <- readRDS(file = "JSM/covid_series.RDS")
lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = TRUE)
})
lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = FALSE)
})

saveRDS(lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = TRUE)
}),
file = "JSM/tables_revisions.RDS")
violin_diag <- function(x, n_group = 6, diff = FALSE){
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n_points <- nrow(x)
    n_col <- ncol(x)
    print(n_points)
    if(diff){
        x <- diff_x13(x)
    }
    covid_tp_gg <- reshape2::melt(x)
    colnames(covid_tp_gg) <- c("date","method","y")
    p <- ggplot(covid_tp_gg, aes(method, y, fill = method))
    p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
                    scale  ="count") + 
        theme_bw() + guides(fill = FALSE) + 
        scale_fill_manual(values=gg_color_hue(n_group)[(n_group - n_col + 1):n_group])
}
boxplot_diag <- function(x, n_group = 7, diff = FALSE){
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n_points <- nrow(x)
    n_col <- ncol(x)
    print(n_points)
    if(diff){
        x <- diff_x13(x)
    }""
    covid_tp_gg <- reshape2::melt(x)
    colnames(covid_tp_gg) <- c("date","method","y")
    p <- ggplot(covid_tp_gg, aes(method, y, fill = method))
    p + geom_boxplot() + 
        theme_bw() + guides(fill = FALSE) + 
        scale_fill_manual(values=gg_color_hue(n_group)[(n_group - n_col + 1):n_group])
}
diff_x13 <- function(x){
    apply(x[,-1],2, function(y) y-x[,1])
}
violin_diag(rev_covid[[2]]*100, diff = FALSE)

