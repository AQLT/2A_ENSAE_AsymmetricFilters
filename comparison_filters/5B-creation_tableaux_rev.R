library(ggplot2)
library(patchwork)
liste_trend_length <- readRDS("comparison_filters/trend_length.RDS")
liste_decomposition <- readRDS("comparison_filters/decomposition.RDS")
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

revision_stat <- function(file = "comparison_filters/revisions/full_rev_fixedylin.RDS",
                          date_deb= NULL, date_fin= NULL,
                          trend_length = c(9, 13, 23),
                          decomposition = c("Multiplicative","Additive" )){
    full_rev <- readRDS(file = file)
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
    apply(x[,-1],2, function(y) y-x[,1])
}
rapport_x13 <- function(x){
    apply(x[,-1],2, function(y) y/x[,1])
}

table_jsm <- function(x, digits = 2, diff = FALSE, rapport = FALSE,
                      quartile = FALSE){
    print(sprintf("nb_series : %i", nrow(x)))
    if(length(grep("X12|X13|X11", colnames(x)))>0){
        col_names <- c("X13-ARIMA",colnames(x)[-1])
    }else{
        col_names <- colnames(x)
    }
    x <- x*100
    if(diff & !rapport){
        x <- diff_x13(x)
        col_names <- col_names[-1]
    }
    if(!diff & rapport){
        x <- rapport_x13(x)
        col_names <- col_names[-1]
    }
    if(quartile){
        by <- 0.25
        def_row <- c("Q1", "Median", "Q3")
    }else{
        by <- 0.1
        def_row <- c("D1", "D2", "D3", "D4", "Median", "D6", "D7",
                     "D8", "D9")
    }
    data_q <- apply(x,2,quantile,seq(0,1,by))
    data_q <- round(rbind(data_q,
                          apply(x, 2, mean)), digits)
    colnames(data_q) <- col_names
    rownames(data_q) <- c("Min", def_row, "Max", "Mean")
    data_q
}


rev_covid <- revision_stat(file = "comparison_filters/revisions/full_rev_fixedylin.RDS",
                           date_deb = 2020, date_fin = 2020 + 8/12, trend_length = 13)
rev_fc <- revision_stat(file = "comparison_filters/revisions/full_rev_fixedylin.RDS",
                        date_deb = 2007, date_fin = 2009, trend_length = 13)

covid_series <- readRDS(file = "comparison_filters/turning_points/covid_series.RDS")
fc_series <- readRDS(file = "comparison_filters/turning_points/fc_series.RDS")

rev_covid_kernel <- revision_stat(file = "comparison_filters/revisions/full_rev_lc_fixedylin.RDS",
                           date_deb = 2020, date_fin = 2020 + 8/12, trend_length = 13)
rev_fc_kernel <- revision_stat(file = "comparison_filters/revisions/full_rev_lc_fixedylin.RDS",
                        date_deb = 2007, date_fin = 2009, trend_length = 13)

covid_series_kernel <- readRDS(file = "comparison_filters/turning_points/covid_kernel_series.RDS")
fc_series_kernel <- readRDS(file = "comparison_filters/turning_points/fc_kernel_series.RDS")


lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = FALSE)
})
lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = TRUE)
})
lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], rapport = TRUE)
})

lapply(rev_fc,function(x){
    table_jsm(x[rownames(x)%in%fc_series,], diff = FALSE)
})
lapply(rev_fc,function(x){
    table_jsm(x[rownames(x)%in%fc_series,], diff = TRUE)
})
lapply(rev_fc,function(x){
    table_jsm(x[rownames(x)%in%fc_series,], rapport = TRUE)
})

lapply(rev_covid_kernel,function(x){
    table_jsm(x[rownames(x)%in%covid_series_kernel,], diff = FALSE)
})
lapply(rev_covid_kernel,function(x){
    table_jsm(x[rownames(x)%in%covid_series_kernel,], diff = TRUE)
})
lapply(rev_covid_kernel,function(x){
    table_jsm(x[rownames(x)%in%covid_series_kernel,], rapport = TRUE)
})

saveRDS(lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = FALSE,quartile = FALSE)
}),
file = "comparison_filters/export/tables_revisions_covid.RDS")

saveRDS(lapply(rev_covid,function(x){
    table_jsm(x[rownames(x)%in%covid_series,], diff = FALSE,quartile = TRUE)
}),
file = "comparison_filters/export/tables_revisions_covid_quartile.RDS")

saveRDS(lapply(rev_covid_kernel,function(x){
    table_jsm(x[rownames(x)%in%covid_series_kernel,], rapport = TRUE,quartile = TRUE)
}),
file = "comparison_filters/export/tables_revisions_covid_kernels_quartile.RDS")

saveRDS(lapply(rev_fc,function(x){
    table_jsm(x[rownames(x)%in%fc_series,], diff = FALSE)
}),
file = "comparison_filters/export/tables_revisions_fc.RDS")


