rm(list = ls())
global_tp <- readRDS("comparison_filters/turning_points.RDS")
sort(global_tp$B_AT)
global_tp[[nom_s]]
est_tp_x
extract_tp <- function(nom_f){
    data_tp <- readRDS(nom_f)
    total_tp <- lapply(names(data_tp), function(nom_s){
        global_tp_x <- global_tp[[nom_s]]
        est_tp_x <- data_tp[[nom_s]]
        # is null when no estimation possible (to short time series)
        est_tp_x <- est_tp_x[!sapply(est_tp_x, is.null)]
        first_date <- as.numeric(names(est_tp_x)[1])
        
        total_delay <- sapply(global_tp_x, function(tp_study){
            if(tp_study < first_date){
                NA
            }else{
                est_tp_x_ <- est_tp_x[as.numeric(names(est_tp_x))>=tp_study]
                if(length(est_tp_x_) >0){
                    dates <- which(sapply(est_tp_x_, 
                                          function(x) round(tp_study, 3) %in% round(x,3)))
                }else{
                    dates <- numeric(0)
                }
                if(length(dates) == 0){
                    NA
                }else{
                    tp <- as.numeric(names(dates)[1])
                    round((tp - tp_study)*12)
                }
                
            }
        })
        total_delay <- matrix(total_delay, nrow = 1)
        colnames(total_delay) <- global_tp_x
        total_delay
    })
    names(total_tp) <- names(data_tp)
    total_tp
}


nom_f_x13 <- "comparison_filters/data_x11/turning_points_x13.RDS"
list_f_t_rw <- sprintf("comparison_filters/rkhs/timeliness_bw_rw_tp_%02.f.RDS",1:9)
list_f_t <- sprintf("comparison_filters/rkhs/timeliness_bw_tp_%02.f.RDS",1:9)
list_f_lc <- sprintf("comparison_filters/lp/lp_lc_%02.f_tp.RDS",1:9)
list_f_ql <- sprintf("comparison_filters/lp/lp_ql_%02.f_tp.RDS",1:9)
list_f_cq <- sprintf("comparison_filters/lp/lp_cq_%02.f_tp.RDS",1:9)
list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_tp.RDS",1:9)

data_x13 <- extract_tp(nom_f_x13)
data_x13$B_AT
saveRDS(data_x13, "comparison_filters/turning_points_x13_first.RDS")

data_t <- do.call(c, lapply(list_f_t, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_t, "comparison_filters/turning_points_rkhs_timeliness_first.RDS")

data_t_rw <- do.call(c, lapply(list_f_t_rw, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_t_rw,"comparison_filters/turning_points_rkhs_timeliness_rw_first.RDS")

data_lc <- do.call(c, lapply(list_f_lc, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_lc, "comparison_filters/turning_points_lc_first.RDS")

data_ql <- do.call(c, lapply(list_f_ql, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_ql, "comparison_filters/turning_points_ql_first.RDS")

data_cq <- do.call(c, lapply(list_f_cq, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_cq, "comparison_filters/turning_points_cq_first.RDS")

data_daf <- do.call(c, lapply(list_f_daf, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_daf, "comparison_filters/turning_points_daf_first.RDS")


data_x13 <- readRDS("comparison_filters/turning_points_x13_first.RDS")
data_t <- readRDS("comparison_filters/turning_points_rkhs_timeliness_first.RDS")
data_t_rw <- readRDS("comparison_filters/turning_points_rkhs_timeliness_rw_first.RDS")
data_lc <- readRDS("comparison_filters/turning_points_lc_first.RDS")
data_ql <- readRDS("comparison_filters/turning_points_ql_first.RDS")
data_cq <- readRDS("comparison_filters/turning_points_cq_first.RDS")
data_daf <- readRDS("comparison_filters/turning_points_daf_first.RDS")

data_tot <- data_x13
i_n <- names(data_x13)[1]
i_n <- "B_BA"
for (i_n in names(data_x13)){
    data_tot[[i_n]] <- rbind(data_x13[[i_n]],
                             data_t[[i_n]],
                             data_t_rw[[i_n]],
                             data_lc[[i_n]],
                             data_ql[[i_n]],
                             data_cq[[i_n]],
                             data_daf[[i_n]])
    rownames(data_tot[[i_n]]) <- c("X13",
                                   "RKHS_timeliness",
                                   "RKHS_timeliness_rw",
                                   "LC",
                                   "QL",
                                   "CQ",
                                   "DAF")
    data_tot[[i_n]] <- data_tot[[i_n]][,sort(colnames(data_tot[[i_n]]))]
}
saveRDS(data_tot, file = "comparison_filters/full_turning_points_first.RDS")
