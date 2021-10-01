rm(list = ls())

global_tp <- readRDS("comparison_filters/turning_points.RDS")
x = global_tp$B_AT
divide_tp <- function(x, default = "downturn"){
    i_up <- which(diff(x)<0)
    if(length(i_up)==0){
        if(default == "downturn"){
            res <- list(upturn = NULL,
                        downturn = x)
        }else{
            res <- list(upturn = NULL,
                        downturn = x)
        }
        res
        
    }else{
        list(upturn = x[seq_len(i_up)],
             downturn = x[-seq_len(i_up)])
    }
    
}
global_tp <- lapply(global_tp, divide_tp)
time_delay <- function(x){
    i <- length(x)
    remove_i <- NULL
    while (x[i] && i > 0) {
        remove_i <- c(i, remove_i)
        i <- i - 1
    }
    as.numeric(names(x)[remove_i[1]])
}
extract_tp <- function(nom_f){
    data_tp <- readRDS(nom_f)
    total_tp <- lapply(names(data_tp), function(nom_s){
        compute_delay <- function(tp_study, comp){
            if(tp_study < first_date){
                NA
            }else{
                last_date <- time_delay(sapply(est_tp_x, 
                                               function(x){
                                                   ifelse(is.null(x[[comp]]),
                                                          FALSE,
                                                          round(tp_study, 3) %in% round(x[[comp]],3)
                                                   )
                                               } ))
                if(length(last_date) == 0){
                    NA
                }else{
                    (last_date - tp_study)*12
                }
                
            }
        }
        global_tp_x <- global_tp[[nom_s]]
        est_tp_x <- data_tp[[nom_s]]
        # is null when no estimation possible (to short time series)
        est_tp_x <- est_tp_x[!sapply(est_tp_x, is.null)]
        est_tp_x <- lapply(est_tp_x, divide_tp,
                           default = c("upturn", "downturn")[which.min(c(min(global_tp_x$upturn),
                                                                         min(global_tp_x$downturn)))])
        first_date <- as.numeric(names(est_tp_x)[1])
        
        upturn <- sapply(global_tp_x$upturn, compute_delay, comp = "upturn")
        downturn <- sapply(global_tp_x$downturn, compute_delay, comp = "downturn")
        upturn <- matrix(upturn, nrow = 1)
        downturn <- matrix(downturn, nrow = 1)
        colnames(upturn) <- global_tp_x$upturn
        colnames(downturn) <- global_tp_x$downturn
        
        list(upturn = upturn,
             downturn = downturn)
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
saveRDS(data_x13, "comparison_filters/turning_points_x13_du.RDS")

data_t <- do.call(c, lapply(list_f_t, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_t, "comparison_filters/turning_points_rkhs_timeliness_du.RDS")

data_t_rw <- do.call(c, lapply(list_f_t_rw, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_t_rw,"comparison_filters/turning_points_rkhs_timeliness_rw_du.RDS")

data_lc <- do.call(c, lapply(list_f_lc, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_lc, "comparison_filters/turning_points_lc_du.RDS")

data_ql <- do.call(c, lapply(list_f_ql, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_ql, "comparison_filters/turning_points_q_dul.RDS")

data_cq <- do.call(c, lapply(list_f_cq, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_cq, "comparison_filters/turning_points_cq_du.RDS")

data_daf <- do.call(c, lapply(list_f_daf, function(x){
    print(x)
    extract_tp(x)
}))
saveRDS(data_daf, "comparison_filters/turning_points_daf_du.RDS")


data_x13 <- readRDS("comparison_filters/turning_points_x13_du.RDS")
data_t <- readRDS("comparison_filters/turning_points_rkhs_timeliness_du.RDS")
data_t_rw <- readRDS("comparison_filters/turning_points_rkhs_timeliness_rw_du.RDS")
data_lc <- readRDS("comparison_filters/turning_points_lc_du.RDS")
data_ql <- readRDS("comparison_filters/turning_points_ql_du.RDS")
data_cq <- readRDS("comparison_filters/turning_points_cq_du.RDS")
data_daf <- readRDS("comparison_filters/turning_points_daf_du.RDS")

data_tot <- data_x13
i_n <- names(data_x13)[1]
i_n <- "B_BA"
for (i_n in names(data_x13)){
    data_tot[[i_n]]$downturn <- rbind(data_x13[[i_n]]$downturn,
                             data_t[[i_n]]$downturn,
                             data_t_rw[[i_n]]$downturn,
                             data_lc[[i_n]]$downturn,
                             data_ql[[i_n]]$downturn,
                             data_cq[[i_n]]$downturn,
                             data_daf[[i_n]]$downturn)
    data_tot[[i_n]]$upturn <- rbind(data_x13[[i_n]]$upturn,
                                      data_t[[i_n]]$upturn,
                                      data_t_rw[[i_n]]$upturn,
                                      data_lc[[i_n]]$upturn,
                                      data_ql[[i_n]]$upturn,
                                      data_cq[[i_n]]$upturn,
                                      data_daf[[i_n]]$upturn)
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        c("X13",
          "RKHS_timeliness",
          "RKHS_timeliness_rw",
          "LC",
          "QL",
          "CQ",
          "DAF")
}
saveRDS(data_tot, file = "comparison_filters/full_turning_points_du.RDS")


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

extract_tp_first <- function(nom_f){
    data_tp <- readRDS(nom_f)
    total_tp <- lapply(names(data_tp), function(nom_s){
        compute_delay <- function(tp_study, comp){
            if(tp_study < first_date){
                NA
            }else{
                est_tp_x_ <- est_tp_x[as.numeric(names(est_tp_x))>=tp_study]
                if(length(est_tp_x_) >0){
                    dates <- which(sapply(est_tp_x_, 
                                          function(x){
                                              ifelse(is.null(x[[comp]]),
                                                     FALSE,
                                                     round(tp_study, 3) %in% round(x[[comp]],3)
                                              )
                                          } ))
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
        }
        global_tp_x <- global_tp[[nom_s]]
        est_tp_x <- data_tp[[nom_s]]
        # is null when no estimation possible (to short time series)
        est_tp_x <- est_tp_x[!sapply(est_tp_x, is.null)]
        est_tp_x <- lapply(est_tp_x, divide_tp,
                           default = c("upturn", "downturn")[which.min(c(min(global_tp_x$upturn),
                                                                         min(global_tp_x$downturn)))])
        first_date <- as.numeric(names(est_tp_x)[1])
        
        upturn <- sapply(global_tp_x$upturn, compute_delay, comp = "upturn")
        downturn <- sapply(global_tp_x$downturn, compute_delay, comp = "downturn")
        upturn <- matrix(upturn, nrow = 1)
        downturn <- matrix(downturn, nrow = 1)
        colnames(upturn) <- global_tp_x$upturn
        colnames(downturn) <- global_tp_x$downturn
        
        list(upturn = upturn,
             downturn = downturn)
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

data_x13 <- extract_tp_first(nom_f_x13)
saveRDS(data_x13, "comparison_filters/turning_points_x13_first_du.RDS")

data_t <- do.call(c, lapply(list_f_t, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_t, "comparison_filters/turning_points_rkhs_timeliness_first_du.RDS")

data_t_rw <- do.call(c, lapply(list_f_t_rw, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_t_rw,"comparison_filters/turning_points_rkhs_timeliness_rw_first_du.RDS")

data_lc <- do.call(c, lapply(list_f_lc, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_lc, "comparison_filters/turning_points_lc_first_du.RDS")

data_ql <- do.call(c, lapply(list_f_ql, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_ql, "comparison_filters/turning_points_q_first_dul.RDS")

data_cq <- do.call(c, lapply(list_f_cq, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_cq, "comparison_filters/turning_points_cq_first_du.RDS")

data_daf <- do.call(c, lapply(list_f_daf, function(x){
    print(x)
    extract_tp_first(x)
}))
saveRDS(data_daf, "comparison_filters/turning_points_daf_first_du.RDS")


data_x13 <- readRDS("comparison_filters/turning_points_x13_first_du.RDS")
data_t <- readRDS("comparison_filters/turning_points_rkhs_timeliness_first_du.RDS")
data_t_rw <- readRDS("comparison_filters/turning_points_rkhs_timeliness_rw_first_du.RDS")
data_lc <- readRDS("comparison_filters/turning_points_lc_first_du.RDS")
data_ql <- readRDS("comparison_filters/turning_points_ql_first_du.RDS")
data_cq <- readRDS("comparison_filters/turning_points_cq_first_du.RDS")
data_daf <- readRDS("comparison_filters/turning_points_daf_first_du.RDS")

data_tot <- data_x13
i_n <- names(data_x13)[1]
i_n <- "B_BA"
for (i_n in names(data_x13)){
    data_tot[[i_n]]$downturn <- rbind(data_x13[[i_n]]$downturn,
                                      data_t[[i_n]]$downturn,
                                      data_t_rw[[i_n]]$downturn,
                                      data_lc[[i_n]]$downturn,
                                      data_ql[[i_n]]$downturn,
                                      data_cq[[i_n]]$downturn,
                                      data_daf[[i_n]]$downturn)
    data_tot[[i_n]]$upturn <- rbind(data_x13[[i_n]]$upturn,
                                    data_t[[i_n]]$upturn,
                                    data_t_rw[[i_n]]$upturn,
                                    data_lc[[i_n]]$upturn,
                                    data_ql[[i_n]]$upturn,
                                    data_cq[[i_n]]$upturn,
                                    data_daf[[i_n]]$upturn)
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        c("X13",
          "RKHS_timeliness",
          "RKHS_timeliness_rw",
          "LC",
          "QL",
          "CQ",
          "DAF")
}
saveRDS(data_tot, file = "comparison_filters/full_turning_points_first_du.RDS")
data_tot$B_BA
