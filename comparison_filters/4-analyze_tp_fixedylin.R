# Dans ce fichier on calcule le dephasage pour toutes les méthodes et on rassemble les résultats
# On définit le déphasage comme le nombre de mois nécessaires pour détecter correctement le point
# de retournement sans aucune révision future.
rm(list = ls())


global_tp <- readRDS("comparison_filters/turning_points/turning_points.RDS")
liste_dates <- readRDS("comparison_filters/liste_date_est.RDS")

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
first_date <- function(x){
    valid_dates <- which(x)
    if(length(valid_dates) ==0)
        valid_dates
    as.numeric(as.numeric(names(x[valid_dates[1]])))
}
extract_tp <- function(nom_f, fun = `time_delay`){
    data_tp <- readRDS(nom_f)
    total_tp <- lapply(names(data_tp), function(nom_s){
        compute_delay <- function(tp_study, comp){
            if((tp_study < first_date) | length(est_tp_x) == 0){
                NA
            }else{
                last_date <- fun(sapply(est_tp_x, 
                                               function(x){
                                                   ifelse(is.null(x[[comp]]),
                                                          NA,
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
        # toto <<- nom_s
        global_tp_x <<- global_tp[[nom_s]]
        est_tp_x <- data_tp[[nom_s]]
        names(est_tp_x) <- liste_dates[[nom_s]]
        # is null when no estimation possible (to short time series)
        null_series <- !sapply(est_tp_x,function(x)all(sapply(x,is.null)))
        est_tp_x <- est_tp_x[!sapply(est_tp_x,function(x)all(sapply(x,is.null)))]
        # find sequence 
        if(!all(abs(diff(rev(as.numeric(names(est_tp_x)))))<1/6)){
            remove_until <- which(rev(abs(diff(rev(as.numeric(names(est_tp_x)))))>=1/6))
            remove_until <- tail(remove_until,1)
            est_tp_x <- est_tp_x[-seq_len(remove_until)]
        }
        
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


# nom_f_x13 <- sprintf("comparison_filters/data_x11/x13_tp_%02.f_fixedylin.RDS",1:9)
# list_f_t <- sprintf("comparison_filters/rkhs/timeliness_bw_tp_%02.f_fixedylin.RDS",1:9)
# list_f_lc <- sprintf("comparison_filters/lp/lp_lc_%02.f_tp_fixedylin.RDS",1:9)
# list_f_ql <- sprintf("comparison_filters/lp/lp_ql_%02.f_tp_fixedylin.RDS",1:9)
# list_f_cq <- sprintf("comparison_filters/lp/lp_cq_%02.f_tp_fixedylin.RDS",1:9)
# list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_tp_fixedylin.RDS",1:9)
# list_f_fst_lc <- sprintf("comparison_filters/fst/fst_lc_tp_%02.f_fixedylin.RDS",1:9)
# list_f_fst_rkhs_timeliness <- sprintf("comparison_filters/fst/fst_rkhs_timeliness_tp_%02.f_fixedylin.RDS",1:9)

liste_files = list(X13 = sprintf("comparison_filters/data_x11/x13_tp_%02.f_fixedylin.RDS",1:9),
                   RKHS_timeliness = sprintf("comparison_filters/rkhs/timeliness_bw_tp_%02.f_fixedylin.RDS",1:9),
                   LC = sprintf("comparison_filters/lp/lp_lc_tp_%02.f_fixedylin.RDS",1:9),
                   QL = sprintf("comparison_filters/lp/lp_ql_tp_%02.f_fixedylin.RDS",1:9),
                   CQ = sprintf("comparison_filters/lp/lp_cq_tp_%02.f_fixedylin.RDS",1:9),
                   DAF = sprintf("comparison_filters/lp/lp_daf_tp_%02.f_fixedylin.RDS",1:9),
                   FST_LC = sprintf("comparison_filters/fst/fst_lc_tp_%02.f_fixedylin.RDS",1:9),
                   FST_LC_min = sprintf("comparison_filters/fst/fst_lc_min_tp_%02.f_fixedylin.RDS",1:9),
                   FST_LC_med = sprintf("comparison_filters/fst/fst_lc_med_tp_%02.f_fixedylin.RDS",1:9),
                   FST_RKHS_timeliness = sprintf("comparison_filters/fst/fst_rkhs_timeliness_tp_%02.f_fixedylin.RDS",1:9)
)
sapply(liste_files,file.exists)
saveRDS(liste_files,
        file = "comparison_filters/turning_points/liste_files.RDS")

for(i in names(liste_files)){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        extract_tp(x)
    }))
    saveRDS(data,
            sprintf("comparison_filters/turning_points/turning_points_%s_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/turning_points/turning_points_%s_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]]$downturn <- do.call(rbind, lapply(data_i, `[[`,"downturn"))
    data_tot[[i_n]]$upturn <- do.call(rbind, lapply(data_i, `[[`,"upturn"))
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/turning_points/full_turning_points_fixedylin.RDS")

################
## First date ##
################

for(i in names(liste_files)){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        extract_tp(x,`first_date`)
    }))
    saveRDS(data,
            sprintf("comparison_filters/turning_points/turning_points_%s_first_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/turning_points/turning_points_%s_first_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]]$downturn <- do.call(rbind, lapply(data_i, `[[`,"downturn"))
    data_tot[[i_n]]$upturn <- do.call(rbind, lapply(data_i, `[[`,"upturn"))
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/turning_points/full_turning_points_first_fixedylin.RDS")


##############################
## local polynomial kernels ##
##############################

liste_files = list(
    Henderson = sprintf("comparison_filters/lp/lp_lc_tp_%02.f_fixedylin.RDS",1:9),
    Biweight = sprintf("comparison_filters/lp/kernel_changes/lp_lc_biweight_tp_%02.f_fixedylin.RDS",1:9),
    Epanechnikov = sprintf("comparison_filters/lp/kernel_changes/lp_lc_parabolic_tp_%02.f_fixedylin.RDS",1:9),
    Triangular = sprintf("comparison_filters/lp/kernel_changes/lp_lc_triangular_tp_%02.f_fixedylin.RDS",1:9),
    Tricube = sprintf("comparison_filters/lp/kernel_changes/lp_lc_tricube_tp_%02.f_fixedylin.RDS",1:9),
    Triweight = sprintf("comparison_filters/lp/kernel_changes/lp_lc_triweight_tp_%02.f_fixedylin.RDS",1:9),
    Uniform = sprintf("comparison_filters/lp/kernel_changes/lp_lc_uniform_tp_%02.f_fixedylin.RDS",1:9)
)
sapply(liste_files,file.exists)
saveRDS(liste_files,
        file = "comparison_filters/turning_points/liste_files_lp_kernels.RDS")
for(i in names(liste_files)[1:3]){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        extract_tp(x,`first_date`)
    }))
    saveRDS(data,
            sprintf("comparison_filters/turning_points/turning_points_lc_%s_first_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/turning_points/turning_points_lc_%s_first_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]]$downturn <- do.call(rbind, lapply(data_i, `[[`,"downturn"))
    data_tot[[i_n]]$upturn <- do.call(rbind, lapply(data_i, `[[`,"upturn"))
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/turning_points/full_turning_points_lc_first_fixedylin.RDS")



for(i in names(liste_files)){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        extract_tp(x)
    }))
    saveRDS(data,
            sprintf("comparison_filters/turning_points/turning_points_lc_%s_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/turning_points/turning_points_lc_%s_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]]$downturn <- do.call(rbind, lapply(data_i, `[[`,"downturn"))
    data_tot[[i_n]]$upturn <- do.call(rbind, lapply(data_i, `[[`,"upturn"))
    rownames(data_tot[[i_n]]$downturn) <- 
        rownames(data_tot[[i_n]]$upturn) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/turning_points/full_turning_points_lc_fixedylin.RDS")


# readRDS("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS")[[1]]
# data_x13 <- do.call(c, lapply(nom_f_x13, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_x13, "comparison_filters/turning_points/turning_points_x13_du_fixedylin.RDS")
# 
# data_t <- do.call(c, lapply(list_f_t, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_t, "comparison_filters/turning_points/turning_points_rkhs_timeliness_du_fixedylin.RDS")
# 
# data_lc <- do.call(c, lapply(list_f_lc, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_lc, "comparison_filters/turning_points/turning_points_lc_du_fixedylin.RDS")
# 
# data_ql <- do.call(c, lapply(list_f_ql, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_ql, "comparison_filters/turning_points/turning_points_ql_du_fixedylin.RDS")
# 
# data_cq <- do.call(c, lapply(list_f_cq, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_cq, "comparison_filters/turning_points/turning_points_cq_du_fixedylin.RDS")
# 
# data_daf <- do.call(c, lapply(list_f_daf, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_daf, "comparison_filters/turning_points/turning_points_daf_du_fixedylin.RDS")
# 
# data_fst_lc <- do.call(c, lapply(list_f_fst_lc, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_fst_lc, "comparison_filters/turning_points/turning_points_fst_lc_du_fixedylin.RDS")
# 
# data_fst_rkhs_timeliness <- do.call(c, lapply(list_f_fst_rkhs_timeliness, function(x){
#     print(x)
#     extract_tp(x)
# }))
# saveRDS(data_fst_rkhs_timeliness, "comparison_filters/turning_points/turning_points_fst_lc_du_fixedylin.RDS")
# 
# data_tot[[i_n]]$upturn <- rbind(data_x13[[i_n]]$upturn,
#                                 data_t[[i_n]]$upturn,
#                                 data_lc[[i_n]]$upturn,
#                                 data_ql[[i_n]]$upturn,
#                                 data_cq[[i_n]]$upturn,
#                                 data_daf[[i_n]]$upturn,
#                                 data_fst_lc[[i_n]]$upturn,
#                                 data_fst_rkhs_timeliness[[i_n]]$upturn)
# data_tot[[i_n]]$downturn <- rbind(data_x13[[i_n]]$downturn,
#                                   data_t[[i_n]]$downturn,
#                                   data_lc[[i_n]]$downturn,
#                                   data_ql[[i_n]]$downturn,
#                                   data_cq[[i_n]]$downturn,
#                                   data_daf[[i_n]]$downturn,
#                                   data_fst_lc[[i_n]]$downturn,
#                                   data_fst_rkhs_timeliness[[i_n]]$downturn)
# data_tot[[i_n]]$upturn <- rbind(data_x13[[i_n]]$upturn,
#                                 data_t[[i_n]]$upturn,
#                                 data_lc[[i_n]]$upturn,
#                                 data_ql[[i_n]]$upturn,
#                                 data_cq[[i_n]]$upturn,
#                                 data_daf[[i_n]]$upturn,
#                                 data_fst_lc[[i_n]]$upturn,
#                                 data_fst_rkhs_timeliness[[i_n]]$upturn)