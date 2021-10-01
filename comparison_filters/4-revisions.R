rm(list = ls())

nom_f <- "comparison_filters/data_x11/x13_01_fixedylin.RDS"
nom_s <- "B_AT"
compute_revisions <- function(nom_f){
    data <- readRDS(nom_f)
    total_tp <- lapply(names(data), function(nom_s){
        est_tp_x <<- data[[nom_s]]
        while(length(est_tp_x) >0 && is.null(est_tp_x[[length(est_tp_x)]])){
            est_tp_x <- est_tp_x[-length(est_tp_x)]
        }
        while(length(est_tp_x) >0 &&is.null(est_tp_x[[1]])){
            est_tp_x <- est_tp_x[-1]
        }
        if(length(est_tp_x) == 0)
            return(NA)
        last_est <- est_tp_x[[length(est_tp_x)]]
        first_est <- tail(time(est_tp_x[[1]]),1)
        est <- est_tp_x[[1]]
        tail(est-last_est,1)
        revisions <- t(sapply(est_tp_x, function(est){
            if(is.null(est)){
                return(rep(NA, ncol(last_est)))
            }
            tail(abs((est-last_est)/last_est),1)
        }))
        revisions <- ts(revisions, start = first_est, frequency = 12)
        colnames(revisions) <- colnames(last_est)
        revisions
    })
    names(total_tp) <- names(data)
    total_tp
}

liste_files <- readRDS(file = "comparison_filters/turning_points/liste_files.RDS")
liste_files <- lapply(liste_files,gsub, pattern ="_tp",replacement = "")

for(i in names(liste_files)){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        compute_revisions(x)
    }))
    saveRDS(data,
            sprintf("comparison_filters/revisions/rev_%s_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/revisions/rev_%s_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]] <- do.call(ts.union, lapply(data_i, function(x){
        if(all(is.na(x))){
            NA
        }else{
            x[,"t"]
        }
    }))
    colnames(data_tot[[i_n]]) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/revisions/full_rev_fixedylin.RDS")


##############################
## local polynomial kernels ##
##############################

liste_files <- readRDS(file = "comparison_filters/turning_points/liste_files_lp_kernels.RDS")
liste_files <- lapply(liste_files,gsub, pattern ="_tp",replacement = "")

for(i in names(liste_files)){
    data <- do.call(c, lapply(liste_files[[i]], function(x){
        print(x)
        compute_revisions(x)
    }))
    saveRDS(data,
            sprintf("comparison_filters/revisions/rev_%s_fixedylin.RDS",
                    tolower(i)))
}

all_data <- lapply(names(liste_files),
                   function(x){
                       name <- sprintf("comparison_filters/revisions/rev_lc_%s_fixedylin.RDS",
                                       tolower(x))
                       print(name)
                       readRDS(name) 
                   } )
names(all_data) <- names(liste_files)
data_tot <- all_data[[1]]
i_n <- "B_BA"
for (i_n in names(data_tot)){
    data_i <- lapply(all_data, `[[`, i_n)
    
    data_tot[[i_n]] <- do.call(ts.union, lapply(data_i, function(x){
        if(all(is.na(x))){
            NA
        }else{
            x[,"t"]
        }
    }))
    colnames(data_tot[[i_n]]) <- 
        names(data_i)
}
saveRDS(data_tot, file = "comparison_filters/revisions/full_rev_lc_fixedylin.RDS")

