library(rjdfilters)
h_list <- c(9, 13, 23)
h_list <- (h_list-1)/2
ic <- 4.5
for( ic in c(3.5, 4.5)){
    for (h in c(6, 11)){
        all_q <- 0:(h-1)
        
        sym_filter <- lp_filter(h, kernel = "Henderson")$filters.coef[,sprintf("q=%i",h)]
        
        rkhs <- readRDS(sprintf("RKHS/rkhs_h%i_p%i.RDS", h, 3))
        res_rkhs <- lapply(rkhs,function(x){
            t(apply(x,2, diagnostic_matrix, lags = h, sweight = sym_filter)[-(1:3),-(h+1)])
        })
        lpp_stats <- lapply(c("LC","QL","CQ","DAF"), function(endpoints){
            a_coef <- lp_filter(h, kernel = "Henderson", endpoints = endpoints,
                                ic = ic)$filters.coef
            t(apply(a_coef,2, diagnostic_matrix, lags = h, sweight = sym_filter)[-(1:3),-(h+1)])
        })
        names(lpp_stats) <- c("LC","QL","CQ","DAF")
        
        all_stats <- lapply(0:3, function(degree){
            res <- lapply(all_q, function(q){
                print(sprintf("q=%i, pdegree = %i", q, degree))
                fst_complete_res <- readRDS(sprintf("comparaison_fst/data/fst_h%i_q%i_pdegree%i.RDS",
                                                    h, q, degree))
                fst_res <- fst_complete_res$results
                weights_fst <- fst_complete_res$weights
                fst_res <- round(fst_res, 4)
                stats <-lapply(c(res_rkhs,
                                 lpp_stats),function(diag_study){
                                     diag_study <- round(diag_study, 4)
                                     tmp <- apply(fst_res, 1,function(fst_w){
                                         c(all(diag_study[sprintf("q=%i",q),]>=fst_w[]),
                                           all(diag_study[sprintf("q=%i",q),1:3]>=fst_w[1:3]),
                                           all(diag_study[sprintf("q=%i",q),-(1:3)]>=fst_w[-(1:3)]),
                                           all(sum(diag_study[sprintf("q=%i",q),-(1:3)])>=sum(fst_w[-(1:3)])))
                                     })
                                     res <- lapply(1:nrow(tmp),function(i) which(tmp[i,]))
                                     res <- lapply(res, function(list_i){
                                         list(diagnostics = fst_res[list_i,],
                                              weights = weights_fst[list_i,],
                                              exists = length(list_i) == 0)
                                     })
                                     names(res) <- c("all", "Guggemos", "Wildi", "MSE")
                                     res
                                 })
                names(stats) <- names(c(res_rkhs,
                                        lpp_stats))
                stats
            })
            names(res) <- sprintf("q=%i", all_q)
            res
        })
        names(all_stats) <- sprintf("d=%i", 0:3)
        
        saveRDS(all_stats,
                file = sprintf("comparaison_fst/data/comparison_fst_h%i_ic%.1f.RDS",
                               h,ic))
    }
    
    
    for (h in c(6, 11)){
        comparaison <- readRDS(sprintf("comparaison_fst/data/comparison_fst_h%i_ic%.1f.RDS", h, ic))
        res <- do.call(rbind, lapply(0:3,function(degree){
            do.call(rbind, lapply(0:(h-1), function(q){
                do.call(rbind, lapply(c("frf", "smoothness", "accuracy", "phase", "LC", "QL", "CQ", "DAF"), function(method){
                    tmp <- comparaison[[sprintf("d=%i", degree)]][[sprintf("q=%i", q)]][[method]][["Guggemos"]]$weights
                    if(is.null(tmp) || (nrow(tmp) == 0))
                        return(NULL)
                    tmp$degree <- degree
                    tmp$q <- q
                    tmp$method <- method
                    
                    tmp
                }))
            }))
        }))
        saveRDS(res, sprintf("comparaison_fst/data/comparison_fst_weights_h%i_ic%.1f.RDS", h, ic))
    }
    
}


# Script pour construire les tables de comparaison rassemblant tous les poids
for (ic in c(3.5, 4.5)){
    for (h in c(6, 11)){
        comparaison <- readRDS(sprintf("comparaison_fst/data/comparison_fst_h%i_ic%.1f.RDS", h, ic))
        res <- do.call(rbind, lapply(0:3,function(degree){
            do.call(rbind, lapply(0:(h-1), function(q){
                do.call(rbind, lapply(c("frf", "smoothness", "accuracy", "phase", "LC", "QL", "CQ", "DAF"), function(method){
                    tmp <- comparaison[[sprintf("d=%i", degree)]][[sprintf("q=%i", q)]][[method]][["Guggemos"]]$weights
                    if(nrow(tmp) == 0)
                        return(NULL)
                    tmp$degree <- degree
                    tmp$q <- q
                    tmp$method <- method
                    
                    tmp
                }))
            }))
        }))
        saveRDS(res, sprintf("comparaison_fst/data/comparison_fst_weights_h%i_ic%.1f.RDS", h, ic))
    }
}

library(dplyr)
for (h in c(6, 11)){
    data <- do.call(rbind, lapply(c(3.5, 4.5), function(ic){
        readRDS(sprintf("comparaison_fst/data/comparison_fst_weights_h%i_ic%.1f.RDS",
                h, ic))
    }))
    data <- data %>% 
        mutate(weight_string = paste(smoothness.weight, timeliness.weight, fidelity.weight,sep = "_")) %>% 
        group_by(method, degree, q) %>% 
        filter(duplicated(weight_string)) %>% 
        ungroup() %>% 
        select(-weight_string)
    saveRDS(data, sprintf("comparaison_fst/data/comparison_fst_weights_h%i_all.RDS", h))
}
