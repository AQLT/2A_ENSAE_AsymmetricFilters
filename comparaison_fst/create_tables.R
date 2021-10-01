## Créer les poids/diagnostiques associés à la méthode FST pour h=6 et h=11
##
library(rjdfilters)
h_list <- c(9, 13, 23)
h_list <- (h_list-1)/2
h <- h_list[3]
all_q <- 0:(h-1)

sym_filter <- lp_filter(h, kernel = "Henderson")$filters.coef[,sprintf("q=%i",h)]

all_fst_res <- function(lags=6, leads=0, pdegree=2, smoothness.weight=1, smoothness.degree=3, 
                        timeliness.weight=0, timeliness.passband=pi/6, timeliness.antiphase=T,
                        resolution=100){
    data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                        timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
    )
    data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
    data <- data[data$fidelity.weight<=1,]
    
    resultat <- t(mapply(function(x,y){
        tryCatch({
            filter <- fst_filter(lags = lags, leads = leads, pdegree=pdegree, 
                                 smoothness.weight=x, smoothness.degree=smoothness.degree,
                                 timeliness.weight=y, timeliness.passband=timeliness.passband, timeliness.antiphase=timeliness.antiphase)
            
            c(filter$criteria,
              mse(sweights = sym_filter, filter$filters.coef,passband = timeliness.passband))
        }, error = function(e) rep(NA,7))
        
    }, data$smoothness.weight, data$timeliness.weight))
    colnames(resultat) <- c("F_g", "S_g", "T_g", "A_w", "S_w", "T_w", "R_w")
    na_res <- apply(is.na(resultat),1,any)
    resultat <- resultat[!na_res,]
    data <- data[!na_res,]
    list(weights = data, results = resultat)
}

all_stats <- lapply(0:3, function(degree){
    lapply(all_q, function(q){
        print(sprintf("q=%i, pdegree = %i", q, degree))
        x <- all_fst_res(leads = q, pdegree = degree,
                         resolution = 200,
                         lags = h)
        saveRDS(x, sprintf("comparaison_fst/data/fst_h%i_q%i_pdegree%i.RDS",
                           h, q, degree))
        NULL
    })
})


rkhs <- readRDS(sprintf("RKHS/rkhs_h%i_p%i.RDS", h, 3))
res_rkhs <- lapply(rkhs,function(x){
    t(apply(x$weight,2, diagnostic_matrix, lags = h, sweight = sym_filter)[-(1:3),-(h+1)])
})
lpp_stats <- lapply(c("LC","QL","CQ","DAF"), function(endpoints){
    a_coef <- lp_filter(h, kernel = "Henderson", endpoints = endpoints)$filters.coef
    t(apply(a_coef,2, diagnostic_matrix, lags = h, sweight = sym_filter)[-(1:3),-(h+1)])
})
names(lpp_stats) <- c("LC","QL","CQ","DAF")

# resolution <- 200
# weights_fst <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
#                     timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
# )
# weights_fst$fidelity.weight <- 1 - (weights_fst$smoothness.weight + weights_fst$timeliness.weight)
# weights_fst <- weights_fst[weights_fst$fidelity.weight<=1,]
all_stats <- do.call(rbind,lapply(0:3, function(degree){
    do.call(rbind,lapply(all_q, function(q){
        print(sprintf("q=%i, pdegree = %i", q, degree))
        fst_res <- readRDS(sprintf("comparaison_fst/data/fst_h%i_q%i_pdegree%i.RDS",
                             h, q, degree))$results
        weights_fst<- readRDS(sprintf("comparaison_fst/data/fst_h%i_q%i_pdegree%i.RDS",
                                      h, q, degree))$weights
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
        names(stats) <- c("all", "Guggemos", "Wildi", "MSE")
        stats
    }))
}))

saveRDS(all_stats,
        file = sprintf("comparaison_fst/data/comparison_fst_h%i.RDS",
                       h))
all_stats <- readRDS(file = "comparaison_fst/data/stats_rkhs_lpp.RDS")
tlpp_stats_lc <- readRDS(file = "comparaison_fst/data/stats_tlpp_lc.RDS")
tlpp_stats_ql <- readRDS(file = "comparaison_fst/data/stats_tlpp_ql.RDS")
all_stats[all_stats$method == "frf",-c(3,5)]
all_stats[all_stats$method == "gain",-c(3,5)]
all_stats[all_stats$method == "phase",-c(3,5)]
all_stats[all_stats$method == "LC",]
all_stats[all_stats$method == "QL",-c(3,5)]
all_stats[all_stats$method == "CQ",-c(3,5)]
all_stats[all_stats$method == "DAF",-c(3,5)]
