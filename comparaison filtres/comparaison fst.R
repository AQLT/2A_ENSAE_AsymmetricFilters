library(rjdfilters)
sym_filter <- filterproperties(6, kernel = "Henderson")$filters.coef[,sprintf("q=%i",6)]

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
      filter <- fstfilter(lags = lags, leads = leads, pdegree=pdegree, 
                          smoothness.weight=x, smoothness.degree=smoothness.degree,
                          timeliness.weight=y, timeliness.passband=timeliness.passband, timeliness.antiphase=timeliness.antiphase)
      
      c(filter$criteria,
        mse(sweights = sym_filter, filter$filter,passband = timeliness.passband))
    }, error = function(e) rep(NA,7))
    
  }, data$smoothness.weight, data$timeliness.weight))
  colnames(resultat) <- c("F_g", "S_g", "T_g", "A_w", "S_w", "T_w", "R_w")
  na_res <- apply(is.na(resultat),1,any)
  resultat <- resultat[!na_res,]
  data <- data[!na_res,]
  list(weights = data, results = resultat)
}
rkhs <- readRDS("FST/rkhs.RDS")
res_rkhs <- lapply(rkhs,function(x){
  t(apply(x$weight,2, diagnostics_matrix, lb = 6, sweight = sym_filter)[-(1:3),-7])
})
lpp_stats <- lapply(c("LC","QL","CQ","DAF"), function(endpoints){
  a_coef <- filterproperties(6, kernel = "Henderson", endpoints = endpoints)$filters.coef
  t(apply(a_coef,2, diagnostics_matrix, lb = 6, sweight = sym_filter)[-(1:3),-7])
})
names(lpp_stats) <- c("LC","QL","CQ","DAF")

all_q <- 0:5
for(q in all_q){
  print(q)
  x <- all_fst_res(leads = q,pdegree = 0)
  x <- na.omit(x)
}
all_stats <- lapply(0:3, function(degree){
  lapply(all_q, function(q){
    print(sprintf("q=%i, pdegree = %i", q, degree))
    x <- all_fst_res(leads = q, pdegree = degree, resolution = 200)
    x <- na.omit(x)
    saveRDS(x, sprintf("comparaison filtres/fst_q%i_pdegree%i.RDS", q, degree))
    NULL
  })
})
all_stats <- do.call(rbind,lapply(0:3, function(degree){
  do.call(rbind,lapply(all_q, function(q){
    print(sprintf("q=%i, pdegree = %i", q, degree))
    x <- readRDS(sprintf("comparaison filtres/fst_q%i_pdegree%i.RDS", q, degree))$results
    x <- round(x,4)
    stats <-t(sapply(c(res_rkhs,
                     lpp_stats),function(t_d){
                      t_d <- round(t_d,4)
      tmp <- apply(x, 1,function(temp){
        c(all(t_d[sprintf("q=%i",q),]>=temp[]),
          all(t_d[sprintf("q=%i",q),1:3]>=temp[1:3]),
          all(t_d[sprintf("q=%i",q),-(1:3)]>=temp[-(1:3)]),
          all(sum(t_d[sprintf("q=%i",q),-(1:3)])>=sum(temp[-(1:3)])))
      })
      apply(tmp,1,any)
    }))
    colnames(stats) <- c("all", "Guggemos", "Wildi", "MSE")
    res <- data.frame(q = q, degree = degree,
                      stats,
                      method = c("frf", "gain", "phase", "LC", "QL", "CQ", "DAF"),
                      stringsAsFactors = FALSE)
    rownames(res) <- NULL
    res
  }))
}))

# saveRDS(all_stats, file = "comparaison filtres/stats_rkhs_lpp.RDS")
all_stats <- readRDS(file = "comparaison filtres/stats_rkhs_lpp.RDS")
tlpp_stats_lc <- readRDS(file = "comparaison filtres/stats_tlpp_lc.RDS")
tlpp_stats_ql <- readRDS(file = "comparaison filtres/stats_tlpp_ql.RDS")
all_stats[all_stats$method == "frf",-c(3,5)]
all_stats[all_stats$method == "gain",-c(3,5)]
all_stats[all_stats$method == "phase",-c(3,5)]
all_stats[all_stats$method == "LC",]
all_stats[all_stats$method == "QL",-c(3,5)]
all_stats[all_stats$method == "CQ",-c(3,5)]
all_stats[all_stats$method == "DAF",-c(3,5)]



tlpp_lc <- readRDS("FST/timeliness_lpp_lc.RDS")
tlpp_ql <- readRDS("FST/timeliness_lpp_ql.RDS")

# On essaye maintenant de voir si il existe un poids alpha_t telle que l'approche FST
# n'arrive pas à donner de meilleurs résultats
tlpp_stats_lc <- do.call(rbind,lapply(0:3, function(degree){
  do.call(rbind,lapply(all_q, function(q){
    print(sprintf("q=%i, pdegree = %i", q, degree))
    x <- readRDS(sprintf("comparaison filtres/fst_q%i_pdegree%i.RDS", q, degree))$results
    x <- round(x,4)
    tlpp_temp <- tlpp_lc[[sprintf("q=%i", q)]]$result[,-(1:3)]
    tmp <- apply(tlpp_temp,1, function(t_d){
      t_d <- round(t_d,4)
      tmp <- apply(x, 1,function(temp){
        c(all(t_d>=temp[]),
          all(t_d[1:3]>=temp[1:3]),
          all(t_d[-(1:3)]>=temp[-(1:3)]),
          all(sum(t_d[-(1:3)])>=sum(temp[-(1:3)])))
      })
      !apply(tmp,1,any)
    })
    res <- apply(tmp,1,any)
    res <- matrix(res,nrow = 1)
    colnames(res) <- c("all", "Guggemos", "Wildi", "MSE")
    data.frame(q = q, degree = degree,
               res,
               stringsAsFactors = FALSE)
  }))
}))
tlpp_stats_ql <- do.call(rbind,lapply(0:3, function(degree){
  do.call(rbind,lapply(all_q, function(q){
    print(sprintf("q=%i, pdegree = %i", q, degree))
    x <- readRDS(sprintf("comparaison filtres/fst_q%i_pdegree%i.RDS", q, degree))$results
    x <- round(x,4)
    tlpp_temp <- tlpp_ql[[sprintf("q=%i", q)]]$result[,-(1:3)]
    res <- apply(apply(tlpp_temp,1, function(t_d){
      t_d <- round(t_d,4)
      tmp <- apply(x, 1,function(temp){
        c(all(t_d>=temp[]),
          all(t_d[1:3]>=temp[1:3]),
          all(t_d[-(1:3)]>=temp[-(1:3)]),
          all(sum(t_d[-(1:3)])>=sum(temp[-(1:3)])))
      })
      !apply(tmp,1,any)
    }),1,any)
    res <- matrix(res,nrow = 1)
    colnames(res) <- c("all", "Guggemos", "Wildi", "MSE")

    data.frame(q = q, degree = degree,
               res,
               stringsAsFactors = FALSE)
  }))
}))
saveRDS(tlpp_stats_lc, file = "comparaison filtres/stats_tlpp_lc.RDS")
saveRDS(tlpp_stats_ql, file = "comparaison filtres/stats_tlpp_ql.RDS")

tlpp_stats
all_stats[all_stats$method == "LC",-c(3,5)]

tmp1 <- all_stats[all_stats$method == "LC",-7]
tmp2 <- tlpp_stats_lc
tmp2[,-(1:2)] <- !tmp2[,-(1:2)]
