## Créer les poids/diagnostiques associés à la méthode FST pour h=6 et h=11
##
library(rjdfilters)
h_list <- c(9, 13, 23)
h_list <- (h_list-1)/2
h <- h_list[2]
for (h in c(6,11)){
    sym_filter <- lp_filter(h, kernel = "Henderson")$filters.coef[,sprintf("q=%i",h)]
    
    all_fst_res <- function(lags=6, leads=0, pdegree=2, smoothness.weight=1, smoothness.degree=3, 
                            timeliness.weight=0, timeliness.passband=pi/6, timeliness.antiphase=T,
                            resolution=100){
        data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                            timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
        )
        data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
        data <- data[(data$fidelity.weight<=1) & data$fidelity.weight>=0,]
        
        resultat <- t(mapply(function(x,y){
            tryCatch({
                filter <- fst_filter(lags = lags, leads = leads, pdegree=pdegree, 
                                     smoothness.weight=x, smoothness.degree=smoothness.degree,
                                     timeliness.weight=y,
                                     timeliness.passband=timeliness.passband,
                                     timeliness.antiphase=timeliness.antiphase)
                
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
    
    all_q <- 0:(h-1)
    all_stats <- lapply(0:3, function(degree){
        lapply(all_q, function(q){
            print(sprintf("q=%i, pdegree = %i", q, degree))
            x <- all_fst_res(leads = q, pdegree = degree,
                             resolution = 201,
                             lags = h)
            x <- na.omit(x)
            saveRDS(x, sprintf("comparaison_fst/data/fst_h%i_q%i_pdegree%i.RDS",
                               h, q, degree))
            NULL
        })
    })
}
