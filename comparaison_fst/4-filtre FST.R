############
library(dplyr)
h <- 6
data = readRDS(sprintf("comparaison_fst/data/comparison_fst_weights_h%i_all.RDS",
                       h))
sym <- fst_filter(h, leads = h, pdegree = 2,
                  smoothness.weight = 1)$filters.coef

extract_fst_filter <- function(method_){
    global_data <- data %>% 
        filter(fidelity.weight * timeliness.weight * smoothness.weight >0) %>% #Contrainte de non nullité
        group_by(method,q,degree) %>%
        arrange(desc(timeliness.weight)) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        filter(method == method_) %>% 
        as.data.frame()
    final_data <- global_data %>% 
        group_by(q) %>% 
        arrange(desc(degree)) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        as.data.frame()
    liste_filters <- apply(final_data, 1, function(x){
        q <- fst_filter(h, leads = as.numeric(x["q"]),
                        pdegree = as.numeric(x["degree"]),
                        timeliness.weight = as.numeric(x["timeliness.weight"]),
                        smoothness.weight = as.numeric(x["smoothness.weight"]))
        q
    })
    coefs <- sapply(liste_filters, function(x){
        q <- x$filters.coef
        matrix(c(q,rep(0,2*h+1-length(q))), ncol = 1)
    })
    coefs <- cbind(coefs, sym)
    criteria <- sapply(liste_filters, `[[`,"criteria")
    
    colnames(coefs) <- sprintf("q=%i", 0:(h))
    colnames(criteria) <- sprintf("q=%i", 0:(h-1))
    
    rownames(coefs) <- rjdfilters:::coefficients_names(-h, h)
    list(global_data = global_data,
         final_weights = final_data,
         coefs = coefs,
         criteria = criteria)
}

extract_fst_min <- function(method_){
    global_data <- data %>% 
        filter(fidelity.weight * timeliness.weight * smoothness.weight >0) %>% #Contrainte de non nullité
        group_by(method,q,degree) %>%
        arrange((timeliness.weight)) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        filter(method == method_) %>% 
        as.data.frame()
    final_data <- global_data %>% 
        group_by(q) %>% 
        arrange((degree)) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        as.data.frame()
    liste_filters <- apply(final_data, 1, function(x){
        q <- fst_filter(h, leads = as.numeric(x["q"]),
                        pdegree = as.numeric(x["degree"]),
                        timeliness.weight = as.numeric(x["timeliness.weight"]),
                        smoothness.weight = as.numeric(x["smoothness.weight"]))
        q
    })
    coefs <- sapply(liste_filters, function(x){
        q <- x$filters.coef
        matrix(c(q,rep(0,2*h+1-length(q))), ncol = 1)
    })
    coefs <- cbind(coefs, sym)
    criteria <- sapply(liste_filters, `[[`,"criteria")
    
    colnames(coefs) <- sprintf("q=%i", 0:(h))
    colnames(criteria) <- sprintf("q=%i", 0:(h-1))
    
    rownames(coefs) <- rjdfilters:::coefficients_names(-h, h)
    list(global_data = global_data,
         final_weights = final_data,
         coefs = coefs,
         criteria = criteria)
}

extract_fst_med <- function(method_){
    global_data <- data %>% 
        filter(fidelity.weight * timeliness.weight * smoothness.weight >0) %>% #Contrainte de non nullité
        group_by(method,q,degree) %>%
        mutate(tw_med = abs(timeliness.weight - median(timeliness.weight))) %>% 
        arrange(tw_med) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        filter(method == method_) %>% 
        as.data.frame()
    final_data <- global_data %>% 
        group_by(q) %>% 
        arrange(degree) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(method, q, degree) %>% 
        as.data.frame()
    liste_filters <- apply(final_data, 1, function(x){
        q <- fst_filter(h, leads = as.numeric(x["q"]),
                        pdegree = as.numeric(x["degree"]),
                        timeliness.weight = as.numeric(x["timeliness.weight"]),
                        smoothness.weight = as.numeric(x["smoothness.weight"]))
        q
    })
    coefs <- sapply(liste_filters, function(x){
        q <- x$filters.coef
        matrix(c(q,rep(0,2*h+1-length(q))), ncol = 1)
    })
    coefs <- cbind(coefs, sym)
    criteria <- sapply(liste_filters, `[[`,"criteria")
    
    colnames(coefs) <- sprintf("q=%i", 0:(h))
    colnames(criteria) <- sprintf("q=%i", 0:(h-1))
    
    rownames(coefs) <- rjdfilters:::coefficients_names(-h, h)
    list(global_data = global_data,
         final_weights = final_data,
         coefs = coefs,
         criteria = criteria)
}


lc <- extract_fst_filter("LC")
lc_min <- extract_fst_min("LC")
lc_med <- extract_fst_med("LC")
lc_med$final_weights

# lc2 <- extract_fst_filter("LC")
# lc$final_weights
# lc2$final_weights

# verification :
henderson_f <- lp_filter(horizon = 6, kernel = "Henderson",ic = 3.5)

sapply(apply(henderson_f$filters.coef,2, fst, lags = 6), `[[`, "criteria")[,-7] - 
    lc$criteria

rkhs <- extract_fst_filter("phase")
rkhs_min <- extract_fst_min("phase")
rkhs_med <- extract_fst_med("phase")


saveRDS(list(lc = lc,
             lc_min = lc_min,
             lc_med = lc_med,
             rkhs_timeliness = rkhs,
             rkhs_timeliness_min = rkhs_min,
             rkhs_timeliness_med = rkhs_med),
        sprintf("comparaison_fst/fst_h%i.RDS", h))


h <- 11
data  = readRDS(sprintf("comparaison_fst/data/comparison_fst_weights_h%i_all.RDS",
                        h))

sym <- fst_filter(h, leads = h, pdegree = 2,
                  smoothness.weight = 1)$filters.coef
lc <- extract_fst_filter("LC")
lc_min <- extract_fst_min("LC")
lc_med <- extract_fst_med("LC")
rkhs <- extract_fst_filter("phase")
rkhs_min <- extract_fst_min("phase")
rkhs_med <- extract_fst_med("phase")

saveRDS(list(lc = lc,
             lc_min = lc_min,
             lc_med = lc_med,
             rkhs_timeliness = rkhs,
             rkhs_timeliness_min = rkhs_min,
             rkhs_timeliness_med = rkhs_med),
        sprintf("comparaison_fst/fst_h%i.RDS", h))

saveRDS(list(
    `h=6` = lapply(readRDS(sprintf("comparaison_fst/fst_h%i.RDS", 6)),`[[`,"coefs"),
    `h=11` = lapply(readRDS(sprintf("comparaison_fst/fst_h%i.RDS", 11)),`[[`,"coefs")
),
"comparison_filters/filters/fst.RDS"
)



