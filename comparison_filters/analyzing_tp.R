rm(list = ls())
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
formatage_db <- function(nom_f,
                         date_deb= NULL, date_fin= NULL,
                         type = c("downturn","upturn"),
                         trend_length = c(9, 13, 23),
                         decomposition = c("Multiplicative","Additive" )){
    full_tp <- readRDS(file = nom_f)
    full_tp <- lapply(full_tp, function(x_){
        lapply(x_, function(x){
            x[, !apply(is.na(x),2,any), drop = FALSE]
        })
    })
    full_tp <- full_tp[sapply(full_tp,function(x) sum(sapply(x,length)))>0]
    trend_length_data <- sapply(names(full_tp),function(x){
        find_trend_length(liste_trend_length[[x]], date_deb, date_fin)
    })
    full_tp <- full_tp[trend_length_data %in% trend_length]
    
    decomposition_data <- sapply(names(full_tp),function(x){
        find_trend_length(liste_decomposition[[x]], date_deb, date_fin,
                          is_num = FALSE)
    })
    full_tp <- full_tp[decomposition_data %in% decomposition]
    
    full_db <- list(downturn = t(do.call(cbind,sapply(full_tp,`[[`, "downturn"))),
                    upturn = t(do.call(cbind,sapply(full_tp,`[[`, "upturn"))))
    if(!is.null(date_fin)){
        full_db <- lapply(full_db, function(x) x[rownames(x)<date_fin,])
    }
    if(!is.null(date_deb)){
        full_db <- lapply(full_db, function(x) x[rownames(x)>date_deb,])
    }
    full_db
}
remove_more_than <- function(x, n = 24){
    x[apply(x,1,function(x) all(abs(x) < n)),]
}

covid_tp <- formatage_db("comparison_filters/turning_points/full_turning_points_du.RDS",
                         date_deb = 2020, date_fin = 2021, trend_length = 13)
covid_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                         date_deb = 2020, date_fin = 2021, trend_length = 13)
covid_tp_first <- formatage_db("comparison_filters/turning_points/full_turning_points_first_du.RDS",
                               date_deb = 2020, date_fin = 2021, trend_length = 13)

covid_tp_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_du.RDS",
                         date_deb = 2020, date_fin = 2021, trend_length = 23)
covid_tp_fixedylin_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                                   date_deb = 2020, date_fin = 2021, trend_length = 23)
covid_tp_first_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_first_du.RDS",
                               date_deb = 2020, date_fin = 2021, trend_length = 23)


fc_tp <- formatage_db("comparison_filters/turning_points/full_turning_points_du.RDS",
                      date_deb = 2008, date_fin = 2010, trend_length = 13)
fc_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                                date_deb = 2008, date_fin = 2010, trend_length = 13)
fc_tp_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_du.RDS",
                      date_deb = 2008, date_fin = 2010, trend_length = 23)
fc_tp_fixedylin_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                                date_deb = 2008, date_fin = 2010, trend_length = 23)

fc_tp_first <- formatage_db("comparison_filters/turning_points/full_turning_points_first_du.RDS",
                            date_deb = 2009, date_fin = 2010, trend_length = 13)

total_first <- formatage_db("comparison_filters/turning_points/full_turning_points_first_du.RDS")
total <- formatage_db("comparison_filters/turning_points/full_turning_points_du.RDS")

save(list=c("covid_tp", "covid_tp_23", "covid_tp_first", 
            "covid_tp_first_23", "covid_tp_fixedylin", "covid_tp_fixedylin_23", 
            "diff_x13", "fc_tp", "fc_tp_23", "fc_tp_first", 
            "fc_tp_fixedylin", "fc_tp_fixedylin_23"),file = "JSM/data_comp.Rdata")
summary(as.numeric(rownames(covid_tp_first)))
library(ggplot2)
library(patchwork)
violin_diag <- function(x, n_group = 7, diff = FALSE){
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n_points <- nrow(x)
    n_col <- ncol(x)
    print(n_points)
    if(diff){
        x <- apply(x[,-1],2, function(y) y-x[,1])
    }
    covid_tp_gg <- reshape2::melt(x)
    colnames(covid_tp_gg) <- c("date","method","y")
    p <- ggplot(covid_tp_gg, aes(method, y, fill = method))
    p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
                    scale  ="count") + 
        theme_bw() + guides(fill = FALSE) + 
        scale_fill_manual(values=gg_color_hue(n_group)[(n_group - n_col + 1):n_group])
}
boxplot_diag <- function(x, n_group = 7, diff = FALSE){
    gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
    }
    n_points <- nrow(x)
    n_col <- ncol(x)
    print(n_points)
    if(diff){
        x <- diff_x13(x)
    }
    covid_tp_gg <- reshape2::melt(x)
    colnames(covid_tp_gg) <- c("date","method","y")
    p <- ggplot(covid_tp_gg, aes(method, y, fill = method))
    p + geom_boxplot() + 
        theme_bw() + guides(fill = FALSE) + 
        scale_fill_manual(values=gg_color_hue(n_group)[(n_group - n_col + 1):n_group])
}
diff_x13 <- function(x){
    apply(x[,-1],2, function(y) y-x[,1])
}

lapply(covid_tp,function(x) apply(x,2,quantile,seq(0,1,0.1)))
apply(do.call(rbind, covid_tp),2,quantile,seq(0,1,0.25))
apply(do.call(rbind, covid_tp_fixedylin),2,quantile,seq(0,1,0.25))
apply(do.call(rbind, covid_tp_fixedylin_23),2,quantile,seq(0,1,0.25))

lapply(covid_tp_fixedylin, apply, 2,quantile,seq(0,1,0.25))
apply(diff_x13(do.call(rbind, covid_tp_fixedylin)),2,quantile,seq(0,1,0.25))



(violin_diag(covid_tp$upturn) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(covid_tp$upturn, diff = TRUE) + labs(x = NULL, y = "Difference with X-13")) + 
    plot_annotation(
        title = 'Time delay to detect upturn in 2020 (602 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point without any further revision'
    ) 

(violin_diag(covid_tp_fixedylin$upturn,n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(covid_tp$upturn[,-3],n_group = 6) + labs(x = NULL, y = "Time delay"))

(boxplot_diag(covid_tp_fixedylin$upturn,n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (boxplot_diag(covid_tp$upturn[,-3],n_group = 6) + labs(x = NULL, y = "Time delay"))

(boxplot_diag(covid_tp_fixedylin$downturn,n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (boxplot_diag(covid_tp$downturn[,-3],n_group = 6) + labs(x = NULL, y = "Time delay"))


(violin_diag(covid_tp$downturn) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(covid_tp$downturn, diff = TRUE) + labs(x = NULL, y = "Difference with X-13")) + 
    plot_annotation(
        title = 'Time delay to detect downturn in 2020 (426 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point without any further revision'
    ) 

(violin_diag(covid_tp_first$upturn) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(covid_tp_first$upturn, diff = TRUE) + labs(x = NULL, y = "Difference with X-13")) + 
    plot_annotation(
        title = 'Time delay to detect upturn in 2020 (885 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point for the first time'
    ) 

(violin_diag(covid_tp_first$downturn) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(covid_tp_first$downturn, diff = TRUE) + labs(x = NULL, y = "Difference with X-13")) + 
    plot_annotation(
        title = 'Time delay to detect downturn in 2020 (658 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point for the first time'
    ) 

#################################
## Financial crisis

apply(do.call(rbind, fc_tp),2,quantile,seq(0,1,0.1))
apply(do.call(rbind, fc_tp_fixedylin),2,quantile,seq(0,1,0.25))
apply((do.call(rbind, fc_tp_fixedylin)),2,quantile,seq(0,1,0.25))

apply(diff_x13(do.call(rbind, fc_tp_fixedylin)),2,quantile,seq(0,1,0.25))
apply((do.call(rbind, fc_tp_fixedylin)),2,mean)

apply(do.call(rbind, fc_tp_23),2,quantile,seq(0,1,0.25))
apply(do.call(rbind, fc_tp_fixedylin_23),2,quantile,seq(0,1,0.25))
lapply(covid_tp_fixedylin, apply, 2,quantile,seq(0,1,0.25))

apply(diff_x13(do.call(rbind, fc_tp_fixedylin_23)),2,quantile,seq(0,1,0.25))


(violin_diag(do.call(rbind, fc_tp)[,-3], n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(do.call(rbind, fc_tp)[,-3], diff = TRUE, n_group = 6) + labs(x = NULL, y = "Difference with X-13"))  + 
    plot_annotation(
        title = 'Time delay to detect turning points (downturn and upturn) in 2009 (3 515 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point without any further revision'
    ) 

(violin_diag(do.call(rbind, fc_tp_first)[,-3], n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(do.call(rbind, fc_tp_first)[,-3], diff = TRUE, n_group = 6) + labs(x = NULL, y = "Difference with X-13"))  + 
    plot_annotation(
        title = 'Time delay to detect turning points (downturn and upturn) in 2009 (2 369 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point for the first time'
    ) 

#######
lapply(fc_tp,function(x) apply(x,2,quantile,seq(0,1,0.1)))
apply(do.call(rbind, fc_tp),2,quantile,seq(0,1,0.1))
apply(do.call(rbind, fc_tp_diff),2,quantile,seq(0,1,0.1))


data <- do.call(rbind, fc_tp_first)[,-3]

nrow(data) - nrow(remove_more_than(data, 40))


(violin_diag(remove_more_than(data, 40), n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(remove_more_than(data, 40), diff = TRUE, n_group = 6) + labs(x = NULL, y = "Difference with X-13"))  + 
    plot_annotation(
        title = 'Time delay to detect turning points (downturn and upturn) in 2009 (1 957 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point for the first time'
    ) 

data <- do.call(rbind, fc_tp)[,-3]
data_diff <- apply(data[,-1],2, function(y) y-data[,1])
apply(data_diff,2,quantile,seq(0,1,0.1))

i_keep <- apply(data_diff,1,function(x) all(abs(x) < 11))

(violin_diag(data[i_keep,], n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(data[i_keep,], diff = TRUE, n_group = 6) + labs(x = NULL, y = "Difference with X-13"))  + 
    plot_annotation(
        title = 'Time delay to detect turning points (downturn and upturn) in 2009 (2 518 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point without any further revision'
    ) 



lapply(total,function(x) apply(x,2,quantile,seq(0,1,0.1)))
apply(do.call(rbind, total),2,quantile,seq(0,1,0.1))

(violin_diag(do.call(rbind, total)[,-3], n_group = 6) + labs(x = NULL, y = "Time delay")) /
    (violin_diag(do.call(rbind, total)[,-3], diff = TRUE, n_group = 6) + labs(x = NULL, y = "Difference with X-13"))  + 
    plot_annotation(
        title = 'Time delay to detect turning points (downturn and upturn) in 2009 (3 515 observations)',
        caption = 'Note: time delay = number of months needed to detect the turning point without any further revision'
    ) 

summary(covid_tp)

