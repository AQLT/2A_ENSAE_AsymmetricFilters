# Find a specific series with the time delay
# and plot the different revisions
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
    full_db <- lapply(full_db,function(x){
        data.frame(x, date = rownames(x))
    })
    full_db[["downturn"]]$series <- unlist(lapply(names(full_tp), function(x){
        rep(x, ncol(full_tp[[x]][["downturn"]]))
    }))
    full_db[["upturn"]]$series <- unlist(lapply(names(full_tp), function(x){
        rep(x, ncol(full_tp[[x]][["upturn"]]))
    }))
    if(!is.null(date_fin)){
        full_db <- lapply(full_db, function(x) x[x$date<date_fin,])
    }
    if(!is.null(date_deb)){
        full_db <- lapply(full_db, function(x) x[x$date>date_deb,])
    }
    full_db
}

covid_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                                   date_deb = 2020, date_fin = 2021, trend_length = 13)
data_covid <- do.call(rbind, covid_tp_fixedylin)
fc_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_du_fixedylin.RDS",
                                date_deb = 2007, date_fin = 2009, trend_length = 13)
financial_crisis <- do.call(rbind, fc_tp_fixedylin)

data_covid[c(which.max(data_covid[,"X13"] - data_covid[,"LC"]),
             which.max(data_covid[,"LC"] - data_covid[,"X13"])),]
financial_crisis[c(which.max(financial_crisis[,"X13"] - financial_crisis[,"LC"]),
             which.max(financial_crisis[,"LC"] - financial_crisis[,"X13"])),]

series_to_file <- readRDS(file = "comparison_filters/series_to_file.RDS")
series_to_file[c("C235_DE", "C25_SE","C205_IT", "C18_ES")]
liste_dates <- readRDS("comparison_filters/liste_date_est.RDS")
# i_f <- 5
# series <- "C235_DE"
Sys.setlocale("LC_TIME", "en_US.UTF-8") 

extract_data <- function(series, date_deb = 2020, comp = "t", date_fin = NULL){
    i_f <- series_to_file[series]
    nom_f_x13 <- sprintf("comparison_filters/data_x11/x13_%02.f_fixedylin.RDS",i_f)
    list_f_t <- sprintf("comparison_filters/rkhs/timeliness_bw_%02.f_fixedylin.RDS",i_f)
    list_f_lc <- sprintf("comparison_filters/lp/lp_lc_%02.f_fixedylin.RDS",i_f)
    list_f_ql <- sprintf("comparison_filters/lp/lp_ql_%02.f_fixedylin.RDS",i_f)
    list_f_cq <- sprintf("comparison_filters/lp/lp_cq_%02.f_fixedylin.RDS",i_f)
    list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_fixedylin.RDS",i_f)
    list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_fixedylin.RDS",i_f)
    list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_fixedylin.RDS",i_f)
    
    liste_dates_s <- liste_dates[[series]]
    if(!is.null(date_deb)){
        if(!is.null(date_fin)){
            liste_dates_bool <- (as.numeric(liste_dates_s)>=date_deb) &
                (as.numeric(liste_dates_s)<=date_fin)
        }else{
            liste_dates_bool <- as.numeric(liste_dates_s)>=date_deb
        }
    }else if(!is.null(date_fin)){
        liste_dates_bool <- as.numeric(liste_dates_s)<=date_fin
    }
    
    
    
    data_x13 <- readRDS(nom_f_x13)[[series]][liste_dates_bool]
    data_rkhs <- readRDS(list_f_t)[[series]][liste_dates_bool]
    data_lc <- readRDS(list_f_lc)[[series]][liste_dates_bool]
    data_ql <- readRDS(list_f_ql)[[series]][liste_dates_bool]
    data_cq <- readRDS(list_f_cq)[[series]][liste_dates_bool]
    data_daf <- readRDS(list_f_daf)[[series]][liste_dates_bool]
    
    series_x13 <- do.call(ts.union, lapply(data_x13, `[`, , comp))
    colnames(series_x13) <- sapply(data_x13, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    
    series_lc <- do.call(ts.union, lapply(data_lc, `[`, , comp))
    colnames(series_lc) <- sapply(data_lc, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    
    series_ql <- do.call(ts.union, lapply(data_ql, `[`, , comp))
    colnames(series_ql) <- sapply(data_ql, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    
    series_cq <- do.call(ts.union, lapply(data_cq, `[`, , comp))
    colnames(series_cq) <- sapply(data_cq, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    
    series_daf <- do.call(ts.union, lapply(data_daf, `[`, , comp))
    colnames(series_daf) <- sapply(data_daf, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    
    series_rkhs <- do.call(ts.union, lapply(data_rkhs, `[`, , comp))
    colnames(series_rkhs) <- sapply(data_rkhs, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
    # colnames(series_x13) <- colnames(series_lc) <- 
    #     colnames(series_ql) <- colnames(series_cq) <- 
    #     colnames(series_daf) <- colnames(series_rkhs) <- 
    #     zoo::as.yearmon(as.numeric(liste_dates_s[liste_dates_bool]))
    list(x13 = series_x13, 
         rkhs = series_rkhs,
         lc = series_lc,
         ql = series_ql,
         cq = series_cq,
         daf = series_daf)
}


# ts_data_l <- readRDS("comparison_filters/eurostat_db.RDS")
# plot(ts_data_l[[series]])
C235_DE <- extract_data("C235_DE")
C25_SE <- extract_data("C25_SE")
C255_ES <- extract_data("C205_IT", date_deb = 2007)
C18_ES <- extract_data("C18_ES", date_deb = 2007)

library(highcharter)
plot_hc <- function(...){
    AQLTools::hc_stocks(...)%>% 
        hc_rangeSelector(enabled = FALSE) %>%
        hc_navigator(enabled = FALSE) %>% 
        hc_scrollbar(enabled = FALSE) %>% 
        hc_credits(enabled = FALSE)
}
export_plot <- function(h_plot){
    
}
series_retain <- c("Feb 2020", "Mar 2020", "Apr 2020", "May 2020", 
                   "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020",
                   "Oct 2020", "Nov 2020","Dec 2020", "Jan 2021",
                   "Feb 2021", "Mar 2021")
titre <- "Trend-cycle estimation of the series %s with the method %s"
file_html <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(widget = abc, file = file_html)

webshot::webshot(url = file_html, vwidth = 1400,
                 file = "map.png",
                 delay = 3)
plot_hc(window(C235_DE$x13, start = c(2019,8))[,
                                               series_retain],
        digits = 1,
        titre = sprintf(titre, "C235_DE", "X12-ARIMA"),
        sous_titre = "Time-delay = 13 months") %>% 
    hc_yAxis(min = 95, max = 115)
plot_hc(window(C235_DE$rkhs, start = c(2019,8)),
        digits = 1,
        titre = sprintf(titre, "C235_DE", "RKHS"),
        sous_titre = "Time-delay = 5 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$lc, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, "C235_DE", "LC"),
        sous_titre = "Time-delay = 3 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$ql, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, "C235_DE", "QL"),
        sous_titre = "Time-delay = 3 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$cq, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, "C235_DE", "CQ"),
        sous_titre = "Time-delay = 2 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$daf, start = c(2019,8))[,
                                               series_retain],
        digits = 1,
        titre = sprintf(titre, "C235_DE", "DAF"),
        sous_titre = "Time-delay = 3 months")%>% 
    hc_yAxis(min = 95, max = 112)

titre <- "Trend-cycle estimation of the series %s with the method %s"
plot_hc(window(C25_SE$x13, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, "C25_SE", "X12-ARIMA"),
        sous_titre = "Time-delay = 5 months")%>% 
    hc_yAxis(min = 98, max = 108)
plot_hc(window(C25_SE$rkhs, start = c(2019,8)),
        digits = 1,
        titre = sprintf(titre, "C25_SE", "RKHS"),
        sous_titre = "Time-delay = 13 months")%>% 
    hc_yAxis(min = 98, max = 108)

plot_hc(window(C25_SE$lc, start = c(2019,8))[,
                                             series_retain],
        digits = 1,
        titre = sprintf(titre, "C25_SE", "LC"),
        sous_titre = "Time-delay = 13 months")%>% 
    hc_yAxis(min = 98, max = 108)

plot_hc(window(C25_SE$ql, start = c(2019,8))[,
                                             series_retain],
        digits = 1,
        titre = sprintf(titre, "C25_SE", "QL"),
        sous_titre = "Time-delay = 14 months")%>% 
    hc_yAxis(min = 98, max = 108)

plot_hc(window(C25_SE$cq, start = c(2019,8))[,
                                             series_retain],
        digits = 1,
        titre = sprintf(titre, "C25_SE", "CQ"),
        sous_titre = "Time-delay = 14 months")%>% 
    hc_yAxis(min = 98, max = 108)

plot_hc(window(C25_SE$daf, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, "C25_SE", "DAF"),
        sous_titre = "Time-delay = 14 months")%>% 
    hc_yAxis(min = 98, max = 108)


C255_ES <- extract_data("C205_IT", date_deb = 2007)
C18_ES <- extract_data("C18_ES", date_deb = 2007)
library(highcharter)
plot_hc <- function(...){
    AQLTools::hc_stocks(..., digits = 1)%>% 
        hc_rangeSelector(enabled = FALSE) %>%
        hc_navigator(enabled = FALSE) %>% 
        hc_scrollbar(enabled = FALSE) %>% 
        hc_credits(enabled = FALSE)
}
financial_crisis[c(which.max(financial_crisis[,"X13"] - financial_crisis[,"LC"]),
                   which.max(financial_crisis[,"LC"] - financial_crisis[,"X13"])),]

plot_hc(window(C255_ES$x13, start = 2007, end = 2009)[,1:30])
plot_hc(window(C18_ES$x13))

plot_hc(window(C18_ES$lc , start = c(2006,8), end = 2009)[,1:30])

