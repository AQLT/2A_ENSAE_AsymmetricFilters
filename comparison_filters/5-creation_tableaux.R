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
remove_more_than <- function(x, n = 24){
    x[apply(x,1,function(x) all(abs(x) < n)),]
}
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
boxplot_diag <- function(x, n_group = 6, diff = FALSE){
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
rapport_x13 <- function(x){
  apply(x[,-1],2, function(y) y/x[,1])
}

table_quantile <- function(x, digits = 2, diff = FALSE, rapport = FALSE){
    x <- do.call(rbind, x)
    if(length(grep("X12|X13|X11", colnames(x)))>0){
      col_names <- c("X13-ARIMA",colnames(x)[-1])
    }else{
      col_names <- colnames(x)
    }
    
    # On enlève les deux dernières colonnes qui contiennent dates + nom série
    x <- x[, c(1,0)-ncol(x)] 
    col_names <- col_names[c(1,0)-length(col_names)] 
    if(diff & !rapport){
        x <- diff_x13(x)
        col_names <- col_names[-1]
    }
    if(!diff & rapport){
      x <- rapport_x13(x)
      col_names <- col_names[-1]
    }
    print(sprintf("nb_series : %i", nrow(x)))
    data_q <- apply(x,2,quantile,seq(0,1,0.1))
    data_q <- round(rbind(data_q,
                          apply(x, 2, mean)), digits)
    colnames(data_q) <- col_names
    rownames(data_q) <- c("Min", "D1", "D2", "D3", "D4", "Median", "D6", "D7",
                          "D8", "D9", "Max", "Mean")
    data_q
}
covid<- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
                                   date_deb = 2020, date_fin = 2021, trend_length = 13)
covid_f<- formatage_db("comparison_filters/turning_points/full_turning_points_first_fixedylin.RDS",
                     date_deb = 2020, date_fin = 2021, trend_length = 13)
covid_kernel <- formatage_db("comparison_filters/turning_points/full_turning_points_lc_fixedylin.RDS",
                                   date_deb = 2020, date_fin = 2021, trend_length = 13)
saveRDS(unique(do.call(rbind, covid)$series),
        file = "comparison_filters/turning_points/covid_series.RDS")
saveRDS(unique(do.call(rbind, covid_kernel)$series),
        file = "comparison_filters/turning_points/covid_kernel_series.RDS")
sum(covid$downturn[,1:10] - covid_f$downturn[,1:10])
table_quantile(covid)
table_quantile(covid, diff = TRUE)
table_quantile(covid, rapport = TRUE)

table_quantile(covid_f)
table_quantile(covid_f, diff = TRUE)
table_quantile(covid_f, rapport = TRUE)

table_quantile(covid_kernel)
table_quantile(covid_kernel, diff = TRUE)
table_quantile(covid_kernel, rapport = TRUE)

financial_crisis<- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
                     date_deb = 2008, date_fin = 2009, trend_length = 13)
financial_crisis_kernel <- formatage_db("comparison_filters/turning_points/full_turning_points_lc_fixedylin.RDS",
                             date_deb = 2008, date_fin = 2009, trend_length = 13)

financial_crisis_f<- formatage_db("comparison_filters/turning_points/full_turning_points_first_fixedylin.RDS",
                                date_deb = 2008, date_fin = 2009, trend_length = 13)

saveRDS(unique(do.call(rbind, financial_crisis)$series),
        file = "comparison_filters/turning_points/fc_series.RDS")
saveRDS(unique(do.call(rbind, financial_crisis_kernel)$series),
        file = "comparison_filters/turning_points/fc_kernel_series.RDS")
table_quantile(financial_crisis)
table_quantile(financial_crisis, diff = TRUE)
table_quantile(financial_crisis_f)
table_quantile(financial_crisis_f, rapport = TRUE)


table_quantile(financial_crisis, rapport = TRUE)

table_quantile(financial_crisis_kernel)
table_quantile(financial_crisis_kernel, diff = TRUE)

covid_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
                                      date_deb = 2020, date_fin = 2021, trend_length = 23)
financial_crisis_23 <- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
                                   date_deb = 2007, date_fin = 2009, trend_length = 23)
table_quantile(covid_23)
table_quantile(covid_23, diff = TRUE)
table_quantile(covid_23, rapport = TRUE)

table_quantile(financial_crisis_23)
table_quantile(financial_crisis_23, diff = TRUE)
table_quantile(financial_crisis_23, rapport = TRUE)

table_covid <- table_quantile(covid) # 899
table_covid_diff <- table_quantile(covid, diff = TRUE)
table_fc <- table_quantile(fc_tp_fixedylin) #2466

table_covid_23 <- table_quantile(covid_tp_fixedylin_23) # 55
table_fc_23 <- table_quantile(fc_tp_fixedylin_23) # 461

# save(list = c("table_covid", "table_fc", 
#               "table_covid_23", "table_fc_23"),file = "JSM/tables.RData")
saveRDS(list(covid = list(full = covid,
                          first = covid_f,
                          kernels = covid_kernel),
             financial_crisis = list(full = financial_crisis,
                                     first = financial_crisis_f,
                                     kernels = financial_crisis_kernel)),
        file = "comparison_filters/export/data_tp.RDS")

library(kableExtra)
caption <- "Deciles of the time delay to detect turning points (downturn and upturn) in 2020 (900 observations)"
footnote <- "Only series for which the optimal trend-cycle filter is of length 13."
D2 <- "D2: 20% of the turning points are detected with less than 3 months for LC filter and less than 5 months for RKHS filter." 
kbl(table_covid,
    caption = caption,
    booktabs = TRUE)  %>% 
    kable_styling(latex_options = c("striped", "hold_position")) %>% 
    footnote(c(footnote, D2),
             threeparttable = TRUE)

caption <- "Deciles of the time delay to detect turning points (downturn and upturn) between 2007 and 2009 (2 535 observations)"
footnote <- "Only series for which the optimal trend-cycle filter is of length 13."
D2 <- "D2: 20% of the turning points are detected with less than 15 months for LC filter and less than 16 months for other methods." 
kbl(table_fc,
    caption = caption,
    booktabs = TRUE)  %>% 
    kable_styling(latex_options = c("striped", "hold_position")) %>% 
    footnote(c(footnote, D2),
             threeparttable = TRUE)

caption <- "Deciles of the time delay to detect turning points (downturn and upturn) between 2007 and 2009 (534 observations)"
footnote <- "Only series for which the optimal trend-cycle filter is of length 23."
D2 <- "D2: 20% of the turning points are detected with less than 15 months for LC filter and less than 16 months for other methods." 
kbl(table_fc_23,
    caption = caption,
    booktabs = TRUE)  %>% 
    kable_styling(latex_options = c("striped", "hold_position")) %>% 
    footnote(c(footnote, D2),
             threeparttable = TRUE)


which_file <- function(series){
    for(i in 1:9){
        data <- readRDS(sprintf("comparison_filters/data_x11/x13_tp_%02.f_fixedylin.RDS",i))
        if(series %in% names(data)){
            break;
        }
    }
    i
}

# which_file("C235_DE") # 5
# which_file("C25_SE") # 7
# 
# liste_dates <- readRDS("comparison_filters/liste_date_est.RDS")
# # i_f <- 5
# # series <- "C235_DE"
# Sys.setlocale("LC_TIME", "en_US.UTF-8") 
# 
# extract_data <- function(series, date_deb = 2020, comp = "t"){
#     i_f <- which_file(series)
#     nom_f_x13 <- sprintf("comparison_filters/data_x11/x13_%02.f_fixedylin.RDS",i_f)
#     list_f_t <- sprintf("comparison_filters/rkhs/timeliness_bw_%02.f_fixedylin.RDS",i_f)
#     list_f_lc <- sprintf("comparison_filters/lp/lp_lc_%02.f_fixedylin.RDS",i_f)
#     list_f_ql <- sprintf("comparison_filters/lp/lp_ql_%02.f_fixedylin.RDS",i_f)
#     list_f_cq <- sprintf("comparison_filters/lp/lp_cq_%02.f_fixedylin.RDS",i_f)
#     list_f_daf <- sprintf("comparison_filters/lp/lp_daf_%02.f_fixedylin.RDS",i_f)
#     
#     liste_dates_s <- liste_dates[[series]]
#     data_x13 <- readRDS(nom_f_x13)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     data_rkhs <- readRDS(list_f_t)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     data_lc <- readRDS(list_f_lc)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     data_ql <- readRDS(list_f_ql)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     data_cq <- readRDS(list_f_cq)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     data_daf <- readRDS(list_f_daf)[[series]][as.numeric(liste_dates_s)>=date_deb]
#     
#     series_x13 <- do.call(ts.union, lapply(data_x13, `[`, , comp))
#     colnames(series_x13) <- sapply(data_x13, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     series_lc <- do.call(ts.union, lapply(data_lc, `[`, , comp))
#     colnames(series_lc) <- sapply(data_lc, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     series_ql <- do.call(ts.union, lapply(data_ql, `[`, , comp))
#     colnames(series_ql) <- sapply(data_ql, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     series_cq <- do.call(ts.union, lapply(data_cq, `[`, , comp))
#     colnames(series_cq) <- sapply(data_cq, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     series_daf <- do.call(ts.union, lapply(data_daf, `[`, , comp))
#     colnames(series_daf) <- sapply(data_daf, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     series_rkhs <- do.call(ts.union, lapply(data_rkhs, `[`, , comp))
#     colnames(series_rkhs) <- sapply(data_rkhs, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
#     
#     list(x13 = series_x13, 
#          rkhs = series_rkhs,
#          lc = series_lc,
#          ql = series_ql,
#          cq = series_cq,
#          daf = series_daf)
# }
# data_covid <- do.call(rbind, covid_tp_fixedylin)
# data_covid[c(which.max(data_covid[,"X13"] - data_covid[,"LC"]),
#              which.max(data_covid[,"LC"] - data_covid[,"X13"])),]

# 
# ts_data_l <- readRDS("comparison_filters/eurostat_db.RDS")
# plot(ts_data_l[[series]])
# C235_DE <- extract_data("C235_DE")
# C25_SE <- extract_data("C25_SE")
# 
# series_retain <- c("Feb 2020", "Mar 2020", "Apr 2020", "May 2020", 
#                    "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020",
#                    "Oct 2020", "Nov 2020","Dec 2020", "Jan 2021",
#                    "Feb 2021", "Mar 2021")
# titre <- "Trend-cycle estimation of the series %s with the method %s"
# library(highcharter)
# plot_hc <- function(...){
#     AQLTools::hc_stocks(...)%>% 
#         hc_rangeSelector(enabled = FALSE) %>%
#         hc_navigator(enabled = FALSE) %>% 
#         hc_scrollbar(enabled = FALSE) %>% 
#         hc_credits(enabled = FALSE)
# }
# export_plot <- function(h_plot){
#     
# }
# file_html <- tempfile(fileext = ".html")
# htmlwidgets::saveWidget(widget = abc, file = file_html)
# setwd("~")
# 
# webshot::webshot(url = file_html, vwidth = 1400,
#                  file = "map.png",
#                  delay = 3)
# plot_hc(window(C235_DE$x13, start = c(2019,8))[,
#                                                series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "X12-ARIMA"),
#         sous_titre = "Time-delay = 13 months") %>% 
#     hc_yAxis(min = 95, max = 115)
# plot_hc(window(C235_DE$rkhs, start = c(2019,8)),
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "RKHS"),
#         sous_titre = "Time-delay = 5 months")%>% 
#     hc_yAxis(min = 95, max = 112)
# plot_hc(window(C235_DE$lc, start = c(2019,8))[,
#                                               series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "LC"),
#         sous_titre = "Time-delay = 3 months")%>% 
#     hc_yAxis(min = 95, max = 112)
# plot_hc(window(C235_DE$ql, start = c(2019,8))[,
#                                               series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "QL"),
#         sous_titre = "Time-delay = 3 months")%>% 
#     hc_yAxis(min = 95, max = 112)
# plot_hc(window(C235_DE$cq, start = c(2019,8))[,
#                                               series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "CQ"),
#         sous_titre = "Time-delay = 2 months")%>% 
#     hc_yAxis(min = 95, max = 112)
# plot_hc(window(C235_DE$daf, start = c(2019,8))[,
#                                                series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C235_DE", "DAF"),
#         sous_titre = "Time-delay = 3 months")%>% 
#     hc_yAxis(min = 95, max = 112)
# 
# titre <- "Trend-cycle estimation of the series %s with the method %s"
# plot_hc(window(C25_SE$x13, start = c(2019,8))[,
#                                               series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "X12-ARIMA"),
#         sous_titre = "Time-delay = 5 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# plot_hc(window(C25_SE$rkhs, start = c(2019,8)),
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "RKHS"),
#         sous_titre = "Time-delay = 13 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# 
# plot_hc(window(C25_SE$lc, start = c(2019,8))[,
#                                              series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "LC"),
#         sous_titre = "Time-delay = 13 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# 
# plot_hc(window(C25_SE$ql, start = c(2019,8))[,
#                                              series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "QL"),
#         sous_titre = "Time-delay = 14 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# 
# plot_hc(window(C25_SE$cq, start = c(2019,8))[,
#                                              series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "CQ"),
#         sous_titre = "Time-delay = 14 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# 
# plot_hc(window(C25_SE$daf, start = c(2019,8))[,
#                                               series_retain],
#         digits = 1,
#         titre = sprintf(titre, "C25_SE", "DAF"),
#         sous_titre = "Time-delay = 14 months")%>% 
#     hc_yAxis(min = 98, max = 108)
# 
# tmp = readRDS(file = "comparison_filters/full_turning_points_du_fixedylin.RDS")
# tmp[[series]]$upturn
# 
# end(data_x13[[1]])
# AQLTools::hc_stocks(window(series_x13, start = c(2019,8))
# )
# AQLTools::hc_stocks(series_lc)
# names(data_x13)
# data_x13
# names(data_x13)
# 
# AQLTools::hc_stocks(window(series_x13, start = c(2019,8)))
# AQLTools::hc_stocks(window(series_lc, start = c(2019,8)))
# 
# file.rename(list.files("/Users/alainquartierlatente/Desktop/jsm_image",
#                        full.names = TRUE),
#             c(sprintf("/Users/alainquartierlatente/Desktop/jsm_image/%s_%s.png",
#                       "C235DE", c("x13","rkhs","lc","ql","cq","daf")),
#               sprintf("/Users/alainquartierlatente/Desktop/jsm_image/%s_%s.png",
#                       "C25SE", c("x13","rkhs","lc","ql","cq","daf"))
#             ))
# d