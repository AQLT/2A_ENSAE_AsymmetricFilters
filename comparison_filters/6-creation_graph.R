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

covid_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
                                   date_deb = 2020, date_fin = 2021, trend_length = 13)
data_covid <- do.call(rbind, covid_tp_fixedylin)
fc_tp_fixedylin <- formatage_db("comparison_filters/turning_points/full_turning_points_fixedylin.RDS",
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
# Sys.setlocale("LC_TIME", "en_US.UTF-8") 
extract_data <- function(series, date_deb = 2020, comp = "t", date_fin = NULL,
                         liste_fichiers = "comparison_filters/turning_points/liste_files.RDS"){
    i_f <- series_to_file[series]
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
    liste_file <- readRDS(liste_fichiers)
    noms_f <- sapply(liste_file, 
                     \(x) sub("_tp","", x[i_f]))
    all_data <- lapply(noms_f,
                       \(x) readRDS(x)[[series]][liste_dates_bool])
    all_data <- lapply(all_data, function(x){
        series <- do.call(ts.union, lapply(x, `[`, , comp))
        colnames(series) <- sapply(x, function(x) as.character(zoo::as.yearmon(tail(time(x),1))))
        series
    })
    all_data
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
        hc_credits(enabled = FALSE) %>% 
        hc_exporting(enabled = FALSE)
}
export_plot <- function(h_plot,
                        export_file,
                        vdim = c(1906, 976),
                        ratio = 2,
                        delay = 1.5,
                        zoom = 3){
    file_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget = h_plot, file = file_html)

    vdim = vdim/ratio
    webshot::webshot(url = file_html,
                     vwidth = vdim[1], vheight = vdim[2],
                     file = export_file,
                     delay = delay, zoom = zoom)
}

global_export_plot <- function(data, nom_serie,
                               series_retain,
                               start = c(2019,8),
                               end = NULL,
                               tp_date = 2020+1/12,
                               data_tp = data_covid){
    # nom_serie <- "C235_DE"
    # tp_date <- 2020+1/12 
    data <- lapply(data, function(x){
        window(x, start = start, end = end)[,series_retain]
    })
    
    ylim <- range(do.call(ts.union, data),na.rm = TRUE)
    
    label_method <- c("X13" = "X-13-ARIMA", 
                      "RKHS_timeliness" = "le filtre RKHS",
                      "LC" = "le filtre Linear-Constant (LC)",
                      "QL" = "le filtre Quadratic-Linear (QL)",
                      "CQ" = "le filtre Cubic-Quadratic (CQ)",
                      "DAF" = "le filtre asymétrique direct (DAF)",
                      "FST_LC" = "le filtre FST optimal par rapport au filtre LC et minimisant la timeliness", 
                      "FST_LC_min" = "le filtre FST optimal par rapport au filtre LC et ayant la timeliness la plus élevée",
                      "FST_LC_med" = "le filtre FST optimal par rapport au filtre LC et ayant une timeliness médiane",
                      "FST_RKHS_timeliness" = "le filtre FST optimal par rapport au filtre RKHS et minimisant la timeliness")
    # dput(names(data))
    titre <- "Tendance-cycle de la série %s avec %s"
    sous_titre <- "Déphasage = %.0f mois"
    file_export <- "comparison_filters/export/%s_%s.%s"
    for(nom_method in names(data)){
        print(nom_method)
        time_delay <- data_tp[with(data_tp,
                                   round(as.numeric(date), 3) == round(tp_date, 3) & series == nom_serie),
                              nom_method]
        p <- plot_hc(data[[nom_method]],
                     digits = 1,
                     titre = sprintf(titre, nom_serie, label_method[nom_method]),
                     sous_titre = sprintf(sous_titre, time_delay)) %>% 
            hc_yAxis(min = ylim[1], max = ylim[2])
        p %>% 
            export_plot(sprintf(file_export,tolower(nom_serie) , tolower(nom_method), "pdf"), 
                        zoom = 1, delay = 1.5)
        p %>% 
            export_plot(sprintf(file_export,tolower(nom_serie) , tolower(nom_method), "jpeg"), 
                        zoom = 2.5, delay = 1.5)
    }
}
series_retain <- c("Feb 2020", "Mar 2020", "Apr 2020", "May 2020", 
                   "Jun 2020", "Jul 2020", "Aug 2020", "Sep 2020",
                   "Oct 2020", "Nov 2020","Dec 2020", "Jan 2021",
                   "Feb 2021", "Mar 2021")

series_retain <- c("fév 2020", "mar 2020", "avr 2020", "mai 2020", 
                   "jui 2020", "jul 2020", "aoû 2020", "sep 2020", "oct 2020", 
                   "nov 2020", "déc 2020", "jan 2021", "fév 2021", "mar 2021")
# C235_DE <- extract_data("C235_DE")
# C25_SE <- extract_data("C25_SE")
global_export_plot(data = C235_DE,
                   nom_serie = "C235_DE",
                   series_retain = series_retain)
global_export_plot(data = C25_SE,
                   nom_serie = "C25_SE",
                   series_retain = series_retain)

plot_hc(window(C235_DE$RKHS_timeliness, start = c(2019,8)),
        digits = 1,
        titre = sprintf(titre, series, "RKHS"),
        sous_titre = "Time-delay = 5 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$lc, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, series, "LC"),
        sous_titre = "Time-delay = 3 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$ql, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, series, "QL"),
        sous_titre = "Time-delay = 3 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$cq, start = c(2019,8))[,
                                              series_retain],
        digits = 1,
        titre = sprintf(titre, series, "CQ"),
        sous_titre = "Time-delay = 2 months")%>% 
    hc_yAxis(min = 95, max = 112)
plot_hc(window(C235_DE$daf, start = c(2019,8))[,
                                               series_retain],
        digits = 1,
        titre = sprintf(titre, series, "DAF"),
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

financial_crisis[c(which.max(financial_crisis[,"X13"] - financial_crisis[,"LC"]),
                   which.max(financial_crisis[,"LC"] - financial_crisis[,"X13"])),]

plot_hc(window(C255_ES$x13, start = 2007, end = 2009)[,1:30])
plot_hc(window(C18_ES$x13))

plot_hc(window(C18_ES$lc , start = c(2006,8), end = 2009)[,1:30])

