library(RJDemetra)
library(rjdfilters)
library(patchwork)
library(ggplot2)
library(zoo)
library(RColorBrewer)
library(Hmisc)

rkhs <- readRDS("FST/rkhs.RDS")
# rkhs_apply <- function(y, component = c("frf", "gain", "phase"), q= 0){
#   component=match.arg(component)
#   coef <- rkhs[[component]]$weight[,sprintf("q=%i",q)]
#   stats::filter(y,rev(coef))
# }
rkhs_apply <- function(y, component = c("frf", "gain", "phase"), q= 0){
    component=match.arg(component)
    coef <- rkhs[[component]]$weight[,sprintf("q=%i",q)]
    jasym_filter(y, coef, 6)
}
fst_apply <- function(y, q= 0){
    f <- fst_filter(lags = 6, leads = q, pdegree=2, 
                   smoothness.weight=1/1001, timeliness.weight = 1000/1001)$filter
    jasym_filter(y, f, 6)
}
asymmetric_lp<-function(y,
                        horizon,
                        degree = 3,
                        kernel = c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),
                        endpoints = c("LC", "QL", "CQ", "CC", "DAF", "CN"),
                        ic = 4.5,
                        q = 0,
                        tweight = 0, passband = pi/12){
    coef <- lp_filter(horizon = horizon, degree = degree,
                           kernel = kernel, endpoints = endpoints,
                           ic = ic, tweight = tweight,
                           passband = passband)
    coef <- coef$filters.coef[,sprintf("q=%i",q)]
    jasym_filter(y, coef, horizon)
}

ipi_fr <- readRDS("Examples/ipi_cl1.RDS")
ipi <- ipi_fr[,"CL1"]

data_daf <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                     asymmetric_lp(ipi,6, ic = 3.5, q =0,endpoints = "DAF"),
                     asymmetric_lp(ipi,6, ic = 3.5, q =1,endpoints = "DAF"),
                     asymmetric_lp(ipi,6, ic = 3.5, q =2,endpoints = "DAF"),
                     asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "DAF"),
                     asymmetric_lp(ipi,6, ic = 3.5, q =4,endpoints = "DAF"),
                     asymmetric_lp(ipi,6, ic = 3.5, q =5,endpoints = "DAF")
)
data_lc <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                    asymmetric_lp(ipi,6, ic = 3.5, q =0,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =1,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =2,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =4,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =5,endpoints = "LC")
)
data_ql <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                    asymmetric_lp(ipi,6, ic = 3.5, q =0,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =1,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =2,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =4,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =5,endpoints = "QL")
)

data_rkhs_gain <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                           rkhs_apply(ipi,"gain", q =0),
                           rkhs_apply(ipi,"gain", q =1),
                           rkhs_apply(ipi,"gain", q =2),
                           rkhs_apply(ipi,"gain", q =3),
                           rkhs_apply(ipi,"gain", q =4),
                           rkhs_apply(ipi,"gain", q =5)
)
data_rkhs_phase <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                            rkhs_apply(ipi,"phase", q =0),
                            rkhs_apply(ipi,"phase", q =1),
                            rkhs_apply(ipi,"phase", q =2),
                            rkhs_apply(ipi,"phase", q =3),
                            rkhs_apply(ipi,"phase", q =4),
                            rkhs_apply(ipi,"phase", q =5)
)
data_rkhs_frf <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                          rkhs_apply(ipi,"frf", q =0),
                          rkhs_apply(ipi,"frf", q =1),
                          rkhs_apply(ipi,"frf", q =2),
                          rkhs_apply(ipi,"frf", q =3),
                          rkhs_apply(ipi,"frf", q =4),
                          rkhs_apply(ipi,"frf", q =5)
)
data_fst <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                     fst_apply(ipi, q =0),
                     fst_apply(ipi, q =1),
                     fst_apply(ipi, q =2),
                     fst_apply(ipi, q =3),
                     fst_apply(ipi, q =4),
                     fst_apply(ipi, q =5)
)

colnames(data_daf) <- colnames(data_lc) <-
    colnames(data_ql) <-colnames(data_rkhs_gain) <-
    colnames(data_rkhs_phase) <- colnames(data_rkhs_frf) <-
    colnames(data_fst) <- 
    c("Final trend", paste0("q=",0:5))
date_fin <- c(2009,2)
dates_fin <- c(2009,2)

est_dynamique <- function(data_f){
    res <- do.call(ts.union, lapply(3:6, function(m_fin){
        data <- window(data_f, end = c(2009,m_fin))
        y <- data[,"Final trend"]
        n <- length(y)
        for(i in 0:5){
            y[n-i] <- data[n-i,paste0("q=",i)]  
        }
        y
    }))
    res <- window(ts.union(data_f[,"Final trend"], res), start = 2008, end = 2010)
    colnames(res) <- c("Final trend", "Mar", "Av", "Mai", "Juin")
    res
}



data_all <- list("DAF" = data_daf,
                 "LC" = data_lc,
                 "QL" = data_ql,
                 "b['q, '] [phi]" = data_rkhs_phase,
                 "b['q, '] [G]" = data_rkhs_gain,
                 "b['q, '] [gamma]" = data_rkhs_frf,
                 "FST" = data_fst)
data_all <- lapply(data_all, est_dynamique)

graph_glob <- function(data, titre = NULL, sous_titre = NULL, legende = NULL, afficheVolatilite = FALSE,
                       cex = 0.6, x_lab = NULL, x_lab_month = FALSE, y_lab = NULL,
                       outDec = ",",
                       n_xlabel = length(time(data)) %/% 24, n_ylabel = 12,
                       geom_size = 0.7,
                       ylimits = NULL){
    
    time <- time(data)
    freq <- frequency(data)
    dataGraph <- data.frame(cbind(time, data))
    if (is.null(legende)){
        if(is.mts(data)){
            legende <- colnames(data)
        }else{
            legende <- ""
        }
    }
    colnames(dataGraph) <- c("date", legende)
    
    dataGraph <- reshape2::melt(dataGraph, id="date")  # convert to long format

        if (freq==1){
        periode <- "Y"
        periode <- factor(periode)
    }
    if (freq==2){
        periode <- ifelse(time(data)%%1==0, "S1", "S2")
        periode <- factor(periode,levels = c("S1","S2"), ordered = T)
    }
    if (freq==4){
        periode <- capitalize(quarters(zoo::as.yearqtr(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(quarters(zoo::as.yearqtr((0:3)/4))),ordered = T)
    }
    if (freq==12){
        periode <- capitalize(months(zoo::as.yearmon(dataGraph$date)))
        periode <- factor(periode,levels=capitalize(months(zoo::as.yearmon((0:11)/12))),ordered = T)
    }
    
    dataGraph <- data.frame(dataGraph,periode=periode)
    p <- ggplot(data = dataGraph, aes(x = date, y = value, group = variable,
                                      color = variable,
                                      linetype = variable,
                                      fill = variable
    )) + 
        scale_linetype_manual(values=c("solid",rep("dashed",ncol(data)-1)))+
        geom_line(size=geom_size) +
        labs(title = titre, subtitle = sous_titre,
             x = x_lab, y = y_lab) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = n_xlabel),
                           labels = function(x) AQLTools:::creation_x_label(x, x_lab_month = x_lab_month, outDec = outDec)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = n_ylabel),
                           labels = function(x) format(x, decimal.mark = outDec),
                           limits = ylimits)+
        AQLTools:::theme_aqltools()
    p+
        scale_colour_manual(breaks = colnames(data),
                            values = c("#000000","#ADCDE6","#0072B2", "#D55E00", "#009E73")
        )+
        guides(color=guide_legend(ncol=5))
}
ylim1 <- range(sapply(data_all, function(data) range(data,na.rm = TRUE)))

for (i in seq_along(data_all)){
    data <- data_all[[i]]
    p <- graph_glob(data, n_xlabel = 8,x_lab_month = TRUE,
                    titre = parse(text=sprintf("%s~~filter", names(data_all)[i])),
                    ylimits = ylim1) 
    ggsave(filename = sprintf("Rapport de stage/img/illustration/est_tr_%i.pdf",i),
           p,
           width = 8, height = 3)
}

AQLTools::hc_stocks(data_all$DAF,digits = 1)
AQLTools::hc_stocks(data_all$LC,digits = 1)
AQLTools::hc_stocks(data_all$QL,digits = 1)
AQLTools::hc_stocks(data_all$`b['q, '] [phi]`,digits = 1)
AQLTools::hc_stocks(data_all$FST,digits = 1)


