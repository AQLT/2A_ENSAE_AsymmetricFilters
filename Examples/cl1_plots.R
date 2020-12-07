library(RJDemetra)
library(rjdfilters)
library(patchwork)
library(ggplot2)
library(zoo)
library(RColorBrewer)

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
  f <- fstfilter(lags = 6, leads = q, pdegree=2, 
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
  coef <- lpp_properties(horizon = horizon, degree = degree,
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
                     asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "DAF")
)
data_lc <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                    asymmetric_lp(ipi,6, ic = 3.5, q =0,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =1,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =2,endpoints = "LC"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "LC")
)
data_ql <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                    asymmetric_lp(ipi,6, ic = 3.5, q =0,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =1,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =2,endpoints = "QL"),
                    asymmetric_lp(ipi,6, ic = 3.5, q =3,endpoints = "QL")
)

data_rkhs_gain <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                            rkhs_apply(ipi,"gain", q =0),
                            rkhs_apply(ipi,"gain", q =1),
                            rkhs_apply(ipi,"gain", q =2),
                            rkhs_apply(ipi,"gain", q =3)
)
data_rkhs_phase <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                            rkhs_apply(ipi,"phase", q =0),
                            rkhs_apply(ipi,"phase", q =1),
                            rkhs_apply(ipi,"phase", q =2),
                            rkhs_apply(ipi,"phase", q =3)
)
data_rkhs_frf <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                            rkhs_apply(ipi,"frf", q =0),
                            rkhs_apply(ipi,"frf", q =1),
                            rkhs_apply(ipi,"frf", q =2),
                            rkhs_apply(ipi,"frf", q =3)
)
data_fst <- ts.union(asymmetric_lp(ipi,6, ic = 3.5, q = 6),
                          fst_apply(ipi, q =0),
                          fst_apply(ipi, q =1),
                          fst_apply(ipi, q =2),
                          fst_apply(ipi, q =3)
)

colnames(data_daf) <- colnames(data_lc) <-
  colnames(data_ql) <-colnames(data_rkhs_gain) <-
  colnames(data_rkhs_phase) <- colnames(data_rkhs_frf) <-
  colnames(data_fst) <- 
  c("Final trend", paste0("q=",0:3))


AQLTools::hc_stocks(window(data_daf,start = 2008, end = 2010),
                    digits = 1)
AQLTools::hc_stocks(window(data_lc,start = 2007, end = 2010),
                    digits = 1)
AQLTools::hc_stocks(window(data_ql,start = 2007, end = 2010),
                    digits = 1)
AQLTools::hc_stocks(window(data_rkhs_frf,start = 2007, end = 2010),
                    digits = 1)
AQLTools::hc_stocks(window(data_fst,start = 2007, end = 2010),
                    digits = 1)
AQLTools::hc_stocks(data_rkhs_phase,digits = 1)

data_all <- list("DAF" = data_daf,
             "LC" = data_lc,
             "QL" = data_ql,
             "b['q, '] [phi]" = data_rkhs_phase,
             "b['q, '] [G]" = data_rkhs_gain,
             "b['q, '] [gamma]" = data_rkhs_frf,
             "FST" = data_fst)
AQLTools::hc_stocks(data_all$FST,digits = 1)
AQLTools::hc_stocks(data_all$`b['q, '] [phi]`,digits = 1)



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
    scale_colour_manual(breaks = colnames(data_daf),
                        values = c("#000000","#ADCDE6","#0072B2", "#D55E00", "#009E73")
    )+
    guides(color=guide_legend(ncol=5))
}
ylim1 <- range(sapply(data_all, function(data) range(window(data,start = 2008, end = 2010))))
ylim2 <- range(sapply(data_all, function(data) range(window(data,start = 2000, end = c(2007,12)))))

for (i in seq_along(data_all)){
  data <- data_all[[i]]
  p <- graph_glob(window(data,start = 2008, end = 2010),n_xlabel = 8,x_lab_month = TRUE,
                  titre = parse(text=sprintf("%s~~filter", names(data_all)[i])),
                  ylimits = ylim1) /
    graph_glob(window(data,start = 2000, end = c(2007,12)),
               n_xlabel = 8,x_lab_month = TRUE,
               ylimits = ylim2) + theme(legend.position = "none")
  ggsave(filename = sprintf("Rapport de stage/img/illustration/illustration_%i.pdf",i),
         p,
         width = 8, height = 6)
}



spectral_pic <- function(x){
  pic <- rollapply(x, width=5,
                   function(x){
                     (x[1]>=x[2]) & (x[2]>=x[3]) &
                       (x[3]<x[4]) & (x[4]<=x[5])
                   })
  pic <- window(pic, start = 2008.5, end = c(2009,12))
  time(pic)[which(pic)]
}
spectral_lag <- sapply(data_all, function(data){
  ref <- data[,1]
  others <- data[,-1]
  sapply(others,function(x){
    (spectral_pic(ref) - spectral_pic(x)[1])*12
  })
})
spectral_lag

revision_crisis <- sapply(data_all, function(data){
  data <- window(data, start = 2008, end = 2010)
  ref <- data[,1]
  others <- data[,-1]
  sapply(others,function(x){
    sqrt(mean((ref - x)^2))
  })
})
revision_crisis2 <- sapply(data_all, function(data){
  data <- window(data, start = c(2008,8), end = c(2009,8))
  ref <- data[,1]
  others <- data[,-1]
  sapply(others,function(x){
    sqrt(mean((ref - x)^2))
  })
})
revision <- sapply(data_all, function(data){
  data <- window(data,start = 2000, end = c(2007,12))
  ref <- data[,1]
  others <- data[,-1]
  sapply(others,function(x){
    sqrt(mean((ref - x)^2))
  })
})
ecarttype <- sapply(data_all, function(data){
  data <- window(data,start = 2000, end = c(2007,12))
  others <- data[,-1]
  ref <- data[,1]
  sapply(others,function(x){
    sd(ref - x)
  })
})
round(revision_crisis2,1)
round(revision_crisis,1)
round(revision,1)
round(ecarttype, 1)

round(revision_crisis,1)
round(revision,1)


diff_pic <- function(secteur = "CL1", q,...){
  (spectral_pic(q=6)-
     spectral_pic(q=q, secteur = secteur,...))*12
}
diff_pic(q=0,endpoints = "CQ")

f$filter
