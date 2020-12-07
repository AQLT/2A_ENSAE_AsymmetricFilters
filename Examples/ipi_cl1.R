library(RJDemetra)
library(rjdfilters)
library(patchwork)
library(ggplot2)
library(zoo)
# ipi_fr <- AQLTools::lectureBDM("010537906",
#                      "010537908",
#                      "010537910",
#                      "010537912",
#                      "010537914",
#                      "010537940",
#                      "010537942")
# colnames(ipi_fr) <- paste0("C",c(1:5,"L1","L2"))
# saveRDS(ipi_fr, "Examples/ipi_cl1.RDS")
ipi_fr <- readRDS("Examples/ipi_cl1.RDS")
# ipi_c_eu <- eurostat::get_eurostat("sts_inpr_m",select_time = "M",
#                                    filters = list(nace_r2="C",
#                                                   unit = "I15", s_adj = "SCA",
#                                                   sinceTimePeriod = "1990M01"))
# ipi_c_eu <- reshape2::dcast(ipi_c_eu, time ~ geo,  value.var = "values")
# ipi_c_eu <- ts(ipi_c_eu[, c("BE", "BG", "CZ", "DK", "DE",
#                             "EE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU",
#                             "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE",
#                             "UK", "NO", "ME", "MK", "RS", "TR", "BA")],
#                start = c(1990, 1), frequency = 12)
# plot(ipi_c_eu[,"FR"])
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
AQLTools::hc_stocks(ipi_fr)


rollapply(ipi_c_eu[,"FR"], width=5,
          function(x){
            (x[1]>=x[2]) & (x[2]>=x[3]) &
              (x[3]<x[4]) & (x[4]<=x[5])
          })
tmp_asym <- rjdfilters::localpolynomials(ipi_c_eu[,"FR"],6, ic = 3.5)
pic <- rollapply(tmp_asym, width=5,
                 function(x){
                   (x[1]>=x[2]) & (x[2]>=x[3]) &
                     (x[3]<x[4]) & (x[4]<=x[5])
                 })
pic <- window(pic, start = 2008, end = 2010)
time(pic)[which(pic)]
spectral_pic <- function(secteur = "CL1", q,...){
  tmp_asym <- asymmetric_lp(ipi_fr[,secteur],6, ic = 3.5, q = q,...)
  pic <- rollapply(tmp_asym, width=5,
            function(x){
              (x[1]>=x[2]) & (x[2]>=x[3]) &
                (x[3]<x[4]) & (x[4]<=x[5])
            })
  pic <- window(pic, start = 2008.5, end = c(2009,12))
  time(pic)[which(pic)]
}

diff_pic <- function(secteur = "CL1", q,...){
  (spectral_pic(q=6)-
     spectral_pic(q=q, secteur = secteur,...))*12
}
diff_pic(q=0,endpoints = "CQ")
henderson_f <- lpp_properties(horizon = 6, kernel = "Henderson",ic = 3.5)
tmp_asym <- asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6)
hc_stocks(tmp_asym)
asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0) -
  stats::filter(ipi_fr[,"CL1"],rev(henderson_f$filters.coef[,"q=0"]))
sum(window(ipi_fr[,"CL1"],start = c(2019,6))*henderson_f$filters.coef[,"q=0"])
rkhs <- readRDS("FST/rkhs.RDS")
rkhs_apply <- function(y, component = c("frf", "gain", "phase"), q= 0){
  component=match.arg(component)
  coef <- rkhs[[component]]$weight[,sprintf("q=%i",q)]
  stats::filter(y,rev(coef))
}

data_daf <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0,endpoints = "DAF"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =1,endpoints = "DAF"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =2,endpoints = "DAF"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =3,endpoints = "DAF")
)
data_lc <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0,endpoints = "LC"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =1,endpoints = "LC"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =2,endpoints = "LC"),
                      asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =3,endpoints = "LC")
)
data_ql <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                     asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0,endpoints = "QL"),
                     asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =1,endpoints = "QL"),
                     asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =2,endpoints = "QL"),
                     asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =3,endpoints = "QL")
)
data_lc_t <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6,tweight = 2000),
                  asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0,tweight = 2000),
                  asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =1,tweight = 2000),
                  asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =2,tweight = 2000),
                  asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =3,tweight = 2000)
)

data_rkhs_phase <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =0),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =1),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =2),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =3)
)

colnames(data_daf) <- colnames(data_lc) <-
  colnames(data_ql) <- colnames(data_lc_t) <-colnames(data_rkhs_phase) <-
  c("sym", paste0("q=",0:3))
AQLTools::hc_stocks(data,digits = 1)

(spectral_pic(q=6, endpoints = "DAF")-spectral_pic(q=0, endpoints = "LC"))*12
diff_pic("ME", q=0, endpoints = "LC")
diff_pic("ME", q=0, endpoints = "QL")
diff_pic("ME", q=0, endpoints = "DAF")


AQLTools::hc_stocks(ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                              asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0),
                              asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q =0,endpoints = "DAF")),digits = 1)

AQLTools::hc_stocks(asymmetric_lp(ipi_c_eu[,"BE"],6, ic = 3.5, q = 6))
                              
AQLTools::hc_stocks(ts.union( asymmetric_lp(ipi_c_eu[,"FR"],6, ic = 3.5, q = 6),
                              asymmetric_lp(ipi_c_eu[,"FR"],6, ic = 3.5, q =0),
                              asymmetric_lp(ipi_c_eu[,"FR"],6, ic = 3.5, q =0,endpoints = "DAF")),digits = 1)

asymmetric_lp<-function(y,
                        horizon,
                        degree = 3,
                        kernel = c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),
                        endpoints = c("LC", "QL", "CQ", "CC", "DAF"),
                        ic = 3.5,
                        q = 0,
                        tweight = 0, passband = pi/12){
  first_date <- time(y)[1] + (horizon*2+q)/frequency(y)
  last_date <- time(y)[length(y)]-horizon/frequency(y)
  available_span <- time(ts(0, start =first_date,
                            end = last_date,
                            frequency = frequency(y)))
  ts(sapply(as.numeric(available_span), function(date_fin){
    res <- localpolynomials(window(y, end = date_fin),
                            horizon = horizon,
                            degree = degree,
                            kernel = kernel,
                            endpoints = endpoints,
                            ic = ic,
                            tweight = tweight,
                            passband = passband)
    res[length(res) - q]
  }),start = available_span[1]-q/frequency(y),
  frequency = frequency(y))

}
rkhs <- readRDS("FST/rkhs.RDS")
rkhs_apply <- function(y, component = c("frf", "gain", "phase"), q= 0){
  component=match.arg(component)
  coef <- rkhs[[component]]$weight[,sprintf("q=%i",q)]
  stats::filter(y,rev(coef))
}
data <- ts.union( asymmetric_lp(ipi_fr[,"CL1"],6, ic = 3.5, q = 6),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =0),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =1),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =2),
                  rkhs_apply(ipi_fr[,"CL1"],"phase", q =3)
)
colnames(data) <- c("sym", paste0("q=",0:3))
data <- window(data, start = 2006, end = 2012)
AQLTools::hc_stocks(data,digits = 1)
names(rkhs)

100000*(1+x)^20 = 199307
(199307/100000)^(1/20)-1

65000+ 12614 + 24000

61.7+17+9+18+23

