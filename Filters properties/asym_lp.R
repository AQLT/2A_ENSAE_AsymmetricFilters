library(RJDemetra)
library(rjdfilters)
library(AQLTools)
library(patchwork)
library(ggplot2)
ipi_c_eu <- eurostat::get_eurostat("sts_inpr_m",select_time = "M",
                                   filters = list(nace_r2="C",
                                                  unit = "I15", s_adj = "CA",
                                                  sinceTimePeriod = "1990M01"))
ipi_c_eu <- reshape2::dcast(ipi_c_eu, time ~ geo,  value.var = "values")
ipi_c_eu <- ts(ipi_c_eu[, c("BE", "BG", "CZ", "DK", "DE",
                            "EE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU",
                            "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE",
                            "UK", "NO", "ME", "MK", "RS", "TR", "BA")],
               start = c(1990, 1), frequency = 12)
# Last date is removed due to NA:
ipi_c_eu <- window(ipi_c_eu, end = c(2019,12))
get_all_asym_filters <- function(sa, icr, q, horizon,
                                 kernels = c("Henderson", "Gaussian", "Trapezoidal",
                                             "Triweight", "Tricube", "Biweight",
                                             "Parabolic", "Triangular", "Uniform")){
  all_as_res <- lapply(kernels,
                       function(kernel){
                         res <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
                           asymmetric_lp(y=sa, horizon = horizon, degree = 3,
                                         kernel = kernel,
                                         endpoints = endpoints,
                                         ic = icr,
                                         q = q
                           )
                         })
                         res <- ts(simplify2array(res), start = start(res[[1]]), frequency = frequency(res[[1]]))
                         colnames(res) <- c("LC", "QL", "CQ", "DAF")
                         res
                       })
  names(all_as_res) <- kernels
  all_as_res
}

x <- ipi_c_eu[, "FR"]
mysa <- jx13(x, spec = "RSA3")
indicators <- get_indicators(mysa,"sa","t","i", "diagnostics.ic-ratio-henderson",
               "diagnostics.ic-ratio")
get_indicators(mysa,"decomposition.d12filter",
               "decomposition.tlen")
indicators$`diagnostics.ic-ratio-henderson`
sa <- indicators$sa
# icr <- sum(abs(diff(indicators$i,1)))/sum(abs(diff(indicators$t,1)))
icr <- indicators$`diagnostics.ic-ratio`
all_asym_filters <- get_all_asym_filters(sa, icr, q=0, horizon = 6)
all_asym_filters <- lapply(names(all_asym_filters), function(kernel){
  sym_trend <- localpolynomials(sa,
                   horizon = 6,
                   degree = 3,
                   kernel = kernel,
                   endpoints = "LC",
                   ic = icr)
  add_trend <- ts.intersect(sym_trend, all_asym_filters[[kernel]])
  colnames(add_trend) <- c("Symmetric trend", colnames(all_asym_filters[[kernel]]))
  for(i in seq_len(ncol(add_trend))[-1]){
    add_trend[,i] <- (add_trend[,1] - add_trend[,i])
  }
  window(add_trend,start = 2003)
})
names(all_asym_filters) <- c("Henderson", "Gaussian", "Trapezoidal",
                             "Triweight", "Tricube", "Biweight",
                             "Epanechnikov", "Triangular", "Uniform")
plot_as_filter <- function(data, kernel, puissance = 1){
  y_lim <- range(sapply(data,function(x)range((x[,-1])^puissance,na.rm=TRUE)))
  y_lim <- max(abs(y_lim))
  y_lim <- c(-y_lim, y_lim)
  data <- data[[kernel]]
  time <- time(data)
  freq <- frequency(data)
  
  all_p_b <- lapply(colnames(data)[-1],function(method){
    data_tmp <- data.frame(cbind(time, data[,c("Symmetric trend",
                                               method)]))
    colnames(data_tmp) <- c("date","y_line","y_bar")
    data_tmp$y_bar <- data_tmp$y_bar ^ puissance
    
    mean_td <- mean(data_tmp$y_line)
    sd_td <- sd(data_tmp$y_line)/ (4^(puissance - 1))
    # if(squared){
    #   
    # }else{
    #   sd_td <- sd(data_tmp$y_line)
    # }
    
    ggplot(data = data_tmp , aes(x = date, y = y_bar)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title = sprintf("%s - %s", kernel, method), x = NULL, 
           y = "Error")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 18))+
      scale_y_continuous(limits = y_lim,
                         breaks = scales::pretty_breaks(n = 6),
                         sec.axis = sec_axis(~.*sd_td+mean_td, name = "Symmetric trend",
                                             breaks = scales::pretty_breaks(n = 6))) +
      AQLTools:::theme_aqltools() +
      guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) +
      geom_line(aes(y = (y_line-mean_td)/sd_td, colour = "Symmetric trend")) 
  })
  Reduce(`/`, all_p_b)
}
kernels <- c("Henderson", "Gaussian", "Trapezoidal",
             "Triweight", "Tricube", "Biweight",
             "Epanechnikov", "Triangular", "Uniform")

for(i in seq_along(kernels)){
  print(kernels[i])
  p1 <- plot_as_filter(all_asym_filters, kernels[i],puissance = 1)
  ggsave(filename = sprintf("Rapport de stage/img/daf/comp_assym_%s.pdf",i), p1,
         width = 8, height = 8)
}
for(i in seq_along(kernels)){
  print(kernels[i])
  p1 <- plot_as_filter(all_asym_filters, kernels[i],puissance = 2)
  ggsave(filename = sprintf("Rapport de stage/img/daf/comp_assym_square_%s.pdf",i), p1,
         width = 8, height = 8)
}
errors_s <- sapply(all_asym_filters,function(x){
  c(apply(x[,-1]^2,2,mean))
})
errors_s <- sapply(all_asym_filters,function(x){
  c(apply(window(x[,-1],2007,2011)^2,2,mean))
})
errors_s <- sapply(all_asym_filters,function(x){
  c(apply(abs(x[,-1]),2,mean))
})
# errors_s <- sapply(all_asym_filters,function(x){
#   x <- window(x[,-1],2007,2010)
#   window(x,start = c(2008,8), end = c(2008,8)) <- NA
#   c(apply(x^2,2,mean,na.rm = TRUE))
# })
round(errors_s,2)
apply(errors_s[,-c(3,9)],1,which.max)
apply(errors_s,1,which.min)

apply(errors_s,2,which.min)
apply(errors_s,2,which.max)

x <- all_asym_filters[[1]]
data <- ts.union(all_asym_filters[[1]][,1],all_asym_filters[[1]][,-1]+all_asym_filters[[1]][,1])
colnames(data) <- colnames(all_asym_filters[[1]])
AQLTools::hc_stocks(data,digits = 1)

AQLTools::hc_stocks(all_asym_filters[[1]][,-1]^2,digits = 1)
time(trend)[1]
plot(irr)
plot(sa)
lines(trend, col = "blue")

error_ipi_crisis <- sapply(colnames(ipi_c_eu),function(serie){
  print(serie)
  x <- ipi_c_eu[, serie]
  mysa <- jx13(x, spec = "RSA3")
  indicators <- get_indicators(mysa,"sa","t","i", "diagnostics.ic-ratio-henderson",
                               "diagnostics.ic-ratio")
  get_indicators(mysa,"decomposition.d12filter",
                 "decomposition.tlen")
  indicators$`diagnostics.ic-ratio-henderson`
  sa <- indicators$sa
  # icr <- sum(abs(diff(indicators$i,1)))/sum(abs(diff(indicators$t,1)))
  icr <- indicators$`diagnostics.ic-ratio`
  all_asym_filters <- get_all_asym_filters(sa, icr, q=0, horizon = 6,
                                           kernels = "Henderson")
  all_asym_filters <- lapply(names(all_asym_filters), function(kernel){
    sym_trend <- localpolynomials(sa,
                                  horizon = 6,
                                  degree = 3,
                                  kernel = kernel,
                                  endpoints = "LC",
                                  ic = icr)
    add_trend <- ts.intersect(sym_trend, all_asym_filters[[kernel]])
    colnames(add_trend) <- c("Symmetric trend", colnames(all_asym_filters[[kernel]]))
    for(i in seq_len(ncol(add_trend))[-1]){
      add_trend[,i] <- (add_trend[,1] - add_trend[,i])
    }
    window(add_trend,start = 2003,extend = TRUE)
  })
  names(all_asym_filters) <- c("Henderson")
  c(apply(window(all_asym_filters[["Henderson"]][,-1],2007,2011)^2,2,mean))
})
round(error_ipi_crisis,2)

sum(error_ipi_crisis[2,] < error_ipi_crisis[3,])
ncol(error_ipi_crisis)
bw <- bandwidth(icr)
bandwidth <- function(icr){
  bw <- c(4, 6, 12)
  cuts <- c(-Inf, 1, 3.5, Inf)
  bw[findInterval(icr, cuts)]
}
kernel = c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube",
           "Gaussian", "Triangular", "Parabolic")
endpoints = c("LC", "QL", "CQ", "CC", "DAF")
available_span
date_fin <- available_span[1]
horizon <- 6
kernel = "Henderson"
endpoints = "DAF"
q = 0
plot(indicators$t)
lines(as_tend, col = "red")

rm(list = ls())
kernel = "Henderson"

res <- do.call(ts.union,lapply(c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube",
         "Gaussian", "Triangular", "Parabolic"), function(kernel){
  sym_trend <- localpolynomials(sa,
                                horizon = 6,
                                degree = 3,
                                kernel = kernel,
                                endpoints = "LC",
                                ic = icr)
}))
colnames(res) <- c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube",
                                   "Gaussian", "Triangular", "Parabolic")
AQLTools::graph_ts(res)
AQLTools::hc_lines(res)
AQLTools::hc_stocks(res,digits = 2)
res2 <- res - res[,"Henderson"]
AQLTools::hc_stocks(res2,digits = 2)
                                 