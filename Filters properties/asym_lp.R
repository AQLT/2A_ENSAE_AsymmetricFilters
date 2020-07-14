library(RJDemetra)
library(rjdfilters)
library(AQLTools)
library(patchwork)
get_all_asym_filters <- function(sa, icr, q, horizon){
  all_as_res <- lapply(c("Henderson", "Gaussian", "Trapezoidal",
                         "Triweight", "Tricube", "Biweight",
                         "Parabolic", "Triangular", "Uniform"),
                       function(kernel){
                         res <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
                           print(kernel)
                           print(endpoints)
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
  names(all_as_res) <- c("Henderson", "Gaussian", "Trapezoidal",
                          "Triweight", "Tricube", "Biweight",
                          "Parabolic", "Triangular", "Uniform")
  all_as_res
}

x <- ipi_c_eu[, "FR"]
spec <- x13_spec("RSA4c",easter.enabled = FALSE)
mysa <- jx13(x, spec = spec)
get_dictionary(mysa)
indicators <- get_indicators(mysa,"sa","t","i", "diagnostics.ic-ratio-henderson",
               "diagnostics.ic-ratio")
get_indicators(mysa,"decomposition.d12filter",
               "decomposition.tlen")
icr <- indicators$`diagnostics.ic-ratio`
indicators$`diagnostics.ic-ratio-henderson`
sa <- indicators$sa
icr <- sum(abs(diff(indicators$i,1)))/sum(abs(diff(indicators$t,1)))
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
  add_trend
})
names(all_asym_filters) <- c("Henderson", "Gaussian", "Trapezoidal",
                             "Triweight", "Tricube", "Biweight",
                             "Epanechnikov", "Triangular", "Uniform")
plot_as_filter <- function(data, kernel, y_lim){
  data <- data[[kernel]]
  time <- time(data)
  freq <- frequency(data)
  dataGraph <- data.frame(cbind(time, data))
  colnames(dataGraph) <- c("date", colnames(data))
  dataGraph <- reshape2::melt(dataGraph, id = "date")
  
  all_p_b <- lapply(colnames(data)[-1],function(method){
    data_tmp <- data.frame(cbind(time, data[,c("Symmetric trend",
                                               method)]))
    colnames(data_tmp) <- c("date","y_line","y_bar")
    mean_td <- mean(data_tmp$y_line)
    sd_td <- sd(data_tmp$y_line)
    ggplot(data =data_tmp , aes(x = date, y = y_bar)) + 
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title = sprintf("%s - %s", kernel, method), x = NULL, 
           y = "Error")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
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
y_lim <- range(sapply(all_asym_filters,function(x)range(x[,-1],na.rm=TRUE)))
y_lim <- max(abs(y_lim))
y_lim <- c(-y_lim, y_lim)
for(i in seq_along(kernels)){
  print(kernels[i])
  p1 <- plot_as_filter(all_asym_filters, kernels[i],y_lim)
  ggsave(filename = sprintf("Rapport de stage/img/daf/comp_assym_%s.pdf",i), p1,
         width = 8, height = 8)
}
res$Henderson

time(trend)[1]
plot(irr)
plot(sa)
lines(trend, col = "blue")

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
