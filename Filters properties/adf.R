library(rjdfilters)
library(ggplot2)
library(reshape2)
library(patchwork)

horizons <- 5:6
horizon = 2
rm(list=ls())
all_result <- lapply(horizons, function(horizon){
  data <- do.call(rbind,
          lapply(c("Henderson", "Gaussian", "Trapezoidal",
                   "Triweight", "Tricube", "Biweight",
                   "Epanechnikov", "Triangular", "Uniform"),
                 function(kernel){
                   n_kernel <- kernel
                   if(kernel == "Epanechnikov")
                     n_kernel <- "Parabolic"
                   coefs <- sapply(0:(2*horizon-1),function(d){
                     print(sprintf("h=%i, kernel = %s, d=%i",horizon,n_kernel,d))
                     
                     k_coef <- lpp_properties(horizon = horizon,
                                                kernel = n_kernel,
                                                degree = d,
                                                endpoints = "DAF")$filters.coef
                     k_coef <- k_coef["t",]
                     k_coef
                   })
                   
                   coefs <- data.frame(q = rownames(coefs),round(coefs,3), kernel = n_kernel,
                                       stringsAsFactors = FALSE)
                   colnames(coefs) <- c("q",sprintf("d=%i", 0:(2*horizon-1)), "kernel")
                   rownames(coefs) <- NULL
                   coefs
                 })
  )
  data
})
names(all_result) <- sprintf("h=%i", horizons)
data <- all_result$`h=6`
data_q0 <- data[data$q=="q=0",]
data_q1 <- data[data$q=="q=1",]
data_q1_m_q0 <- data_q1[,-1]
data_q1_m_q0[,-ncol(data_q1_m_q0)] <- (data_q1[,-c(1,ncol(data))] - data_q0[,-c(1,ncol(data))]) / data_q0[,-c(1,ncol(data))]
data_q1_m_q0[,-ncol(data_q1_m_q0)] <- round(100*data_q1_m_q0[,-ncol(data_q1_m_q0)],1)
data_q1_m_q0

coef_daf <- function(horizon, endpoints = "DAF"){
  all_as_filters <- do.call(rbind,
                         lapply(c("Henderson", "Gaussian", "Trapezoidal",
                                  "Triweight", "Tricube", "Biweight",
                                  "Epanechnikov", "Triangular", "Uniform"),
                                function(kernel){
                                  n_kernel <- kernel
                                  if(kernel == "Epanechnikov")
                                    n_kernel <- "Parabolic"
                                  k_coef <- lpp_properties(horizon = horizon, kernel = n_kernel,
                                                             endpoints = endpoints)$filters.coef
                                  coefs_na = sapply(0:horizon, function(x){
                                    c(rep(1,horizon+1+x),rep(NA,nrow(k_coef)-(horizon+1+x)))
                                  })
                                  
                                  data.frame(k_coef*coefs_na, by = kernel,
                                             x = factor(rownames(k_coef),ordered = TRUE,levels = rownames(k_coef)),
                                             stringsAsFactors = F,row.names = NULL)
                                })
  )
  
  colnames(all_as_filters) <- gsub(".", "=", colnames(all_as_filters), fixed = TRUE)
  dataGraph <- reshape2::melt(all_as_filters, id = c("x","by"))
  dataGraph
}

plot_daf_filters <- function(data, horizon, kernel){
  y_lim <- range(data$value, na.rm = TRUE)
  title = sprintf("%s kernel (bandwidth h = %s)",kernel, horizon)
  
  ggplot(data = data[data$by==kernel,], aes(x = x, y = value, group = variable,
                                                        colour = variable)) + 
    scale_y_continuous(limits = y_lim) +
    geom_line(size = 0.7) +
    geom_point(size = 1) +
    # facet_wrap(~by) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Coefficients", title = title)
}

gain_daf <- function(horizon, endpoints = "DAF"){
  all_as_filters <- do.call(rbind,
                            lapply(c("Henderson", "Gaussian", "Trapezoidal",
                                     "Triweight", "Tricube", "Biweight",
                                     "Epanechnikov", "Triangular", "Uniform"),
                                   function(kernel){
                                     n_kernel <- kernel
                                     if(kernel == "Epanechnikov")
                                       n_kernel <- "Parabolic"
                                     k_gain <- lpp_properties(horizon = horizon, kernel = n_kernel,
                                                                endpoints = endpoints)$filters.gain
                                     data.frame(k_gain, by = kernel,
                                                x = seq(0, pi, length.out = nrow(k_gain)),
                                                stringsAsFactors = F,row.names = NULL)
                                   })
  )
  
  colnames(all_as_filters) <- gsub(".", "=", colnames(all_as_filters), fixed = TRUE)
  dataGraph <- reshape2::melt(all_as_filters, id = c("x","by"))
  dataGraph
}

plot_gain_daf <- function(data, horizon, kernel){
  y_lim <- range(data$value, na.rm = TRUE)
  nxlab = 7
  x_lab_at <- seq(0, 1, length.out = nxlab)
  xlabel <- function(x, symbol = "pi"){
    x <- x
    fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")  # convert to fractions
    labels <- sapply(fracs, function(i)
      if (length(i) > 1) { paste(i[1], "*", symbol, "/", i[2]) }
      else { paste(i, "*", symbol) })
    labels <- sub("0 * pi", "0", labels, fixed = TRUE)
    labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
    labels
  }
  
  ggplot(data = data[data$by==kernel,], aes(x = x, y = value, group = variable,
                                            colour = variable)) + 
    scale_y_continuous(limits = y_lim) +
    geom_line(size = 0.7) +
    # facet_wrap(~by) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Gain") +
    scale_x_continuous(NULL, 
                       breaks = x_lab_at*pi,
                       labels = parse(text=xlabel(x_lab_at)))
  
}
horizon = 6
kernels <- c("Henderson", "Gaussian", "Trapezoidal",
             "Triweight", "Tricube", "Biweight",
              "Epanechnikov", "Triangular", "Uniform")
data_coef <- coef_daf(horizon = horizon)
data_gain <- gain_daf(horizon = horizon)
for(i in seq_along(kernels)){
  p1 <- plot_daf_filters(data_coef, horizon = horizon,
                        kernel = kernels[i])
  p2 <- plot_gain_daf(data_gain, horizon = horizon,
                         kernel = kernels[i])
  p3 <- p1/p2
  print(p3)
  ggsave(filename = sprintf("Rapport de stage/img/daf/coef_gain_%s.pdf",i), p3,
         width = 8, height = 5)
}
