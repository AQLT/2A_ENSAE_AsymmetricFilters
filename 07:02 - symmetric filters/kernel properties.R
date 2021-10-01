library(rjdfilters)
library(ggplot2)
library(reshape2)
get_all_kernels <- function(horizon){
  x <- sprintf("t%+i", seq(-horizon,horizon))
  x <- sub("+0", "", x, fixed = TRUE)
  x <- factor(x,levels = x, ordered = TRUE)
  all_kernels <- do.call(rbind,
                         lapply(c("Henderson","Uniform", "Triangular",
                                  "Epanechnikov","Biweight", "Triweight","Tricube",
                                  "Trapezoidal", "Gaussian"),
                                function(kernel){
                                  k_coef <- get_kernel(kernel, horizon)$coef
                                  k_coef <- c(rev(k_coef[-1]), k_coef)
                                  if(kernel == "Gaussian")
                                    k_coef <- k_coef/sum(k_coef)
                                  data.frame(x = seq(-horizon,horizon), y = k_coef, 
                                             kernel = factor(kernel,
                                                             levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                        "Triweight", "Tricube", "Biweight",
                                                                        "Epanechnikov", "Triangular", "Uniform"),
                                                             ordered = TRUE),
                                             h = horizon,
                                             stringsAsFactors = FALSE)
                                })
  )
  all_kernels
}
get_all_sfilters <- function(horizon){
  x <- sprintf("t%+i", seq(-horizon,horizon))
  x <- sub("+0", "", x, fixed = TRUE)
  x <- factor(x,levels = x, ordered = TRUE)
  all_kernels <- do.call(rbind,
                         lapply(c("Henderson","Uniform", "Triangular",
                                  "Epanechnikov","Biweight", "Triweight","Tricube",
                                  "Trapezoidal", "Gaussian"),
                                function(kernel){
                                  n_kernel <- kernel
                                  if(kernel == "Epanechnikov")
                                    n_kernel <- "Parabolic"
                                  tryCatch({
                                    k_coef <- lp_filter(horizon = horizon, kernel = n_kernel)$filters.coef
                                    k_coef <- k_coef[,ncol(k_coef)]
                                    
                                    data.frame(x = seq(-horizon,horizon), y = (k_coef),
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE),
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                  }, error = function(e){
                                    data.frame(x = seq(-horizon,horizon), y = NA,
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE), 
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                  })
                                  
                                })
  )
}
get_all_gain_sfilters <- function(horizon, xlim = c(0, 2*pi/12), resolution = 80){
  all_gain <- do.call(rbind,
                      lapply(c("Henderson","Uniform", "Triangular",
                               "Epanechnikov","Biweight", "Triweight","Tricube",
                               "Trapezoidal", "Gaussian"),
                             function(kernel){
                               n_kernel <- kernel
                               if(kernel == "Epanechnikov")
                                 n_kernel <- "Parabolic"
                               x_values <- seq(from = xlim[1], to = xlim[2],
                                               length.out = resolution)
                               k_f <- lp_filter(horizon = horizon, kernel = n_kernel)
                               k_gain <- get_properties_function(k_f, "Symmetric Gain")
                               
                               data.frame(x = x_values, y = k_gain(x_values),
                                          kernel = factor(kernel,
                                                          levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                     "Triweight", "Tricube", "Biweight",
                                                                     "Epanechnikov", "Triangular", "Uniform"),
                                                          ordered = TRUE),
                                          h = horizon,
                                          stringsAsFactors = FALSE)
                             }))
  xlabel <- function(x, symbol = "pi"){
    x <- x/pi
    fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")  # convert to fractions
    labels <- sapply(fracs, function(i)
      if (length(i) > 1) { paste(i[1], "*", symbol, "/", i[2]) }
      else { paste(i, "*", symbol) })
    labels <- sub("0 * pi", "0", labels, fixed = TRUE)
    labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
    labels
  }
  x_lab_at <- c(0,2*pi/(12*7),2*pi/36, 2 * pi / 24, 2*pi / 16, pi/6)
  x_lab <- c("0", "2 * pi / 84", "2 * pi / 36", "2 * pi / 24", "2*pi / 16", "2* pi /12")
  title = sprintf("bandwidth h = %s (filter of order %s)",horizon, 2*horizon+1)
  ggplot(data = all_gain, aes(x = x, y = y)) +
    ylim(0,1.05) +
    geom_vline(xintercept=2*pi/36, linetype="dashed")+
    geom_vline(xintercept=2*pi/(12*7), linetype="dashed") +
    geom_line(size = 0.7) + 
    scale_x_continuous(NULL, 
                       breaks = x_lab_at,
                       labels = parse(text=x_lab),
                       limits = c(xlim)) +
    facet_wrap(~kernel, scales = "free_x") +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Gain", title = title)
}
get_all_gain_sfilters2 <- function(horizon){
  all_gain <- do.call(rbind,
                         lapply(c("Henderson","Uniform", "Triangular",
                                  "Epanechnikov","Biweight", "Triweight","Tricube",
                                  "Trapezoidal", "Gaussian"),
                                function(kernel){
                                  n_kernel <- kernel
                                  if(kernel == "Epanechnikov")
                                    n_kernel <- "Parabolic"
                                  
                                  k_f <- lp_filter(horizon = horizon, kernel = n_kernel)
                                  k_gain <- k_f$filters.gain
                                  k_gain <- k_gain[,ncol(k_gain)]
                                  x_values <- seq(from = 0, to = pi,
                                                  length.out = length(k_gain))
                                  

                                  data.frame(x = x_values, y = k_gain,
                                             kernel = factor(kernel,
                                                             levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                        "Triweight", "Tricube", "Biweight",
                                                                        "Epanechnikov", "Triangular", "Uniform"),
                                                             ordered = TRUE),
                                             h = horizon,
                                             stringsAsFactors = FALSE)
                                }))
  xlabel <- function(x, symbol = "pi"){
    # x <- x/pi
    fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")  # convert to fractions
    labels <- sapply(fracs, function(i)
      if (length(i) > 1) { paste(i[1], "*", symbol, "/", i[2]) }
      else { paste(i, "*", symbol) })
    labels <- sub("0 * pi", "0", labels, fixed = TRUE)
    labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
    labels
  }
  x_lab_at <- c(0,2*pi/(12*7),2*pi/36, 2 * pi / 24, 2*pi / 16, pi/6)
  x_lab <- c("0", "2 * pi / 84", "2 * pi / 36", "2 * pi / 24", "2*pi / 16", "2* pi /12")
  nxlab = 7
  x_lab_at <- sort(c(2/36,2/(12*7),seq(0, 1, length.out = nxlab)))
  x_lab <- xlabel(x_lab_at)
  x_lab[2:3] <- c("2 * pi / 84", "2 * pi / 36")
  
  x_lab_at <- seq(0, 1, length.out = nxlab)
  x_lab <- xlabel(x_lab_at)
  
  title = sprintf("bandwidth h = %s (filter of order %s)",horizon, 2*horizon+1)
  ggplot(data = all_gain, aes(x = x, y = y)) +
    ylim(0,1.05) +
    geom_vline(xintercept=2*pi/36, linetype="dashed")+
    geom_vline(xintercept=2*pi/(12*7), linetype="dashed") +
    geom_line(size = 0.7)  + 
    scale_x_continuous(NULL, 
                       breaks = x_lab_at*pi,
                       labels = parse(text=x_lab))+
    facet_wrap(~kernel, scales = "free_x") +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Gain", title = title)
}
variance_reduction <- function(){
  all_var_red <- do.call(rbind,lapply(2:30, function(horizon){
    all_kernels <- do.call(rbind,
                           lapply(c("Henderson","Uniform", "Triangular",
                                    "Epanechnikov","Biweight", "Triweight","Tricube",
                                    "Trapezoidal", "Gaussian"),
                                  function(kernel){
                                    n_kernel <- kernel
                                    if(kernel == "Epanechnikov")
                                      n_kernel <- "Parabolic"
                                    diag <- lp_filter(horizon = horizon, kernel = n_kernel)$filters.diagnostics
                                    
                                    data.frame(variance_reduction = diag[1,3],
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE),
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                    
                                  }))
  }
    ))
    d <- dcast(all_var_red,h ~kernel, value.var = "variance_reduction")
    saveRDS(d,file = "Rapport de stage/data/var_red_sym_filters.RDS")
}
rapport_coeff <- function(){
  all_var_red <- do.call(rbind,lapply(2:30, function(horizon){
    all_kernels <- do.call(rbind,
                           lapply(c("Henderson","Uniform", "Triangular",
                                    "Epanechnikov","Biweight", "Triweight","Tricube",
                                    "Trapezoidal", "Gaussian"),
                                  function(kernel){
                                    n_kernel <- kernel
                                    if(kernel == "Epanechnikov")
                                      n_kernel <- "Parabolic"
                                    k_coef <- lp_filter(horizon = horizon, kernel = n_kernel)$filters.coef
                                    k_coef <- k_coef[1:(horizon+1),ncol(k_coef)]
                                    k_coef_max <- max(k_coef)
                                    k_coef_neg <- k_coef[k_coef<= 0]
                                    data.frame(mean_ratio_coef = max(abs(k_coef_neg / k_coef_max)),
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE),
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                    
                                  }))
  }
  ))
  d <- dcast(all_var_red,h ~kernel, value.var = "mean_ratio_coef")
  d[,-1] <- (1-d[,-1])*100
  round(d,0)
  # saveRDS(d,file = "Rapport de stage/data/var_red_sym_filters.RDS")
}
plot_filter <- function(data, horizon){
  title = sprintf("bandwidth h = %s (filter of order %s)",horizon, 2*horizon+1)
  ggplot(data = data, aes(x = x, y = y)) +
    geom_line(size = 0.7) +
    facet_wrap(~kernel, scales = "free_x") +
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
for(h in 2:30){
  print(h)
  p <- plot_filter(data = get_all_kernels(h), h)
  ggsave(filename = sprintf("Rapport de stage/img/kernels/%s.pdf",h), p,
         width = 8, height = 8)
}

for(h in 2:30){
  print(h)
  p <- plot_filter(get_all_sfilters(h), h)
  ggsave(filename = sprintf("Rapport de stage/img/symmetricFilters/%s.pdf",h), p,
         width = 8, height = 8)
}

for(h in 2:30){
  print(h)
  p <- get_all_gain_sfilters2(h)
  ggsave(filename = sprintf("Rapport de stage/img/symmetricFilters/gain%s.pdf",h), p,
         width = 8, height = 8)
}

for(h in 2:30){
  print(h)
  p <- plot_filter(data = get_all_kernels(h), h)
  ggsave(filename = sprintf("Stage_2A/img/kernels/%s.png",h), p,
         width = 8, height = 8)
}

for(h in 2:30){
  print(h)
  p <- plot_filter(get_all_sfilters(h), h)
  ggsave(filename = sprintf("Stage_2A/img/symmetricFilters/%s.png",h), p,
         width = 8, height = 8)
}

for(h in 2:30){
  print(h)
  p <- get_all_gain_sfilters2(h)
  ggsave(filename = sprintf("Stage_2A/img/symmetricFilters/gain%s.png",h), p,
         width = 8, height = 8)
}
get_all_gain_sfilters(3, resolution = 500)
get_all_gain_sfilters(3, resolution = 80)
get_all_gain_sfilters(3, resolution = 30)

plot_filter(get_all_filters(2),2)
plot_all_kernels(5)

for(h in 3:10){
  p <- plot_filter(get_all_sfilters(h), h)
  print(p)
}



library(gganimate)
all_h_kernels <- do.call(rbind, lapply(1:20,get_all_kernels))
unique(all_h_kernels$h)
table(all_h_kernels$h)
ggplot(data = all_h_kernels, aes(x = x, y = y)) +
  geom_line(size = 0.7) +
  geom_point(size = 1) +
  facet_wrap(~kernel) +
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid.major = element_line(colour = "grey92"),
        panel.grid.minor = element_line(colour = "grey92",
                                        size = 0.25),
        strip.background = element_rect(fill = "grey85", colour = "grey20"),
        complete = TRUE, plot.title = element_text(hjust = 0.5),
        legend.title=element_blank()) +
  transition_manual(horizon) +
  ease_aes('linear')

horizon = 5
c("Triangular", "Triweight","Tricube")

specific_kernel(100,kernels = c("Epanechnikov","Biweight", "Triweight"))
specific_filter(14,kernels = c("Epanechnikov","Trapezoidal"))
p1 <- specific_filter(6,kernels = c("Henderson","Triweight", "Biweight"),
                      degree = 3)
specific_filter(6, kernels = c("Henderson","Triweight", "Biweight"),
                degree = 3)
comp <- t(sapply(3:50, function(h){
  c(sum((get_kernel("Henderson", h)$coef - get_kernel("Triweight", h)$coef)^2),
    sum((get_kernel("Henderson", h)$coef - get_kernel("Biweight", h)$coef)^2))
}))
rownames(comp) <- 3:50
colnames(comp) <- c("Triweight", "Biweight")
comp[,1] <= comp[,2]
p <- get_kernel("Henderson", 6)
p$
specific_kernel(6, kernels = c("Henderson","Triweight", "Biweight"))
p2 <- specific_filter(14,kernels = c("Uniform","Triweight"),degree = 2)
p1+p2
kernel <- "Uniform"


filter0$filters.coef - filter2$filters.coef
filter2$filters.coef - filter3$filters.coef

filter$filters.coef - filter2$filters.coef
filter$internal$getFilter()$coefficientsAsPolynomial()
filter2$internal$getFilter()$coefficientsAsPolynomial()

filter2$filters.coef
library(patchwork)
p1+p2
specific_kernel <- function(horizon= 20, kernels = c("Henderson","Uniform", "Triangular",
                                                     "Epanechnikov","Biweight", "Triweight","Tricube",
                                                     "Trapezoidal", "Gaussian")){
  x <- sprintf("t%+i", seq(-horizon,horizon))
  x <- sub("+0", "", x, fixed = TRUE)
  x <- factor(x,levels = x, ordered = TRUE)
  all_kernels <- do.call(rbind,
                         lapply(kernels,
                                function(kernel){
                                  k_coef <- get_kernel(kernel, horizon)$coef
                                  k_coef <- c(rev(k_coef[-1]), k_coef)
                                  if(kernel == "Gaussian")
                                    k_coef <- k_coef/sum(k_coef)
                                  data.frame(x = seq(-horizon,horizon), y = k_coef,
                                             kernel = factor(kernel,
                                                             levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                        "Triweight", "Tricube", "Biweight",
                                                                        "Epanechnikov", "Triangular", "Uniform"),
                                                             ordered = TRUE), 
                                             h = horizon,
                                             stringsAsFactors = FALSE)
                                }))
  title = sprintf("horizon h = %s, bandwith 2h + 1 = %s",horizon, 2*horizon+1)
  ggplot(data = all_kernels, aes(x = x, y = y, color = kernel)) +
    geom_line(size = 0.7) +
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
specific_filter <- function(horizon= 20, kernels = c("Henderson","Uniform", "Triangular",
                                                     "Epanechnikov","Biweight", "Triweight","Tricube",
                                                     "Trapezoidal", "Gaussian"),
                            degree = 3){
  all_kernels <- do.call(rbind,
                         lapply(kernels,
                                function(kernel){
                                  n_kernel <- kernel
                                  if(kernel == "Epanechnikov")
                                    n_kernel <- "Parabolic"
                                  tryCatch({
                                    k_coef <- lp_filter(horizon = horizon, kernel = n_kernel,degree = degree)$filters.coef
                                    k_coef <- k_coef[,ncol(k_coef)]
                                    
                                    data.frame(x = seq(-horizon,horizon), y = k_coef,
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE), 
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                  }, error = function(e){
                                    data.frame(x = seq(-horizon,horizon), y = NA, 
                                               kernel = factor(kernel,
                                                               levels = c("Henderson", "Gaussian", "Trapezoidal",
                                                                          "Triweight", "Tricube", "Biweight",
                                                                          "Epanechnikov", "Triangular", "Uniform"),
                                                               ordered = TRUE), 
                                               h = horizon,
                                               stringsAsFactors = FALSE)
                                  })
                                  
                                }))
  title = sprintf("horizon h = %s, bandwith 2h + 1 = %s",horizon, 2*horizon+1)
  ggplot(data = all_kernels, aes(x = x, y = y, color = kernel)) +
    geom_line(size = 0.7) +
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
plot(get_kernel("Gaussian", 20,sd_gauss = 0.25))
