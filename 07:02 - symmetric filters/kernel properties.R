library(rjdfilters)
library(ggplot2)
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
                                  data.frame(x = seq(-horizon,horizon), y = k_coef, kernel = kernel, h = horizon,
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
                                    k_coef <- filterproperties(horizon = horizon, kernel = n_kernel)$filters.coef
                                    k_coef <- k_coef[,ncol(k_coef)]
                                    
                                    data.frame(x = seq(-horizon,horizon), y = k_coef, kernel = kernel, h = horizon,
                                               stringsAsFactors = FALSE)
                                  }, error = function(e){
                                    data.frame(x = seq(-horizon,horizon), y = NA, kernel = kernel, h = horizon,
                                               stringsAsFactors = FALSE)
                                  })
                                  
                                })
  )
}
get_all_gain_sfilters <- function(horizon, xlimp = 2*pi/36){
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
                                  get_f
                                  t <- f$internal$getAfilters()
                                  t$length
                                  t$
                                  t
                                  t <- .jevalArray(t)[[1]]
                                  t$
                                  t$weightsToArray()
                                  t$weights()
                                  t$coefficientsAsPolynomial()
                                  t$length()
                                  t$gainFunction()
                                  f$internal$getAfilters()
                                  t[5]
                                  t[3][1]$getClass()
                                  t2 = .jcast(t[0],"jdplus/math/linearfilters/IFilter")
                                  t2[0]$getClass()
                                  filter$getClass()
                                  f <- filterproperties(horizon = horizon, kernel = n_kernel)
                                  filter <- f$internal$getFilter()
                                  polynom <- filter$coefficientsAsPolynomial()
                                  polynom$getEpsilon()
                                  polynom$toArray()
                                  polynom$isZero()
                                  polynom$evaluateAt(0)
                                  polynom$evaluateAtFrequency(1)
                                  polynom$getClass()
                                  ?polym()
                                  polynom$derivate()
                                  polynom$integrate()$integrate()$derivate()
                                  frf <- .jcall(t2,
                                         "Ljava/util/function/DoubleFunction;",
                                         "frequencyResponseFunction")
                                  filter$frequencyResponseFunction()
                                  tmp <- frf$apply(1)
                                  tmp$getRe()+tmp$getIm()*i
                                  filter$coefficientsAsPolynomial()
                                  filter$realFrequencyResponse()
                                  t2 = filter$factorize()
                                  t2$
                                  t2$scaling
                                  t = f$internal$getFilter()$phaseFunction()$applyAsDouble
                                  t(0)
                                  t$compose()
                                  i = 0
                                  gain <- function(x){
                                    t$applyAsDouble(x)
                                  }
                                  gain(2*pi/36)
                                  g <- t$applyAsDouble
                                  plot(Vectorize(t$applyAsDouble),xlim=c(0,2*pi/36))
                                  
                                  k_gain<- filterproperties(horizon = horizon, kernel = n_kernel)$filters.gain
                                  k_gain <- k_gain[,ncol(k_gain)]
                                  x_values <- seq(0, pi, length.out = length(k_gain))
                                  x_values_keep <- x_values[x_values<xlimp]
                                  data.frame(x = seq(-horizon,horizon), y = k_coef, kernel = kernel, h = horizon,
                                             stringsAsFactors = FALSE)
                                  
                                })
  )
}
plot_filter <- function(data, horizon){
  title = sprintf("horizon h = %s, bandwith 2h + 1 = %s",horizon, 2*horizon+1)
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
for(h in 2:80){
  p <- plot_filter(get_all_kernels(h), h)
  ggsave(filename = sprintf("Rapport de stage/img/kernels/%s.pdf",h), p,
         width = 8, height = 8)
}

for(h in 2:80){
  p <- plot_filter(get_all_sfilters(h), h)
  ggsave(filename = sprintf("Rapport de stage/img/symmetricFilters/%s.pdf",h), p,
         width = 8, height = 8)
}

#TODO
for(h in 2:80){
  p <- plot_filter(get_all_sfilters(h), h)
  ggsave(filename = sprintf("Rapport de stage/img/symmetricFilters/gain%s.pdf",h), p,
         width = 8, height = 8)
}
plot_filter(get_all_filters(2),2)
plot_all_kernels(5)



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
