library(rjdfilters)
compare_filter <- function(x,
                           hend_horizon = 3, ic = 3.5,
                           fst_lag = 6, fst_leads = 3, 
                           pdegree = 2, xlim = c(0,pi)){
  small_h <- lp_filter(horizon = hend_horizon,
                              kernel = "Henderson", endpoints = "LC",
                              ic = ic)
  f <- tryCatch({
    fst_filter(lags = fst_lag, leads = fst_leads, smoothness.weight = 1/(x+2), timeliness.weight = x/(x+2),
              pdegree = pdegree)
  },
  error = function(e) {
    print("error")
    NULL
  })
  
  if(is.null(f))
    return(NULL)
  # title <- sprintf("timeliness.weight = %.3f, smoothness.weight = fidelity.weight= %.3f",
  #                  x, (1-x)/2)
  title <- sprintf("timeliness.weight = %i, smoothness.weight = fidelity.weight= 1",
                   x)
  data_matrix <- cbind(diagnostic_matrix(small_h$filters.coef[,sprintf("q=%i",hend_horizon)],
                                          lb = hend_horizon),
                       diagnostic_matrix(f$filters.coef, lb = fst_lag))
  data_matrix[nrow(data_matrix),] <- data_matrix[nrow(data_matrix),]*100
  data_matrix <- round(data_matrix,3)
  colnames(data_matrix) <- c(sprintf("H%ix%i",hend_horizon, hend_horizon),
                             sprintf("FST%ix%i",fst_lag, fst_leads))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
  par(mai = c(0.3, 0.3, 0.2, 0))
  
  plot_coef(f,
            ylim = range(c(-0.15,0.45)),
            main = title, zeroAsNa = TRUE)
  plot_coef(small_h, q = hend_horizon, add = TRUE, col = "red")
  
  plotrix::addtable2plot(6, 0.5,
                         formatC(data_matrix,
                                 format = "f", digits = 3),
                         bty = "o", display.rownames = TRUE, hlines = TRUE,
                         vlines = TRUE, xjust = 0.8, yjust = -0.1)
  par(mai = c(0.3, 0.5, 0.2, 0))
  
  plot_gain(f,
            ylim = range(small_h$filters.gain[,sprintf("q=%i",hend_horizon)]),
            xlim = xlim,
            main = "gain")
  plot_gain(small_h, q = hend_horizon, add = TRUE, col = "red")
  abline(v = pi/6, lty = "dotted")
  
  par(mai = c(0.3, 0.5, 0.2, 0))
  
  plot_phase(f,
             xlim = xlim,
             main = "phase")
  plot_phase(small_h, q = hend_horizon, add = TRUE, col = "red")
  abline(v = pi/6, lty = "dotted")
}

x_values <- c(seq(0,1000, by = 100),5000, 10000)
pdf(file = "FST/H3x3vsFST6x3.pdf", width=8,height = 5)
for(x in x_values){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2,
                 hend_horizon = 3,
                 fst_lag = 6, fst_leads = 3)
}
dev.off()

pdf(file = "FST/H6x6vsFST9x3.pdf", width=8,height = 5)
for(x in x_values){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2,
                 hend_horizon = 6,
                 fst_lag = 9, fst_leads = 3)
}
dev.off()

pdf(file = "FST/H6x6vsFST9x6.pdf", width=8,height = 5)
for(x in x_values){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2,
                 hend_horizon = 6,
                 fst_lag = 9, fst_leads = 6)
}
dev.off()

pdf(file = "FST/H6x6vsFST10x6.pdf", width=8,height = 5)
for(x in x_values){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2,
                 hend_horizon = 6,
                 fst_lag = 10, fst_leads = 6)
}
dev.off()
