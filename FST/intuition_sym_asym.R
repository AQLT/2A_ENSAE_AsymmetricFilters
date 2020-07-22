library(rjdfilters)
compare_filter <- function(x,
                           hend_horizon = 3, ic = 3.5,
                           fst_lag = 6, fst_leads = 3, 
                           pdegree = 2, xlim = c(0,pi)){
  small_h <- filterproperties(horizon = hend_horizon,
                              kernel = "Henderson", endpoints = "LC",
                              ic = ic)
  f <- tryCatch({
    fstfilter(lags = fst_lag, leads = fst_leads, smoothness.weight = 1/(x+2), timeliness.weight = x/(x+2),
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
  data_matrix <- cbind(diagnostics_matrix(small_h$filters.coef[,sprintf("q=%i",hend_horizon)],
                                          lb = hend_horizon),
                       diagnostics_matrix(f$filter, lb = fst_lag))
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

pdf(file = "FST/H3x3vsFST6x3.pdf", width=8,height = 5)
for(x in c(50,seq(100,1000, by = 100),seq(1000,100000, by = 1000))){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2,
                 hend_horizon = 3,
                 fst_lag = 6, fst_leads = 3)
}
dev.off()

pdf(file = "test.pdf", width=8,height = 5)
for(x in c(50,seq(100,1000, by = 100),seq(1000,100000, by = 1000))){
  compare_filter(1-1/x, xlim = c(0,pi/6), pdegree = 2)
}
dev.off()
?pdf

compare_filter(1/10,xlim = c(0,pi/6),pdegree = 2)
compare_filter(0,xlim = c(0,pi/6),pdegree = 2)
compare_filter(1-1/1000,xlim = c(0,pi/6),pdegree = 2)


#########
small_h <- filterproperties(horizon = 3, kernel = "Henderson", endpoints = "LC", ic = 3.5)


pdf(file = "FST/H3x3vsFST6x3.pdf", width=8,height = 5)
for(x in c(50,seq(100,1000, by = 100),seq(1000,100000, by = 1000))){
  compare_filter(x, xlim = c(0,pi/6), pdegree = 2)
}
dev.off()