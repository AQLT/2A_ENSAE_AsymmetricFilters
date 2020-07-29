library(rjdfilters)
ipi_manuf <- AQLTools::lectureBDM("010537903","010537904")
colnames(ipi_manuf) <- c("brut","cvs")
plot(ipi_manuf, plot.type = "single",
     col = c("black", "red"))

AQLTools::hc_stocks(data_plot_hend)

graphiques <- function(file, start = NULL, end = NULL,
                       start_ipi = NULL, end_ipi = NULL){
  ipi_manuf <- window(ipi_manuf, start = start_ipi,
                      end = end_ipi)
  uniform_cq <- localpolynomials(ipi_manuf[,"cvs"],
                                 6, kernel = "Uniform",
                                 ic= 3.5,endpoints = "CQ")
  triweight_cq <- localpolynomials(ipi_manuf[,"cvs"],
                                   6, kernel = "Triweight",
                                   ic= 3.5, endpoints = "CQ")
  data_plot_ut_cq <- ts.union(ipi_manuf,uniform_cq, triweight_cq)
  
  uniform_lc <- localpolynomials(ipi_manuf[,"cvs"],
                                 6, kernel = "Uniform",
                                 ic= 3.5,endpoints = "LC")
  triweight_lc <- localpolynomials(ipi_manuf[,"cvs"],
                                   6, kernel = "Triweight",
                                   ic= 3.5, endpoints = "LC")
  data_plot_ut_lc <- ts.union(ipi_manuf,uniform_lc, triweight_lc)
  
  uniform_ql <- localpolynomials(ipi_manuf[,"cvs"],
                                 6, kernel = "Uniform",
                                 ic= 3.5,endpoints = "QL")
  triweight_ql <- localpolynomials(ipi_manuf[,"cvs"],
                                   6, kernel = "Triweight",
                                   ic= 3.5, endpoints = "QL")
  data_plot_ut_ql <- ts.union(ipi_manuf,uniform_ql, triweight_ql)
  
  uniform_daf <- localpolynomials(ipi_manuf[,"cvs"],
                                  6, kernel = "Uniform",
                                  ic= 3.5,endpoints = "DAF")
  triweight_daf <- localpolynomials(ipi_manuf[,"cvs"],
                                    6, kernel = "Triweight",
                                    ic= 3.5, endpoints = "DAF")
  data_plot_ut_daf <- ts.union(ipi_manuf,uniform_daf, triweight_daf)
  
  lc <- localpolynomials(ipi_manuf[,"cvs"],
                         6, kernel = "Henderson",
                         ic= 3.5, endpoints = "LC")
  ql <- localpolynomials(ipi_manuf[,"cvs"],
                         6, kernel = "Henderson",
                         ic= 3.5, endpoints = "LC")
  cq <- localpolynomials(ipi_manuf[,"cvs"],
                         6, kernel = "Henderson",
                         ic= 3.5, endpoints = "CQ")
  daf <- localpolynomials(ipi_manuf[,"cvs"],
                          6, kernel = "Henderson",
                          ic= 3.5, endpoints = "DAF")
  data_plot_hend <- ts.union(ipi_manuf,lc, ql, cq, daf)
  
  data_plot_ut_cq <- window(data_plot_ut_cq, start = start, end = end)
  data_plot_ut_lc <- window(data_plot_ut_lc, start = start, end = end)
  data_plot_ut_ql <- window(data_plot_ut_ql, start = start, end = end)
  data_plot_ut_daf <- window(data_plot_ut_daf, start = start, end = end)
  data_plot_hend <- window(data_plot_hend, start = start, end = end)
  
  pdf(file = file, width = 8, height = 5)
  print(AQLTools::graph_ts(data_plot_ut_lc, "LC", n_xlabel = 6, x_lab_month = TRUE))
  print(AQLTools::graph_ts(data_plot_ut_ql, "QL", n_xlabel = 6, x_lab_month = TRUE))
  print(AQLTools::graph_ts(data_plot_ut_cq, "CQ", n_xlabel = 6, x_lab_month = TRUE))
  print(AQLTools::graph_ts(data_plot_ut_daf, "DAF", n_xlabel = 6, x_lab_month = TRUE))
  print(AQLTools::graph_ts(data_plot_hend,"Henderson filter 13 terms", n_xlabel = 6, x_lab_month = TRUE))
  dev.off()
}


graphiques(file = "FST/ipi_manuf_fr.pdf", start = 2018)
graphiques(file = "Examples/ipi_manuf_fr_2008.pdf", start = 2007, end_ipi = c(2009,6))






