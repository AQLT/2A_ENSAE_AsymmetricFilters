library(rjdfilters)

?filterproperties

kernels = c("Henderson", "Gaussian", "Trapezoidal",
            "Triweight", "Tricube", "Biweight",
            "Parabolic", "Triangular", "Uniform")
list_endpoints <- c("LC", "QL", "CQ", "DAF")
lp_diagnostics <- do.call(rbind,lapply(list_endpoints, function(endpoints){
  data <- sapply(kernels, function(kernel){
    f <- filterproperties(horizon = 6, kernel = kernel, endpoints = endpoints, ic = 3.5)
    result <- diagnostics_matrix(f$filters.coef[1:7,"q=0"], lb = 6)
    remove_row <- switch (endpoints,
      LC = -1,
      QL = -c(1,2),
      -c(1,2,3)
    )
    result[remove_row]
  })
  colnames(data) <- sub("Parabolic", "Epanechnikov",
                        kernels)
  data<- data.frame(Method = endpoints,
                    Criteria = rownames(data),
                    data,
                    stringsAsFactors = FALSE)
  rownames(data) <- NULL
  data
}))
lp_diagnostics[lp_diagnostics$Criteria == "T_g",-c(1,2)] <- lp_diagnostics[lp_diagnostics$Criteria == "T_g",-c(1,2)]*10^3
lp_diagnostics[,-c(1,2)] <- round(lp_diagnostics[,-c(1,2)],3)
lp_diagnostics$Criteria <- paste("$", lp_diagnostics$Criteria , "$")
lp_diagnostics$Criteria <- gsub("T_g", "T_g \\times 10^{-3}",lp_diagnostics$Criteria, fixed = TRUE)
lp_diagnostics
saveRDS(lp_diagnostics,file = "Rapport de stage/data/lp_diagnostics.RDS")
title = "titre"
library(kableExtra)
groupement <- table(factor(lp_diagnostics[,1],levels = unique(lp_diagnostics[,1]), ordered = TRUE))
lp_diagnostics[,-1] %>% 
  kable(format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
        escape = FALSE,caption = title, format = "latex") %>% 
  kable_styling(latex_options=c("striped","scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Kernel" = ncol(lp_diagnostics)-2)) %>%
  group_rows(index = c(A=5,B=4,c=3,d=3))
 
lp_diagnostics <- lp_diagnostics[,c()]
endpoints = "QL"

lapply()
jobj <- f$internal
asym0 <- jobj$getAfilters()[[1]]
l = max(asym0$getLowerBound(),-2)
u = min(asym0$getLowerBound())
asym0$getUpperBound()

round(f$filters.diagnostics,3)
round(,3)

f$internal
rjdfilters:::proc_data(f$internal,"abias0(0)")
x = f$filters.coef[,"q=0"]
sum(x)
lb = 6

data <- data.frame(rowname = paste("$",rownames(head(mtcars,15)),"$"),
                   head(mtcars,15))
x <- knitr::kable(data, "latex",
                  format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
                  escape = FALSE,
                  caption = "test")%>% 
  kable_styling(latex_options=c("striped","scale_down", "hold_position"))%>%
  add_header_above(c(" " = 1, "Kernel" = ncol(mtcars)-1)) %>% 
  pack_rows(index = groupement)
x

