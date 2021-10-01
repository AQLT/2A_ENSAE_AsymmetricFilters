library(rjdfilters)

kernels = c("Henderson", "Gaussian", "Trapezoidal",
            "Triweight", "Tricube", "Biweight",
            "Parabolic", "Triangular", "Uniform")
list_endpoints <- c("LC", "QL", "CQ", "DAF")
lp_diagnostics <- do.call(rbind,lapply(list_endpoints, function(endpoints){
  data <- sapply(kernels, function(kernel){
    f <- lp_filter(horizon = 6, kernel = kernel, endpoints = endpoints, ic = 3.5)
    result <- diagnostic_matrix(f$filters.coef[1:7,"q=0"], lb = 6)
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




kernel = c("Henderson")
list_endpoints <- c("LC", "QL", "CQ", "DAF")
all_q <- c(0,1,2)
lp_diagnostics <- do.call(rbind,lapply(list_endpoints, function(endpoints){
  f <- lp_filter(horizon = 6, kernel = kernel, endpoints = endpoints, ic = 3.5)
  a_coeff <- f$filters.coef[,sprintf("q=%i",all_q)]
  data <- apply(a_coeff,2,diagnostic_matrix, lb = 6,sweight = f$filters.coef[,"q=6"])
  data <- t(data)
  data<- data.frame(q = rownames(data),
                    Method = factor(endpoints, levels = list_endpoints, ordered = TRUE),
                    data,
                    stringsAsFactors = FALSE)
  rownames(data) <- NULL
  data
}))
lp_diagnostics <- lp_diagnostics[order(lp_diagnostics$q,lp_diagnostics$Method),]

lp_diagnostics[,"T_g"] <- lp_diagnostics[,"T_g"] *10^3
lp_diagnostics[,-c(1,2)] <- round(lp_diagnostics[,-c(1,2)],3)
colnames(lp_diagnostics)[-(1:2)] <-  paste("$", colnames(lp_diagnostics)[-(1:2)] , "$")
lp_diagnostics[,"q"] <-  paste("$", lp_diagnostics[,"q"]  , "$")

colnames(lp_diagnostics) <- gsub("T_g", "T_g \\times 10^{-3}",
                                 colnames(lp_diagnostics), fixed = TRUE)
lp_diagnostics
lp_diagnostics$MSE <- lp_diagnostics$`$ A_w $` +  lp_diagnostics$`$ S_w $` +  
  lp_diagnostics$`$ T_w $` + lp_diagnostics$`$ R_w $`

saveRDS(lp_diagnostics,file = "Rapport de stage/data/lp_diagnostics_henderson.RDS")

lp_diagnostics <- readRDS("data/lp_diagnostics.RDS")
title <- "Quality criteria of real-time filters ($q=0$) computed by local polynomial."
groupement <- table(lp_diagnostics[,1])
lp_diagnostics[,-1] %>% 
  kable(format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
        escape = FALSE,caption = title) %>% 
  kable_styling(latex_options=c(#"striped", 
    "scale_down", "hold_position")) %>%
  pack_rows(index = groupement)

title <- "Quality criteria of asymmetric filters ($q=0,1,2$) computed by local polynomial with Henderson kernel for $h=6$ and $R=3.5$."
groupement <- table(lp_diagnostics[,1])
lp_diagnostics[,-1] %>% 
  kable(format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
        escape = FALSE,caption = title) %>% 
  kable_styling(latex_options=c(#"striped", 
    "scale_down", "hold_position")) %>%
  add_header_above(c(" " = 1, "Kernel" = ncol(lp_diagnostics)-2)) %>%
  pack_rows(index = groupement, escape = FALSE)


tableau <- function(R = 3.5, horizon = 6,
                    all_q = c(0,1,2,3,4,5)){
  kernel = c("Henderson")
  list_endpoints <- c("LC", "QL", "CQ", "DAF")
  lp_diagnostics <- do.call(rbind,lapply(list_endpoints, function(endpoints){
    f <- lp_filter(horizon = horizon, kernel = kernel, endpoints = endpoints, ic = R)
    a_coeff <- f$filters.coef[,sprintf("q=%i",all_q)]
    data <- apply(a_coeff,2,diagnostic_matrix, lb = horizon,sweight = f$filters.coef[,sprintf("q=%i",horizon)])
    data <- t(data)
    data<- data.frame(q = rownames(data),
                      Method = factor(endpoints, levels = list_endpoints, ordered = TRUE),
                      data,
                      stringsAsFactors = FALSE)
    rownames(data) <- NULL
    data
  }))
  lp_diagnostics <- lp_diagnostics[order(lp_diagnostics$q,lp_diagnostics$Method),]
  
  lp_diagnostics[,"T_g"] <- lp_diagnostics[,"T_g"] *10^3
  lp_diagnostics[,-c(1,2)] <- round(lp_diagnostics[,-c(1,2)],3)
  
  lp_diagnostics
  colnames(lp_diagnostics)[-(1:2)] <-  paste("$", colnames(lp_diagnostics)[-(1:2)] , "$")
  lp_diagnostics[,"q"] <-  paste("$", lp_diagnostics[,"q"]  , "$")
  
  colnames(lp_diagnostics) <- gsub("T_g", "T_g \\times 10^{-3}",
                                   colnames(lp_diagnostics), fixed = TRUE)
  
  title <- sprintf("Quality criteria of asymmetric filters ($q=0,1,2,3$) computed by local polynomial with Henderson kernel for $h=6$ and $IC=%.1f$.",
                   R)
  groupement <- table(lp_diagnostics[,1])
  lp_diagnostics[,-1] %>% 
    kable(format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
          escape = FALSE,caption = title) %>% 
    kable_styling(latex_options=c(#"striped", 
      "scale_down", "hold_position")) %>%
    pack_rows(index = groupement, escape = FALSE)
}

tableau(horizon = 10,all_q = c(3,4,5,6))
