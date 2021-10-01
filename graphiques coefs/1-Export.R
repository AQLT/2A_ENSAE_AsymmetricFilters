source(file = "graphiques coefs/0-fonctions.R",encoding = "UTF-8")
fst_retained_filters = c("lc", "lc_min", "lc_med", "rkhs_timeliness"
                         #,"rkhs_timeliness_min", "rkhs_timeliness_med"
)
fst = readRDS("comparison_filters/filters/fst.RDS")[["h=6"]][fst_retained_filters]
rkhs = readRDS("comparison_filters/filters/rkhs_rw_p3.RDS")[["h=6"]]["phase"]
lp <- lapply(c("LC", "QL", "CQ", "DAF"), function(endpoints){
    lp_filter(
        horizon = 6,
        degree = 3,
        kernel = "Henderson",
        endpoints = endpoints,
        ic = 3.5,
        tweight = 0,
        passband = pi/12
    )$filters.coef
})
all_filters <- c(fst,
                 rkhs,
                 lp)
names(all_filters) <- c("fst_lc", "fst_lc_min", "fst_lc_med",
                        "fst_rkhs_timeliness",
                        "rkhs_timeliness",
                        "LC", "QL", "CQ", "DAF")
if(!dir.exists("graphiques coefs/filters_used")){
  dir.create("graphiques coefs/filters_used")
}
for(i in names(all_filters)){
    print(i)
    p <- plot_graph(all_filters[[i]])
    ggsave(filename = sprintf("graphiques coefs/filters_used/%s.pdf",tolower(i)), 
           p,
           width = 8, height = 5)
}
for(i in names(all_filters)){
    print(i)
    p <- plot_graph(all_filters[[i]])
    ggsave(filename = sprintf("graphiques coefs/filters_used/%s.svg",tolower(i)), 
           p,
           width = 8, height = 5)
}

for(i in names(all_filters)){
  print(i)
  p <- plot_graph(all_filters[[i]])
  ggsave(filename = sprintf("graphiques coefs/filters_used/%s.jpg",tolower(i)), 
         p,
         width = 8, height = 5)
}


mse_theo <- sapply(all_filters[c("LC", "QL", "CQ", "DAF", "rkhs_timeliness", "fst_lc", "fst_lc_min", "fst_lc_med", "fst_rkhs_timeliness")], function(x){
    mse_ <- rjdfilters::mse(x[,"q=6"], x[,"q=0"])
    tot <- sum(mse_)
    res = c(mse_, tot)
    names(res)[5] <- "MSE"
    res
})
rownames(mse_theo) <- c("Accuracy", "Smoothness", "Timeliness", "Residual", "RMSE")
colnames(mse_theo) <- c("LC", "QL", "CQ", "DAF", "$b_{q,\\varphi}$",
                        "Min.", "Max.", 
                        "Méd.", "Min.")
saveRDS(mse_theo, "graphiques coefs/filters_used/mse_theo.RDS")
format.args = list(decimal.mark = ",",
                   nsmall = 1)
library(kableExtra)
kbl(mse_theo,
    caption = "toto",
    booktabs = TRUE,
    format.args = format.args,
    escape = FALSE,
    align = "c", digits = 1)  %>% 
    kable_styling(latex_options = c("striped", "hold_position")) %>% 
    add_header_above(c(" "=6, "FST - LC" = 3, "FTS - RKHS" = 1))
c("LC min. timeliness" = 3,
  "LC max. timeliness" = 3,
  "LC timeliness médiane" = 3,
  "RKHS min. timeliness" = 3)

dput(names(all_filters))
