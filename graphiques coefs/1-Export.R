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

sweights = all_filters[["QL"]][,"q=6"]
aweights = as.numeric(na.omit(rjdfilters:::trailingZeroAsNa(x[,"q=0"])))
aweights = as.numeric(na.omit(rjdfilters:::trailingZeroAsNa(x[,"q=0"])))
if(length(sweights)>length(aweights)){
  # we asume sweights were specify from [-n to n] instead of [0,n]
  n <- (length(sweights)-1)/2
  sweights <- sweights[-seq_len(n)]
}
library(rJava)
SymmetricFilter <- J("jdplus.math.linearfilters.SymmetricFilter")
sf = SymmetricFilter$ofInternal(sweights)
FiniteFilter af = FiniteFilter.of(afilter, -sfilter.length+1);
spectral = match.arg(density)
rslt<-.jcall("demetra/saexperimental/r/FiltersToolkit", "[D", "mseDecomposition",
             sweights, aweights, spectral, passband)
mse_theo <- sapply(all_filters[c("LC", "QL", "CQ", "DAF", "rkhs_timeliness", "fst_lc", "fst_lc_min", "fst_lc_med", "fst_rkhs_timeliness")],
                   function(x){
    mse_ <- rjdfilters::mse(x[,"q=6"], as.numeric(na.omit(rjdfilters:::trailingZeroAsNa(x[,"q=0"]))),
                            passband = pi/6+0.00001)
    tot <- sum(mse_)
    res = c(mse_, tot)
    names(res)[5] <- "MSE"
    res
})
rownames(mse_theo) <- c("Accuracy", "Smoothness", "Timeliness", "Residual", "MSE")
colnames(mse_theo) <- c("LC", "QL", "CQ", "DAF", "$b_{q,\\varphi}$",
                        "Min.", "Max.", 
                        "Méd.", "Min.")
saveRDS(mse_theo, "graphiques coefs/filters_used/mse_theo.RDS")

mse_theo2 <- lapply(0:5, function(q){
  sapply(all_filters[c("LC", "QL", "CQ", "DAF", "rkhs_timeliness", "fst_lc", "fst_lc_min", "fst_lc_med", "fst_rkhs_timeliness")], function(x){
    mse_ <- rjdfilters::diagnostic_matrix(x[,sprintf("q=%i",q)],6,sweights = x[,"q=6"])
    tot <- sum(mse_)
    res = c(mse_, tot)
    names(res)[length(res)] <- "MSE"
    res
  })
})
mse_theo2 <- lapply(mse_theo2, round, 3)

rownames(mse_theo) <- c("Accuracy", "Smoothness", "Timeliness", "Residual", "RMSE")
colnames(mse_theo) <- c("LC", "QL", "CQ", "DAF", "$b_{q,\\varphi}$",
                        "Min.", "Max.", 
                        "Méd.", "Min.")

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
