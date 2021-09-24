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
for(i in names(all_filters)){
    print(i)
    p <- plot_graph(all_filters[[i]])
    ggsave(filename = sprintf("graphiques coefs/img_filters_used/daf/%s.png",tolower(i)), 
           p,
           width = 8, height = 5)
}