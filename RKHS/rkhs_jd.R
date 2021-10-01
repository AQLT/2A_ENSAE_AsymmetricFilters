library(rjdfilters)
?rkhs_filter()

h <- 6
replace_q <- 5
phase <- rkhs_filter(horizon = h, degree = 3,
            kernel = "Biweight",
            asymmetricCriterion = "Timeliness")$filters.coef
phase
bw <- optimize(rkhs_optimization_fun(degree = 3, horizon = 6,
                               leads = replace_q),
         interval = c(h,3*h))
q5 <- rkhs_filter(horizon = h, degree = 3,
                  kernel = "Biweight",
                  asymmetricCriterion = "Timeliness",
                  optimalbw = F,
                  bandwidth = bw$minimum)$filters.coef
phase[,sprintf("q=%i", replace_q)] <- q5[,sprintf("q=%i", replace_q)]

frf <- rkhs_filter(horizon = h, degree = 3,
                   kernel = "Biweight",
                   asymmetricCriterion = "FrequencyResponse")$filters.coef

accuracy <- rkhs_filter(horizon = h, degree = 3,
                   kernel = "Biweight",
                   asymmetricCriterion = "Accuracy")$filters.coef
smoothness <- rkhs_filter(horizon = h, degree = 3,
                   kernel = "Biweight",
                   asymmetricCriterion = "Smoothness")$filters.coef

saveRDS(list(phase = phase, frf = frf,
             smoothness = smoothness,
             accuracy = accuracy),
        sprintf("RKHS/rkhs_h%i_p3.RDS", h))

h <- 11
replace_qs <- c(9,10)
phase <- rkhs_filter(horizon = h, degree = 3,
                     kernel = "Biweight",
                     asymmetricCriterion = "Timeliness")$filters.coef
phase
for(replace_q in replace_qs){
    bw <- optimize(rkhs_optimization_fun(degree = 3, horizon = 6,
                                         leads = replace_q),
                   interval = c(h,3*h))
    q <- rkhs_filter(horizon = h, degree = 3,
                      kernel = "Biweight",
                      asymmetricCriterion = "Timeliness",
                      optimalbw = F,
                      bandwidth = bw$minimum)$filters.coef
    phase[,sprintf("q=%i", replace_q)] <- q[,sprintf("q=%i", replace_q)]
}


frf <- rkhs_filter(horizon = h, degree = 3,
                   kernel = "Biweight",
                   asymmetricCriterion = "FrequencyResponse")$filters.coef

accuracy <- rkhs_filter(horizon = h, degree = 3,
                        kernel = "Biweight",
                        asymmetricCriterion = "Accuracy")$filters.coef
smoothness <- rkhs_filter(horizon = h, degree = 3,
                          kernel = "Biweight",
                          asymmetricCriterion = "Smoothness")$filters.coef

saveRDS(list(phase = phase, frf = frf,
             smoothness = smoothness,
             accuracy = accuracy),
        sprintf("RKHS/rkhs_h%i_p3.RDS", h))



h <- 4
replace_q <- 3
phase <- rkhs_filter(horizon = h, degree = 3,
                     kernel = "Biweight",
                     asymmetricCriterion = "Timeliness")$filters.coef
phase
bw <- optimize(rkhs_optimization_fun(degree = 3, horizon = 6,
                                     leads = replace_q),
               interval = c(h,3*h))
q5 <- rkhs_filter(horizon = h, degree = 3,
                  kernel = "Biweight",
                  asymmetricCriterion = "Timeliness",
                  optimalbw = F,
                  bandwidth = bw$minimum)$filters.coef
phase[,sprintf("q=%i", replace_q)] <- q5[,sprintf("q=%i", replace_q)]

frf <- rkhs_filter(horizon = h, degree = 3,
                   kernel = "Biweight",
                   asymmetricCriterion = "FrequencyResponse")$filters.coef

accuracy <- rkhs_filter(horizon = h, degree = 3,
                        kernel = "Biweight",
                        asymmetricCriterion = "Accuracy")$filters.coef
smoothness <- rkhs_filter(horizon = h, degree = 3,
                          kernel = "Biweight",
                          asymmetricCriterion = "Smoothness")$filters.coef

saveRDS(list(phase = phase, frf = frf,
             smoothness = smoothness,
             accuracy = accuracy),
        sprintf("RKHS/rkhs_h%i_p3.RDS", h))


all_files <- lapply(c(4,6, 11), function(h){
    readRDS(sprintf("RKHS/rkhs_h%i_p3.RDS", h))
})
names(all_files) <- sprintf("h=%i", c(4,6, 11))
saveRDS(all_files, sprintf("comparison_filters/filters/rkhs_rw_p3.RDS"))


