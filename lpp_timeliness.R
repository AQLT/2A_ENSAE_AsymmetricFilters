library(rjdfilters)
library(ggplot2)
library(patchwork)


alpha_t <- c(seq(0,99,5),
             seq(100,by = 20, length.out = 1000))
result_all_q <- lapply(alpha_t,function(x){
  henderson_f <- lpp_properties(horizon = 6, kernel = "Henderson",
                                  tweight = x,endpoints =  "LC")
  henderson_f$filters.coef
})
tracer_coef <- function(coefs = c("t-6", "t-5", "t-4", "t-3", "t-2", "t-1", "t"),
                        ncol = NULL, nrow = NULL, byrow = NULL, q=0,...){
  result <- sapply(result_all_q, function(x){
   x[,sprintf("q=%i",q)]
  })
  all_p <- lapply(coefs,
                  function(x){
                    ggplot(mapping = aes(x = alpha_t, y = result[x,])) + geom_line() +
                      scale_y_continuous(limits = range(result)) +
                      labs(x= expression(alpha[T]),
                           y=parse(text=sprintf("v[%s]",x)),
                           title = sprintf("Coefficient in %s, q = %i",x, q))+
                      scale_x_continuous(...) +
                      AQLTools:::theme_aqltools()
                  })
  wrap_plots(all_p, ncol = ncol,
             nrow = nrow, byrow = byrow)
}

tracer_coef(ncol = 2, limits = c(0,5000),
            coefs = c("t-3", "t-2", "t-1", "t")
            )

tracer_coef(ncol = 2, limits = c(0,100))
tracer_coef(ncol = 3, limits = c(0,5000),q =1)
for(q in 0:5){
  ggsave(filename = sprintf("Rapport de stage/img/lppgug_lc_q%i.pdf",q),
         tracer_coef(ncol = 3, limits = c(0,5000), q = q),
         width = 8, height = 6)
}


par()
plot(alpha_t, result["t-6",],ylim = range(result), type = "l")
plot(alpha_t, result["t-5",],ylim = range(result), type = "l")
plot(alpha_t, result["t-4",],ylim = range(result), type = "l")
plot(alpha_t, result["t-3",],ylim = range(result), type = "l")
plot(alpha_t, result["t-2",],ylim = range(result), type = "l")
plot(alpha_t, result["t-1",],ylim = range(result), type = "l")
plot(alpha_t, result["t",],ylim = range(result), type = "l",xlim = c(0,100))
plot(alpha_t, result["t",], type = "l",)


sp <- ggplot(mapping = aes(x = alpha_t, y = result["t",])) + geom_line() +
  scale_y_continuous(limits = range(result)) 
sp
sp+ scale_x_continuous(trans='log2')
# Transformation log en utilisant scale_xx()
# valeurs possibles pour trans : 'log2', 'log10','sqrt'
sp + scale_x_continuous(trans='log2')+
  coord_trans(x="log2")
plot(log(alpha_t), result["t",], type = "l")
scale_x_l


plot(log(alpha_t), result["t-6",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t-5",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t-4",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t-3",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t-2",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t-1",],ylim = range(result), type = "l")
plot(log(alpha_t), result["t",],ylim = range(result), type = "l")

alpha_t <- c(seq(0,10,1),
             seq(11,200,5),
             seq(200, 3000, 10),
             10000)
h13 <- lpp_properties(horizon = 6, kernel = "Henderson",
                 tweight = 0,endpoints =  "LC")$filters.coef[,"q=6"]
result_lc <- lapply(alpha_t,function(x){
  henderson_f <- lpp_properties(horizon = 6, kernel = "Henderson",
                                  tweight = x,endpoints =  "LC")
  apply(henderson_f$filters.coef, 2, diagnostics_matrix,
        lb = -6, passband = pi/6, sweight = h13)
})
result_ql <- lapply(alpha_t,function(x){
  henderson_f <- lpp_properties(horizon = 6, kernel = "Henderson",
                                  tweight = x,endpoints =  "QL")
  apply(henderson_f$filters.coef, 2, diagnostics_matrix,
        lb = -6, passband = pi/6, sweight = h13)
})
res_by_q_lc <- lapply(c("q=0", "q=1", "q=2", "q=3", "q=4", "q=5", "q=6"),
       function(col){
         res <- t(sapply(result_lc, function(x){
           x[-c(1:3),col]
         }))
         res <- cbind(matrix(NA, nrow = nrow(res), ncol = 3),
               res)
         colnames(res) <- c("smoothness.weight", "timeliness.weight", "fidelity.weight", 
                            "Fidelity", "Smoothness", "Timeliness", "accuracy", "smoothness", 
                            "timeliness", "residual")
         res
         text_guggemos <- paste0(sprintf('</br> LC with timeliness weight= %.0f', alpha_t),
                                 sprintf('</br> Fidelity: %.3f', res[,"Fidelity"]),
                               sprintf('</br> Smoothness: %.3f', res[,"Smoothness"]),
                               sprintf('</br> Timeliness: %.3f x 10^-3', res[,"Timeliness"]*1000))
         text_wildi <- paste0(sprintf('</br> LC with timeliness weight= %.0f', alpha_t),
                              sprintf('</br> Fidelity: %.3f', res[,"accuracy"]),
                              sprintf('</br> Smoothness: %.3f', res[,"smoothness"]),
                              sprintf('</br> Timeliness: %.3f', res[,"timeliness"]),
                              sprintf('</br> Residuals: %.3f', res[,"residual"]))
         text_both <- sprintf('</br> LC with timeliness weight= %.0f', alpha_t)
         list(results = res, text_guggemos = text_guggemos,
              text_wildi = text_wildi,
              text_both = text_both)
       })
res_by_q_ql <- lapply(c("q=0", "q=1", "q=2", "q=3", "q=4", "q=5", "q=6"),
                      function(col){
                        res <- t(sapply(result_ql, function(x){
                          x[-c(1:3),col]
                        }))
                        res <- cbind(matrix(NA, nrow = nrow(res), ncol = 3),
                                     res)
                        colnames(res) <- c("smoothness.weight", "timeliness.weight", "fidelity.weight", 
                                           "Fidelity", "Smoothness", "Timeliness", "accuracy", "smoothness", 
                                           "timeliness", "residual")
                        res
                        text_guggemos <- paste0(sprintf('</br> QL with timeliness weight= %.0f', alpha_t),
                                                sprintf('</br> Fidelity: %.3f', res[,"Fidelity"]),
                                                sprintf('</br> Smoothness: %.3f', res[,"Smoothness"]),
                                                sprintf('</br> Timeliness: %.3f x 10^-3', res[,"Timeliness"]*1000))
                        text_wildi <- paste0(sprintf('</br> QL with timeliness weight= %.0f', alpha_t),
                                             sprintf('</br> Fidelity: %.3f', res[,"accuracy"]),
                                             sprintf('</br> Smoothness: %.3f', res[,"smoothness"]),
                                             sprintf('</br> Timeliness: %.3f', res[,"timeliness"]),
                                             sprintf('</br> Residuals: %.3f', res[,"residual"]))
                        text_both <- sprintf('</br> QL with timeliness weight= %.0f', alpha_t)
                        list(results = res, text_guggemos = text_guggemos,
                             text_wildi = text_wildi,
                             text_both = text_both)
                      })
names(res_by_q_lc) <- names(res_by_q_ql) <- 
  c("q=0", "q=1", "q=2", "q=3", "q=4", "q=5", "q=6")
saveRDS(res_by_q_lc, file = "FST/timeliness_lpp_lc.RDS")
saveRDS(res_by_q_ql, file = "FST/timeliness_lpp_ql.RDS")

tracer_coef(ncol = 2, limits = c(0,5000),
            coefs = c("t-3", "t-2", "t-1", "t")
)


?lpp_properties
