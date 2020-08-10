library(rjdfilters)
test <- AQLTools::ctrl_v()
test2 <- apply(test,2,function(x){
  tmp <- na.omit(rev(x))
  c(tmp, rep(NA,sum(is.na(x))))
})
filter <- filterproperties(horizon = 6, degree = 3, kernel= "Henderson", endpoints = "LC",ic = 3.5)
round(timeliness(filter,lowerbound = 0,
                 penalty = function(x,y)x^2*sin(y)^2),3)
# filter[,-7][filter[,-7]==0] <- NA # Pas obligatoire d'enlever les 0
musgrave <- apply(filter$filters.coef,2,function(x) fst(rev(na.omit(x)), lb = -6)$criteria)
musgrave <- round(musgrave, 3)
musgrave #timeliness musgrave pas ok
a_coef <- filter$filters.coef[,sprintf("q=%i",0)]
a_coef <- rjdfilters:::trailingZeroAsNa(a_coef)
mse(sweights = filter$filters.coef[,"q=6"], na.omit(a_coef))
round(apply(test2,2,function(x) fst(na.omit(x), lb = -6)$criteria),3) - musgrave
round(apply(test,2,function(x) fst(na.omit(x), lb = -6)$criteria),3)- musgrave


timeliness <- function(x, lowerbound = 2*pi/120,
                                 upperbound = 2*pi/12,
                                  penalty = function(x,y)x^2*sin(y)^2,
                                  ...){
  s_gain <- get_properties_function(x, "Symmetric Gain")
  s_phase <- get_properties_function(x, "Symmetric Phase")
  as_gain <- get_properties_function(x, "Asymmetric Gain")
  as_phase <- get_properties_function(x, "Asymmetric Phase")
  all_gain <- c(as_gain, s_gain)
  all_phase <- c(as_phase, s_phase)
  names(all_gain)[length(names(all_gain))] <-
    names(all_phase)[length(names(all_phase))]  <- "symmetric"
  
  results <- sapply(seq_along(all_gain), function(i){
    f <- function(x, ...){
      "penalty"(all_gain[[i]](x),all_phase[[i]](x))
    }
    integrate(f, lower = lowerbound,
              upper = upperbound)$value
  })
  names(results) <- names(all_gain)
  results
}

################## Henderson ##########################



henderson <- sapply(0:6,function(i){
  fstfilter(lags = 12-i, leads = 0+i, pdegree=2, 
            smoothness.weight=1, timeliness.weight = 0)$criteria
})
colnames(henderson) <- 0:6
henderson <- round(henderson,3)
henderson # tout OK

no_phase_shift <- sapply(0:5,function(i){
  fstfilter(lags = 12-i, leads = 0+i, pdegree=2, 
            smoothness.weight=1/1001, timeliness.weight = 1000/1001)$criteria
})
no_phase_shift_coef <- sapply(0:5,function(i){
  fstfilter(lags = 12-i, leads = 0+i, pdegree=2, 
            smoothness.weight=1/1001, timeliness.weight = 1000/1001)$filter
})
colnames(no_phase_shift) <- 0:5
no_phase_shift <- round(no_phase_shift, 3)
no_phase_shift
#verif : 
no_phase_shift - round(sapply(0:5,function(i){
  fst(no_phase_shift_coef[,i+1], lb = -(12-i))$criteria
}),3)


nps_dom <- apply(nps_dom,2,rev)
nps_dom <- structure(c(0.059577014, 0.011598164, -0.098280553, -0.117664149, 
                       0.013404814, 0.179109203, 0.1858186, -0.034854178, -0.298634749, 
                       -0.280750982, 0.147248496, 0.639930212, 0.593498108, 0.007262546, 
                       -0.022251689, -0.022802953, 0.02967566, 0.070273991, 0.029271142, 
                       -0.07491603, -0.135013114, -0.049417538, 0.170381826, 0.379189606, 
                       0.404183915, 0.21416264, -0.021304492, -0.013872558, 0.039632005, 
                       0.072074779, 0.017842262, -0.092499148, -0.141601095, -0.034437844, 
                       0.194576826, 0.384546488, 0.381148135, 0.195567682, 0.018326962, 
                       -0.019545884, 0.006797998, 0.044124229, 0.02622291, -0.046312938, 
                       -0.093969794, -0.032198966, 0.140998497, 0.323000476, 0.378284865, 
                       0.256793342, 0.05857433, -0.042769066, -0.001808923, 0.006043783, 
                       0.001633688, -0.021845285, -0.032912701, 0.011769599, 0.121689191, 
                       0.250112121, 0.317728285, 0.270409627, 0.130231581, -0.00689036, 
                       -0.046160605, 0.004023176, -0.010582428, -0.028888601, -0.016923829, 
                       0.043965001, 0.139644539, 0.229754647, 0.27028542, 0.238446918, 
                       0.14712069, 0.040975964, -0.026959367, -0.03086213, -0.019349845, 
                       -0.027863777, 0, 0.065491784, 0.147356514, 0.214336747, 0.240057157, 
                       0.214336747, 0.147356514, 0.065491784, 0, -0.027863777, -0.019349845
), .Dim = c(13L, 7L), .Dimnames = list(NULL, c("H12_0", "H11_1", 
                                               "H10_2", "H9_3", "H8_4", "H7_5", "H6_6")))


round(sapply(0:5,function(i){
  x <- nps_dom[,i+1]
  fst(x, lb = -(12-i))$criteria
}),3)
sum(no_phase_shift[,1]*c(0,1,1000))
sum(sapply(0:5,function(i){
  x <- nps_dom[,i+1]
  fst(x, lb = -(12-i))$criteria
})*c(0,1,1000))
no_phase_shift2 <- sapply(0:5,function(i){
  f <- fstfilter(lags = 12-i, leads = i, pdegree=3, 
            smoothness.weight=1/1001, timeliness.weight = 1000/1001)$criteria
  f
})
colnames(no_phase_shift2) <- 0:5
no_phase_shift2 <- round(no_phase_shift2, 3)
no_phase_shift2
test2 <- apply(test,2,function(x){
  tmp <- na.omit(rev(x))
  c(tmp, rep(NA,sum(is.na(x))))
})

pen <- function(x){
  sum(fst(x, lb = -12)$criteria*c(0, 1, 1000))
}
const <- function(x){
  f = rbind(sum(x)-1,
            sum(x*seq(-12,0)),
            sum(x*seq(-12,0)^2))
  return(list(ceq=round(f,6),c=NULL))
}
const(nps_dom[,1] )
const(no_phase_shift_coef[,1])
nps_dom[,1] - no_phase_shift_coef[,1]
x0=nps_dom[,1]
res$par 
sum(fst(nps_dom[,1], lb = -12)$criteria*c(0,1,1000))
sum(fst(no_phase_shift_coef[,1], lb = -12)$criteria*c(0,1,1000))
fstfilter(lags = 12, leads = 12-i, pdegree=3, 
          smoothness.weight=1/1001, timeliness.weight = 1000/1001)

pen(x0)
res <- solnl(x0,objfun=pen,confun=const)
res$par
pen(res$par)
pen(no_phase_shift_coef[,1])
pen(x0)
const(x0)
const(no_phase_shift_coef[,1])
library(NlcOptim)
?nlm

test
nps_dom2 <- structure(list(H12_0 = c(0.05994, 0.01138, -0.09895, -0.11779, 
                                     0.01418, 0.17992, 0.18563, -0.03593, -0.29941, -0.28032, 0.14829, 
                                     0.64018, 0.59288),
                           H11_1 = c(0.00719, -0.02221, -0.02268, 0.02971, 
                                     0.07015, 0.02913, -0.07491, -0.13487, -0.0493, 0.17034, 0.37906, 
                                     0.40415, 0.21423),
                           H10_2 = c(-0.0214, -0.01382, 0.03979, 0.07211, 
                                     0.01768, -0.09267, -0.14158, -0.03426, 0.19471, 0.38449, 0.38101, 
                                     0.19553, 0.0184), 
                           H9_3 = c(-0.01956, 0.00681, 0.04415, 0.02622, 
                                    -0.04634, -0.094, -0.03219, 0.14103, 0.32302, 0.37827, 0.25677, 
                                    0.05857, -0.04276), 
                           H8_4 = c(-0.00181, 0.00604, 0.00163, -0.02185, 
                                    -0.03291, 0.01177, 0.12169, 0.25011, 0.31773, 0.27041, 0.13023, 
                                    -0.00689, -0.04616), 
                           H7_5 = c(0.00402, -0.01058, -0.02889, -0.01692, 
                                    0.04397, 0.13964, 0.22975, 0.27028, 0.23845, 0.14712, 0.04098, 
                                    -0.02696, -0.03086), 
                           H6_6 = c(-0.01935, -0.02786, 0, 0.06549, 
                                    0.14736, 0.21434, 0.24006, 0.21434, 0.14736, 0.06549, 0, -0.02786, 
                                    -0.01935)), class = "data.frame", row.names = c(NA, -13L))

round(nps_dom,5) - nps_dom2
