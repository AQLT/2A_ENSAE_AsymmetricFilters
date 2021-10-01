#####################################################
# Real time detection of trend-cycle turning points #
# Authors: Estela Bee Dagum & Silvia Bianconcini    #
# Date: May 2013                                    #
#####################################################
# Henderson biweight kernel
library(rjdfilters)
library(rJava)
p<-3 #polynomial degree

dens<-function(x) (1-x^2)^2
dens1<-function(x) dens(x)/integrate(dens,-1,1)$value                  

mom<-array(0,2*p)
for (i in 0:(2*p)){
  moma<-function(x) (x^i)*dens1(x)
  mom[i+1]<-integrate(moma,-1,1,subdivisions = 500L)$value
}

Ho<-array(0,c(p+1,p+1))
for (i in 1:(p+1)){
  for (j in 1:(p+1)) Ho[i,j]<-mom[i+j-1]
}
detH1<-function(x) (mom[5]-mom[3]*x*x)*(mom[3]*mom[7]-mom[5]^2)

K3<-function(x) (detH1(x)/det(Ho))*dens1(x)

asyKer<-function(j,b,m,q){
  num<-K3(j/b)
  den<-sum(K3((-m:q)/b))
  asyKer<-num/den
  asyKer
}

#Selection of the length h of the filter
h<-13
m<-(h-1)/2

asybiw<-function(q,b) asyKer(-m:q,b,m,q)
#Symmetric weights 
sym<-asyKer(-m:m,m+1,m,m)
#smoothing matrix based on a bandwidth=m+1 for all the filters
webiwsym<-array(0,c(m,(2*m+1)))
for (q in 0:(m-1)) webiwsym[(m-q),(m-q+1):(2*m+1)]<-asyKer(-m:q,m+1,m,q)

weia<-array(0,dim(webiwsym))
r<-dim(webiwsym)[1]
c<-dim(webiwsym)[2]
for (i in 1:r){for (j in 1:c){weia[i,j]<-webiwsym[r+1-i,c+1-j]}}

weisym<-rbind(weia,c(sym),webiwsym)

jsymf <- .jcall("jdplus/math/linearfilters/SymmetricFilter",
                "Ljdplus/math/linearfilters/SymmetricFilter;",
                "of", .jcall("demetra/data/DoubleSeq",
                             "Ldemetra/data/DoubleSeq;", "of",sym))
sym_gain <- rjdfilters:::get_gain_function(jsymf)
sym_phase <- rjdfilters:::get_phase_function(jsymf)
sym_frf <- rjdfilters:::get_frequencyResponse_function(jsymf)
gain_asym <- function(b, q){
  jasymf <- .jcall("jdplus/math/linearfilters/FiniteFilter",
                   "Ljdplus/math/linearfilters/FiniteFilter;",
                   "of", asybiw(q,b),as.integer(-6))
  rjdfilters:::get_gain_function(jasymf)
}

frf_asym <- function(b, q){
  jasymf <- .jcall("jdplus/math/linearfilters/FiniteFilter",
                   "Ljdplus/math/linearfilters/FiniteFilter;",
                   "of", asybiw(q,b),as.integer(-6))
  rjdfilters:::get_frequencyResponse_function(jasymf)
}
phase_asym <- function(b, q){
  jasymf <- .jcall("jdplus/math/linearfilters/FiniteFilter",
                   "Ljdplus/math/linearfilters/FiniteFilter;",
                   "of", asybiw(q,b),as.integer(-6))
  rjdfilters:::get_phase_function(jasymf)
}
Hbandgain<- Hbandfrf <- Hbandphase <- Hbandphase2 <- NULL
mingain <- minfrf <- minphase <- minphase2 <-NULL
webiwgain <- webiwfrf <- 
  webiwphase <- webiwphase2 <- array(0,c(m,(2*m+1)))
# Gain
for (q in 0:(m-1)){
  print(q)
  diff_f <- function(x, b, q){
    2* abs(gain_asym(b, q)(x) - sym_gain(x))^2
     # 2* abs(frf_asym(b, q)(x) - sym_frf(x))^2
  }
  tran <- function(b, q){
    sqrt(integrate(diff_f,0,pi,b=b, q = q)$value)
  }
  aa<-optimize(tran,c(m+0.01,h),tol=1e-5, q = q)
  band<-aa$minimum
  mingain<-c(mingain,aa$objective)
  webiwgain[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
  Hbandgain<-c(Hbandgain,band)
}
# frf
for (q in 0:(m-1)){
  print(q)
  diff_f <- function(x, b, q){
    2* abs(frf_asym(b, q)(x) - sym_frf(x))^2
  }
  tran <- function(b, q){
    sqrt(integrate(diff_f,0,pi,b=b, q = q)$value)
  } 
  
  aa<-optimize(tran,c(m+0.01,h),tol=1e-5, q = q)
  band<-aa$minimum
  minfrf <- c(minfrf,aa$objective)
  webiwfrf[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
  Hbandfrf<-c(Hbandfrf,band)
}
plot_const <- function(q){
  plot(Vectorize(function(x)tran(x,q)),m+0.01, h,main = sprintf("q=%i",q))
}

# Timeliness
for (q in 0:(m-1)){
  print(q)
  diff_f <- function(x, b, q){
    # 8*sym_gain(x)*gain_asym(b, q)(x)*sin((sym_phase(x)-phase_asym(b, q)(x))/2)^2
    fr = sym_frf(x); fra = frf_asym(b,q)(x)
    # fr = rjdfilters:::get_frequencyResponse_function(jsymf)(x); fra = rjdfilters:::get_frequencyResponse_function(jasymf)(x)
    g = Mod(fr); ga = Mod(fra)
    p = -Arg(fr); pa = Arg(fra)
    s = sin((pa)/2)
    g*ga*s*s
  }
  tran <- function(b, q){
    8*integrate(diff_f, 0,2*pi/16,b=b, q = q)$value
  } 
  # plot(Vectorize(function(x) tran(x,q)),m-.5, 3*m)
  aa<-optimize(tran,c(m+0.01,3*m),tol=1e-7, q = q)
  band<-aa$minimum
  minphase2 <- c(minphase2,aa$objective)
  webiwphase2[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
  Hbandphase2<-c(Hbandphase2,band)
}
tran(band, q = q)
tran(6+0.1, q = q)

formatage_poids <- function(weight, band){
  res <- apply(rbind(sym, weight),1,function(x){
    x[cumsum(x)==0] <- NA
    c(na.omit(x), rep(0, sum(is.na(x))))
  })
  res <- res[, rev(seq_len(ncol(res)))]
  h_tmp <- (nrow(res) - 1)/2
  rownames(res) <- rjdfilters:::coefficients_names(-h_tmp,h_tmp)
  colnames(res) <- sprintf("q=%i", 0:h_tmp)
  names(band) <-  sprintf("q=%i", 0:(h_tmp-1))
  list(weight = res, band = band)
}
rkhs <- list(frf = formatage_poids(webiwfrf, Hbandfrf),
             gain = formatage_poids(webiwgain, Hbandgain),
             # phase = formatage_poids(webiwphase, Hbandphase),
             phase = formatage_poids(webiwphase2, Hbandphase2))
diagnostic_matrix(rkhs$frf$weight[,1], lb = -6)
saveRDS(rkhs, file = "RKHS/rkhs.RDS")
saveRDS(rkhs, file = "FST/rkhs.RDS")

rkhs <- readRDS(file = "RKHS/rkhs.RDS")
names(rkhs) <- c("$b_{q,\\gamma}$", "$b_{q,G}$",
                 "$b_{q,\\varphi}$")
all_q <- c(0,1,2)

rkhs_diagnostics <- do.call(rbind,lapply(names(rkhs),
                                       function(method){
  f <- lp_filter(horizon = 6, kernel = "Henderson", ic = 3.5)
  a_coeff <- rkhs[[method]]$weight[,sprintf("q=%i",all_q)]
  data <- apply(a_coeff,2,diagnostic_matrix, lb = 6,sweight = f$filters.coef[,"q=6"])
  data <- t(data)
  data<- data.frame(q = rownames(data),
                    Method = factor(method, levels = names(rkhs), ordered = TRUE),
                    data,
                    stringsAsFactors = FALSE)
  rownames(data) <- NULL
  data
}))
rkhs_diagnostics <- rkhs_diagnostics[order(rkhs_diagnostics$q,rkhs_diagnostics$Method),]

rkhs_diagnostics[,"T_g"] <- rkhs_diagnostics[,"T_g"] *10^3
rkhs_diagnostics[,-c(1,2)] <- round(rkhs_diagnostics[,-c(1,2)],3)
colnames(rkhs_diagnostics)[-(1:2)] <-  paste("$", colnames(rkhs_diagnostics)[-(1:2)] , "$")
rkhs_diagnostics[,"q"] <-  paste("$", rkhs_diagnostics[,"q"]  , "$")

colnames(rkhs_diagnostics) <- gsub("T_g", "T_g \\times 10^{-3}",
                                 colnames(rkhs_diagnostics), fixed = TRUE)
saveRDS(rkhs_diagnostics,file = "Rapport de stage/data/rkhs_diagnostics.RDS")

title <- "Quality criteria of asymmetric filters ($q=0,1,2$) computed by the RKHS methodology $h=6$."
groupement <- table(rkhs_diagnostics[,1])

library(kableExtra)
rkhs_diagnostics[,-1] %>% 
  kable(format.args = list(digits = 3), align = "c", booktabs = T, row.names = FALSE,
        escape = FALSE,caption = title) %>% 
  kable_styling(latex_options=c(#"striped", 
    "scale_down", "hold_position")) %>%
  pack_rows(index = groupement, escape = FALSE)

i <- 1
lp_diagnostics[lp_diagnostics$q=="$ q=0 $",-(1:5)] - 
  rbind(rkhs_diagnostics[i,-(1:5)],rkhs_diagnostics[i,-(1:5)],
        rkhs_diagnostics[i,-(1:5)], rkhs_diagnostics[i,-(1:5)])

i <- 2
lp_diagnostics[lp_diagnostics$q=="$ q=0 $",-(1:5)] - 
  rbind(rkhs_diagnostics[i,-(1:5)],rkhs_diagnostics[i,-(1:5)],
        rkhs_diagnostics[i,-(1:5)], rkhs_diagnostics[i,-(1:5)])

i <- 3
lp_diagnostics[lp_diagnostics$q=="$ q=0 $",-(1:5)] - 
  rbind(rkhs_diagnostics[i,-(1:5)],rkhs_diagnostics[i,-(1:5)],
        rkhs_diagnostics[i,-(1:5)], rkhs_diagnostics[i,-(1:5)])
