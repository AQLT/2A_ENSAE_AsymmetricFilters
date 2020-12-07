#####################################################
# Real time detection of trend-cycle turning points #
# Authors: Estela Bee Dagum & Silvia Bianconcini    #
# Date: May 2013                                    #
#####################################################
# Henderson biweight kernel
library(rjdfilters)
library(rJava)
p<-3

dens<-function(x) (1-x^2)^2
dens1<-function(x) dens(x)/integrate(dens,-1,1)$value                  

mom<-array(0,2*p)
for (i in 0:(2*p)){
  moma<-function(x) (x^i)*dens1(x)
  mom[i+1]<-integrate(moma,-1,1)$value
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

#par(mfrow=c(1,2))
#OPTIMAL BANDWIDTH SELECTION

#Gain functions on the whole domain
w<-seq(0,0.5,1e-05)
Asym<-t(array(rep(sym,length(w)),c(h,length(w))))*cos(2*pi*w*t(array(rep(-m:m,length(w)),c(h,length(w)))))
Bsym<-t(array(rep(sym,length(w)),c(h,length(w))))*sin(2*pi*w*t(array(rep(-m:m,length(w)),c(h,length(w)))))
G<-sqrt(apply(Asym,1,sum)^2+apply(Bsym,1,sum)^2)
Hbandgain<-NULL
minegain<-NULL
webiwgain2<-array(0,c(m,(2*m+1)))
for (q in 0:(m-1)){
  biw1<-function(b) asybiw(q,b)
  Gasy<-function(b){
    A<-t(array(rep(biw1(b),length(w)),c((m+q+1),length(w))))*cos(2*pi*w*t(array(rep(-m:q,length(w)),c((m+q+1),length(w)))))
    B<-t(array(rep(biw1(b),length(w)),c((m+q+1),length(w))))*sin(2*pi*w*t(array(rep(-m:q,length(w)),c((m+q+1),length(w)))))
    Ga<-sqrt(apply(A,1,sum)^2+apply(B,1,sum)^2) 
    Ga
  }
  tran<-function(b)  sqrt(2*sum(abs(Gasy(b)-G)^2))
  aa<-optimize(tran,c(m+0.01,h),tol=1e-5)
  band<-aa$minimum
  minegain<-c(minegain,aa$objective)
  webiwgain2[(m-q),(m-q+1):(2*m+1)]<-biw1(band)
  Hbandgain<-c(Hbandgain,band)
}

weia<-array(0,dim(webiwgain))
for (i in 1:nrow(weia)){for (j in 1:ncol(weia)){weia[i,j]<-webiwgain[nrow(weia)+1-i,ncol(weia)+1-j]}}
weigain<-rbind(weia,c(sym),webiwgain)


#Gain function graphics
#w<-seq(0,0.5,1e-05)
#plot(G,type="l",ylim=c(0,1.2))
#for (q in 0:(m-1)){biw1<-function(b) asybiw(q,b)
#                   Gasy<-function(b){A<-t(array(rep(biw1(b),length(w)),c((m+q+1),length(w))))*cos(2*pi*w*t(array(rep(-m:q,length(w)),c((m+q+1),length(w)))))
#                                     B<-t(array(rep(biw1(b),length(w)),c((m+q+1),length(w))))*sin(2*pi*w*t(array(rep(-m:q,length(w)),c((m+q+1),length(w)))))
#                                     Ga<-sqrt(apply(A,1,sum)^2+apply(B,1,sum)^2) 
#                                     Ga}
#                   lines(abs(Gasy(Hbandgain[q+1])),col="green")
#                   lines(abs(Gasy(Hbandphi[q+1])),col="red")
#                   lines(abs(Gasy(Hbandtot[q+1])),col="blue")}

#save smoothing matrix
write.table(weisym,file= paste("weifixedb_",h,"_.txt"),sep="\t",dec=".",row.names=FALSE, col.names=FALSE)
write.table(weigain,file= paste("weiG_",h,"_.txt"),sep="\t",dec=".",row.names=FALSE, col.names=FALSE)

#save selected bandwidths
write.table(round(Hbandgain,digits=3),file= paste("Hbandgain_",h,"_.txt"),sep="\t",dec=".",row.names=FALSE, col.names=FALSE)



jsymf <- .jcall("jdplus/math/linearfilters/SymmetricFilter",
                "Ljdplus/math/linearfilters/SymmetricFilter;",
                "ofInternal", rev(sym[1:7]))
jsymf$gainFunction()
sym_gain <- rjdfilters:::get_gain_function(jsymf)
sym_phase <- rjdfilters:::get_phase_function(jsymf)
sym_frf <- rjdfilters:::get_frequencyResponse_function(jsymf)
gain_asym <- function(b, q){
  jasymf <- .jcall("jdplus/math/linearfilters/FiniteFilter",
                   "Ljdplus/math/linearfilters/FiniteFilter;",
                   "of", asybiw(q,b),as.integer(-6))
  rjdfilters:::get_gain_function(jasymf)
}
# .jcall("jdplus/dfa/MSEDecomposition",
#        "Ljdplus/dfa/MSEDecomposition;",
#        "of", .jnull("java/util/function/DoubleUnaryOperator"),
#        .jcast(jsymf$frequencyResponseFunction(), "java/util/function/DoubleFunction"),
#        .jcast(jasymf$frequencyResponseFunction(), "java/util/function/DoubleFunction"),
#        passband)
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

for (q in 0:(m-1)){
  print(q)
  diff_f <- function(x, b, q){
    2* abs(gain_asym(b, q)(x) - sym_gain(x))^2
     # 2* abs(frf_asym(b, q)(x) - sym_frf(x))^2
  }
  tran <- function(b, q){
    sqrt(integrate(diff_f,0,pi,b=b, q = q)$value)
  }
  plot_const(0)
  plot_const(1)
  plot_const(2)
  plot_const(3)
  plot_const(4)
  plot_const(5)
  aa<-optimize(tran,c(m+0.01,h),tol=1e-5, q = q)
  band<-aa$minimum
  mingain<-c(mingain,aa$objective)
  webiwgain[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
  Hbandgain<-c(Hbandgain,band)
}
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

# for (q in 0:(m-1)){
#   print(q)
#   
#   tran <- function(b, q){
#     mse(sweights = sym,
#         aweights = rev(asybiw(q,b)),
#         density = "uniform",passband = 2*pi/12)["timeliness"]
#   } 
#   aa<-optimize(tran,c(m+0.01,h),tol=1e-5, q = q)
#   band<-aa$minimum
#   minphase <- c(minphase,aa$objective)
#   webiwphase[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
#   Hbandphase<-c(Hbandphase,band)
# }
for (q in 0:(m-1)){
  print(q)
  diff_f <- function(x, b, q){
    # 8*sym_gain(x)*gain_asym(b, q)(x)*sin((sym_phase(x)-phase_asym(b, q)(x))/2)^2
    fr = sym_frf(x); fra = frf_asym(b,q)(x)
    # fr = rjdfilters:::get_frequencyResponse_function(jsymf)(x); fra = rjdfilters:::get_frequencyResponse_function(jasymf)(x)
    g = Mod(fr); ga = Mod(fra)
    p = -Arg(fr); pa = -Arg(fra)
    s = sin((p-pa)/2)
    g*ga*s*s
  }
  tran <- function(b, q){
    8*integrate(diff_f, 0,2*pi/12,b=b, q = q)$value
  } 
  # plot(Vectorize(function(x)tran(x,5)),m+0.01, h)
  aa<-optimize(tran,c(m+0.01,h),tol=1e-5, q = q)
  band<-aa$minimum
  minphase2 <- c(minphase2,aa$objective)
  webiwphase2[(m-q),(m-q+1):(2*m+1)] <- asybiw(q,band)
  Hbandphase2<-c(Hbandphase2,band)
}

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
diagnostics_matrix(rkhs$frf$weight[,1], lb = -6)
saveRDS(rkhs, file = "RKHS/rkhs.RDS")
saveRDS(rkhs, file = "FST/rkhs.RDS")

rkhs <- readRDS(file = "RKHS/rkhs.RDS")
names(rkhs) <- c("$b_{q,\\gamma}$", "$b_{q,G}$",
                 "$b_{q,\\varphi}$")
all_q <- c(0,1,2)

rkhs_diagnostics <- do.call(rbind,lapply(names(rkhs),
                                       function(method){
  f <- lpp_properties(horizon = 6, kernel = "Henderson", ic = 3.5)
  a_coeff <- rkhs[[method]]$weight[,sprintf("q=%i",all_q)]
  data <- apply(a_coeff,2,diagnostics_matrix, lb = 6,sweight = f$filters.coef[,"q=6"])
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
