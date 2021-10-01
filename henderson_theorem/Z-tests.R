library(rjdfilters)
h =  6
validitySign <- function(coef, p = 2){
    updn <- c(0, diff(sign(coef)))
    ix <- which(updn != 0)
    length(ix) <= p
}
# Test symétrique 
p=2
fst <- fst_filter(h,6,smoothness.weight = 0.5,timeliness.weight = .4,pdegree = p)
coef <- fst$filters.coef
validitySign(coef)
X_gen <- function(p = 1, h = 6){
    sapply(0:p, function(exp) seq(-h, h)^exp)
}
P_u <- function(u, h = 6){
    3*(h+2)^2-16-11*u^2 +1
}
ker = coef/ sapply(seq(-h, h), P_u)
X = X_gen(p=p)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
isTRUE(all.equal(w, coef))

# Test henderson 
p=2
h_ <- lp_filter(6, degree = p)
coef <- h_$filters.coef[,"q=6"]
X_gen <- function(p = 1, h = 6){
    sapply(0:p, function(exp) seq(-h, h)^exp)
}
P_u <- function(u, h = 6){
    3*(h+2)^2-16-11*u^2 +1
}
paste(round(sapply(seq(-h, 0), P_u),4),collapse = "&")

ker = coef/ sapply(seq(-h, h), P_u)
X = X_gen(p=p)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
isTRUE(all.equal(w, coef))


# Test asymétrique p = 1
p= 3
q = 4
fst <- fst_filter(h,q,smoothness.weight = 0.1,timeliness.weight = .8,pdegree = p)
plot_coef(fst)
coef <- fst$filters.coef
coef

P_u <- function(u, h = 6){
    u+4+1/h
}
ker = coef#/ sapply(seq(-h, q), P_u)
X = head(X_gen(p=p), h+q+1)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
isTRUE(all.equal(w, coef))


# Test asymétrique musgrave
p_gen= 2
p_pol = 3
q = 4
h_ <- lp_filter(6, degree = p_gen,endpoints = "LC")
coef <- h_$filters.coef[,sprintf("q=%i",q)][1:(2*h+1-h+q)]
plot_coef(h_,q=q+1,zeroAsNa = TRUE,legend = TRUE)

P_u <- function(u, h = 6){
    3*(h+2)^2-16-11*u^2 +1
}
ker = coef#/ sapply(seq(-h, q), P_u)
X = head(X_gen(p=p_pol), h+q+1)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p_pol+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
isTRUE(all.equal(w, coef))


# Test asymétrique QL
p= 2
q = 0
h_ <- lp_filter(6, degree = p,endpoints = "LC")
plot_coef(h_,q=q+1,zeroAsNa = TRUE,legend = TRUE)
coef <- h_$filters.coef[,sprintf("q=%i",q)][1:(2*h+1-h+q)]
coef
p=1
P_u <- function(u, h = 6){
    3*(h+2)^2-16-11*u^2 +1
}
ker = coef#/ sapply(seq(-h, q), P_u)
X = head(X_gen(p=p), h+q+1)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
cbind(w, coef)
isTRUE(all.equal(w, coef))


# Test asymétrique rkhs
p= 2
q = 4
rkhs <- rkhs_filter(horizon = 6,
                    degree = p)
plot_coef(rkhs,q=q+1,zeroAsNa = TRUE,legend = TRUE)
coef <- rkhs$filters.coef[,sprintf("q=%i",q)][1:(2*h+1-h+q)]
coef
diagnostic_matrix(coef,lags = h)

P_u <- function(u, h = 6){
    3*(h+2)^2-16-11*u^2 
}
P_u <- function(u, h = 6){
    u+6-1/h
}
ker = coef/ sapply(seq(-h, q), P_u)
X = head(X_gen(p=p), h+q+1)
K = diag(ker)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
cbind(w,coef)
cbind(w,w2,coef)
isTRUE(all.equal(w, coef))

# Test aléatoire
p=2
sigma = matrix(runif((h*2+1)^2), nrow = h*2+1)
e1 <- matrix(0, ncol = 1, nrow = p+1)
e1[1,1] = 1
X = X_gen(p=p)
coef = c(sigma %*% X %*% solve(t(X) %*% sigma %*%X, e1))
ker = coef#/ sapply(seq(-h, h), P_u)
K = diag(ker)
w = c(K %*% X %*% solve(t(X) %*% K %*%X, e1))
names(w) <- names(coef)
isTRUE(all.equal(w, coef))
validitySign(coef)

# test fst plus grande echelle
h =  6
data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = 101)),
                    timeliness.weight = c(seq(0,1,length.out = 101))
)
data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
data <- data[data$fidelity.weight<=1,]
# data <- data[data$smoothness.weight>0,]

null_value = 10^-10
resultat_asym <- mapply(function(x,y){
    all(
        sapply(0:2, function(p){
            all(
                sapply(0:(h-1), function(q){
                    tryCatch({
                        fst <- fst_filter(h,q,smoothness.weight = x,timeliness.weight = y,
                                          pdegree = p,
                                          smoothness.degree = 3)
                        coef <- fst$filters.coef
                        # coef
                        plot_coef(fst,zeroAsNa = TRUE)
                        # mat = diagnostic_matrix(coef, h)
                        if((p==0)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1))<null_value)){
                            p <- 1
                        }
                        if((p==1)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^2)<null_value)){
                            p <- p+1
                        }
                        if((p==2)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^3)<null_value)){
                            p <- p+1
                        }
                        validitySign(coef, p = p+1)
                    }, error = function(e) TRUE)
                })
            )
        })
    )
}, data$smoothness.weight, data$timeliness.weight)
resultat_sym <- mapply(function(x,y){
    all(
        sapply(0:2, function(p){
            tryCatch({
                fst <- fst_filter(h,h,smoothness.weight = x,timeliness.weight = y,
                                  pdegree = p,
                                  smoothness.degree = 3)
                coef <- fst$filters.coef
                # coef
                # plot_coef(fst)
                # diagnostic_matrix(coef, h)
                if((p==0)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1))<null_value)){
                    p <- 1
                }
                if((p==1)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^2)<null_value)){
                    p <- p+1
                }
                if((p==2)&&(sum(coef*seq(-h,length(coef) -h-1, by = 1)^3)<null_value)){
                    p <- p+1
                }
                validitySign(coef, p = p+1)
            }, error = function(e) TRUE)
        })
    )
}, data$smoothness.weight, data$timeliness.weight)
setwd("/Users/alainquartierlatente/Desktop/Carriere/AsymmetricFilters/")
saveRDS(resultat_sym,
        file = "henderson_theorem/resultats_sym.RDS")
saveRDS(resultat_asym,
        file = "henderson_theorem/resultats_asym.RDS")
saveRDS(data,
        file = "henderson_theorem/data_test.RDS")

mean(resultat_asym)
mean(resultat_sym)
which(!resultat)
data[which(!resultat_asym),]

x=data$smoothness.weight[6589]
y=data$timeliness.weight[6589]
x+y
resultat_asym <- readRDS("henderson_theorem/resultats_asym.RDS")
data <- readRDS(file = "henderson_theorem/data_test.RDS")
library(plotly)
data_nonok <- data[which(!resultat_asym),]
plot_ly(x=data_nonok$fidelity.weight,
        y=data_nonok$smoothness.weight,
        z=data_nonok$timeliness.weight,
        type="scatter3d", mode="markers"
) %>%
    layout(scene = list(xaxis = list(title = 'Fidelity'),
                        yaxis = list(title = 'Smoothness'),
                        zaxis = list(title = 'Timeliness'))) 
resultat
p=1
sapply()


p=5
sapply(0:h, function(q){
    fst <- fst_filter(h,q,smoothness.weight = 0.5,timeliness.weight = .4,pdegree = p)
    coef <- fst$filters.coef
    validitySign(coef)
})
all

#########@


c(w)
chol2inv(sqrt(diag(ker))%*%X)

?solve
X_gen()
p=1
h=6
t <- rjdfilters::get_kernel("Henderson", horizon = 6)
h_k <- c(rev(t$coef),t$coef[-1])
tmp <- h_k*sapply(seq(-h, h), P_u)
tmp / sum(tmp)


