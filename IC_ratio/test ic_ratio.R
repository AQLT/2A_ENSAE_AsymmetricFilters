library(rjdfilters)
method = "LC"
horizon = 6
kernel = "Henderson"
ic_values = seq(0.1, 30, length.out = 2000)

estimate <- function(method = "LC", kernel = "Henderson",
                     horizon = 6){
    lapply(ic_values, function(ic){
        lp_filter(horizon = horizon, endpoints = method,
                       kernel = kernel, ic = ic)$filters.coef
    }) 
}
for (kernel in  c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube",
                  "Gaussian", "Triangular", "Parabolic")){
    for (horizon in 3:12){
        for (method in c("LC", "QL", "CQ", "DAF")){
            file = sprintf("data/%s_%s_h%i.RDS", method, kernel, horizon)
            print(file)
            if(!file.exists(file)){
                res <- estimate(method = method, kernel = kernel, horizon = horizon)
                saveRDS(res, file)
            }
            zip("data.zip", list.files("data", full.names = TRUE))
        }
    } 
}
for (kernel in  c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube",
                  "Gaussian", "Triangular", "Parabolic")){
    for (horizon in 3:12){
        for (method in c("LC", "QL", "CQ", "DAF")){
            file = sprintf("data_coefs/%s_%s_h%i.fst", method, kernel, horizon)
            print(file)
            d <- extract_all(kernel = kernel,h = horizon,method = method)
            fst::write.fst(d, file)
        }
    } 
}
sapply(list.files(path="data",full.names = TRUE),function(f){
    print(f)
    d <- readRDS(f)
    names(d)
    fst::write.fst(d, gsub(".RDS",".fst",f))
    NULL
})

extract_q <- function(res, q=0){
    col_ = sprintf("q=%i",q)
    if(length(grep(col_, colnames(res[[1]])))==0){
        NULL
    }else{
        do.call(rbind, lapply(seq_along(ic_values), function(i){
            data = res[[i]][,col_]
            x_num = as.numeric(gsub("t","",names(data)))
            x_num[is.na(x_num)] <- 0
            x = names(data)
            z = data
            y = ic_values[i]
            data = data.frame(x=x,y=y,z=z, row.names = NULL)
            data[x_num>q,c("y","z")] <- NA
            data
        }))
    }
}
kernel = c("Henderson", "Uniform")
method = c("LC","QL")
m = method[1]
q = 0
h = 4
setwd("IC_ratio")
all

all_data <- extract_all(kernel = c("Henderson"),
                        method = c("LC"),
                        h=3:12,
                        q=0:12)
q0 <- extract_q(res, 0)
q1 <- extract_q(res, 1)
q2 <- extract_q(res, 2)
q3 <- extract_q(res, 3)
q4 <- extract_q(res, 4)
q5 <- extract_q(res, 5)


# library("plot3D")
# scatter3D(q0$x, q0$y, q0$z)

library(plotly)

plot_ly(x=q0$x,
        y=q0$y,
        z=q0$z,
        type="scatter3d", mode="markers"
) 
plot_ly(x=q1$x,
        y=q1$y,
        z=q1$z,
        type="scatter3d", mode="markers"
)
plot_ly(x=q2$x,
        y=q2$y,
        z=q2$z,
        type="scatter3d", mode="markers"
) 
plot_ly(x=q3$x,
        y=q3$y,
        z=q3$z,
        type="scatter3d", mode="markers"
) 
plot_ly(x=q4$x,
        y=q4$y,
        z=q4$z,
        type="scatter3d", mode="markers"
) 

plot_ly(x=q5$x,
        y=q5$y,
        z=q5$z,
        type="scatter3d", mode="markers"
) 

fig <- plot_ly(z = ~volcano)
fig <- fig %>% add_surface()

fig

plot_ly(x=q0$x,
        y=q0$y,
        z=q0$z
) %>% add_surface()

fig

kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
kd$z

library(plotly)
plot_ly() %>% 
    add_trace(data = q0,  x=q0$x, y=q0$y, z=q0$z, type="mesh3d" )

library(lattice) 
wireframe(q0$z ~ q0$x * q0$y,
          xlab="Parameter 1", ylab="Parameter 2", zlab="Fitness Value")

set.seed(123)

# Create Random Data
ds <- diamonds[sample(1:nrow(diamonds), size = 1000),]

# Create lists for axis properties
f1 <- list(
    family = "Arial, sans-serif",
    size = 18,
    color = "lightgrey")

f2 <- list(
    family = "Old Standard TT, serif",
    size = 14,
    color = "#ff9999")

axis <- list(
    titlefont = f1,
    tickfont = f2,
    showgrid = F
)

scene = list(
    xaxis = axis,
    yaxis = axis,
    zaxis = axis,
    camera = list(eye = list(x = -1.25, y = 1.25, z = 1.25)))


fig <- plot_ly(ds, x = ~carat, y = ~cut, z = ~price, type = 'scatter3d', mode = 'markers', marker = list(size = 3))
fig <- fig %>% layout(title = "3D Scatter plot", scene = scene)

fig

unzip(file.choose())
