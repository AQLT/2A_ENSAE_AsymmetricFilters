data = readRDS(sprintf("comparaison_fst/data/comparison_fst_weights_h%s_ic%.1f.RDS", 6, 3.5))
q = 0
method = "LC"
degree = 0
data_tri <- data[(data$q %in% as.numeric(q)) &
                     (data$method %in% method) &
                     (data$degree %in% as.numeric(degree)),]

library(plot3D)

global_plot <- function(data, q, method, degree, phi = 40,
                        theta = 40,
                        titre = NULL){
    
    data_tri <- data[(data$q %in% as.numeric(q)) &
                         (data$method %in% method) &
                         (data$degree %in% as.numeric(degree)),]
    scatter_3D(data_tri, titre = titre, phi = phi, theta = theta)
}

par(mfrow=c(2,3))
par(mar = 0 + c(1, 0, 1, 0))
par (mai = c(0.2, 0.2, 0.2, 0.2))
scatter_3D <- function(x, titre = NULL, phi = 40,
                       theta = 40){
    add <- nrow(x) >0
    if(add){
        with(x, 
             scatter3D(x = fidelity.weight,
                       y = smoothness.weight,
                       z = timeliness.weight,
                       colvar = NULL, 
                       phi = phi, theta = theta,
                       # bty = "g",
                       pch = 1,
                       # cex = 0.1, alpha = 0.4,
                       ticktype = "detailed",
                       xlim = c(0,1),
                       ylim = c(0,1),
                       zlim = c(0,1), 
                       xlab = "\n\nFidelity",
                       ylab ="\n\nSmoothness",
                       zlab = "\n\nTimeliness",
                       main = titre))
        polygon3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0),
                  add = add, alpha = 0.2,
                  ticktype = "detailed",
                  phi = phi, theta = theta,
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }else{
        scatter3D(x = -2,
                  y = 2,
                  z = 2,
                  colvar = NULL, 
                  phi = phi, theta = theta,
                  # bty = "g",
                  pch = 1,
                  # cex = 0.1, alpha = 0.4,
                  ticktype = "detailed",
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }
}
# LC
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 0)
    global_plot(data, q = q, method = "LC", degree = 0, theta = 150,
                titre = titre)
}

for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "LC", degree = 1, theta = 150,
                titre = titre)
}
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "LC", degree = 2, theta = 150,
                titre = titre)
}

for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "LC", degree = 3, theta = 150,
                titre = titre)
}

# QL
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "QL", degree = 1, theta = 150,
                titre = titre)
}
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 2)
    global_plot(data, q = q, method = "QL", degree = 2, theta = 150,
                titre = titre)
}

for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 3)
    global_plot(data, q = q, method = "QL", degree = 3, theta = 150,
                titre = titre)
}

# CQ
for(q in 0:5){
    titre <- sprintf("CQ - q = %i, degree = %i", q, 2)
    global_plot(data, q = q, method = "CQ", degree = 2, theta = 150,
                titre = titre)
}

for(q in 0:5){
    titre <- sprintf("CQ - q = %i, degree = %i", q, 3)
    global_plot(data, q = q, method = "CQ", degree = 3, theta = 150,
                titre = titre)
}

# DAF
for(q in 0:5){
    titre <- sprintf("DAF - q = %i, degree = %i", q, 3)
    global_plot(data, q = q, method = "DAF", degree = 3, theta = 150,
                titre = titre)
}


# Phase
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 0)
    global_plot(data, q = q, method = "phase", degree = 0, theta = 150,
                titre = titre)
}
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "phase", degree = 1, theta = 150,
                titre = titre)
}


# Gain
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 0)
    global_plot(data, q = q, method = "gain", degree = 0, theta = 150,
                titre = titre)
}
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "gain", degree = 1, theta = 150,
                titre = titre)
}

# frf
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 0)
    global_plot(data, q = q, method = "frf", degree = 0, theta = 150,
                titre = titre)
}
for(q in 0:5){
    titre <- sprintf("q = %i, degree = %i", q, 1)
    global_plot(data, q = q, method = "frf", degree = 1, theta = 150,
                titre = titre)
}


global_plot(data, q = q, method = "LC", degree = 0)
global_plot(data, q = q, method = "LC", degree = 0, theta = 0)
global_plot(data, q = q, method = "LC", degree = 0, theta = 50)
global_plot(data, q = q, method = "LC", degree = 0, theta = 100)
global_plot(data, q = q, method = "LC", degree = 0, theta = 150)

data = readRDS(sprintf("comparaison_fst/data/comparison_fst_weights_h%s_ic%.1f.RDS", 11, 3.5))

par(mfrow = c(4,3), 
    # mar = 0 + c(0.5, 0, .5, 0),
    mai = c(0, 0.1, 0.1, 0.1)
    )
par("mai")
par("mar")
par(mar = 0 + c(0.5, 0, .5, 0))
par(mfrow=c(1,1))
for(q in 0:11){
    titre <- sprintf("q = %i, degree = %i", q, 0)
    # global_plot(data, q = q, method = "LC", degree = 0, theta = 150,
    #             titre = titre)
    polygon3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0),
              add = F, alpha = 0.2,
              ticktype = "detailed",
              phi = 40, theta = 150,
              xlim = c(0,1),
              ylim = c(0,1),
              zlim = c(0,1),
              xlab = "\n\nFidelity",
              ylab ="\n\nSmoothness",
              zlab = "\n\nTimeliness",
              main = titre)
    # plot(sin, -pi, 2*pi) # see ?plot.function
}



scatter_3D(data_tri, titre)
with(data_tri, 
     scatter3D(smoothness.weight,
               fidelity.weight,
               timeliness.weight,
               colvar = NULL, #phi = 0, 
               # bty = "g",
               pch = 20,
               # cex = 2, 
               ticktype = "detailed",
               xlim = c(0,1),
               ylim = c(0,1),
               zlim = c(0,1)))


resolution = 6
data <- expand.grid(x = c(seq(0,1,length.out = resolution)),
                    y = c(seq(0,1,length.out = resolution))
)
data$z <- 1 - (data$x + data$y)
data <- data[data$z<=1,]

oldmar <- par("mar")
oldmai <- par("mai")

par(mar = 0 + c(1, 1, 1, 1)+2)
par (mar = 0 + c(1, 1, 1, 1)+4,
     mai = oldmai)

library(plot3D)
scatter3D(x = 0, y = 0, z = 0,
          ticktype = "detailed",
          xlim = c(0,1), ylim = c(0,1),  zlim = c(0,1), 
          xlab = "Toto",
          ylab ="Tata",
          zlab = "Titi")





install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
scatterplot3d()
with(data_tri, 
     scatterplot3d(x = fidelity.weight,
               y = smoothness.weight,
               z = timeliness.weight,
               # colvar = NULL, phi = 20, 
               # bty = "g",
               # pch = 20,
               # cex = 2, 
               # ticktype = "detailed",
               xlim = c(0,1),
               ylim = c(0,1),
               zlim = c(0,1), 
               xlab = "Fidelity",
               ylab ="Smoothness",
               zlab = "Timeliness",
               box=FALSE))
