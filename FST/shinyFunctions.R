library(rjdfilters)
library(plotly)

rkhs <- readRDS("rkhs.RDS")
tlpp_lc <- readRDS("timeliness_lpp_lc.RDS")
tlpp_ql <- readRDS("timeliness_lpp_ql.RDS")
tlpp <- lapply(names(tlpp_lc), function(q){
  list(results = rbind(tlpp_lc[[q]]$results,
                       tlpp_ql[[q]]$results),
       text_guggemos = c(tlpp_lc[[q]]$text_guggemos,
                         tlpp_ql[[q]]$text_guggemos),
       text_wildi = c(tlpp_lc[[q]]$text_wildi,
                         tlpp_ql[[q]]$text_wildi),
       text_both = c(tlpp_lc[[q]]$text_both,
                     tlpp_ql[[q]]$text_both),
       legend = rep("Local polynomial (Timeliness)", 2*nrow(tlpp_lc[[q]]$results))
       )
})
names(tlpp) <- names(tlpp_lc)

all_fst_res <- function(lags=6, leads=0, pdegree=2, smoothness.weight=1, smoothness.degree=3, 
                        timeliness.weight=0, timeliness.passband=pi/6, timeliness.antiphase=T,
                        resolution=100){
  data <- expand.grid(smoothness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000),
                      timeliness.weight = c(seq(0,1,length.out = resolution),1-1/1000,1-1/2000,1-1/3000,1-1/4000,1-1/5000)
                      )
  data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
  sym_filter <- filterproperties(lags, kernel = "Henderson")$filters.coef[,sprintf("q=%i",lags)]
  resultat <- t(mapply(function(x,y){
    tryCatch({
      filter <- fstfilter(lags = lags, leads = leads, pdegree=pdegree, 
                          smoothness.weight=x, smoothness.degree=smoothness.degree,
                          timeliness.weight=y, timeliness.passband=timeliness.passband, timeliness.antiphase=timeliness.antiphase)
      
      c(filter$criteria,
        mse(sweights = sym_filter, filter$filter,passband = timeliness.passband))
    }, error = function(e) rep(NA,7))
    
  }, data$smoothness.weight, data$timeliness.weight))
  
  data <- rbind(data,
                matrix(NA,ncol = ncol(data), nrow = 4, dimnames = list(NULL,colnames(data))))
  
  lpp_stats <- t(sapply(c("LC","QL","CQ","DAF"), function(endpoints){
    a_coef <- filterproperties(lags, kernel = "Henderson", endpoints = endpoints)$filters.coef[,sprintf("q=%i",leads)]
    a_coef <- rjdfilters:::trailingZeroAsNa(a_coef)
    a_coef <- na.omit(a_coef)
    c(fst(a_coef, lb = lags, passband = timeliness.passband)$criteria,
      mse(sweights = sym_filter, a_coef,
          passband = timeliness.passband))
  }))
  resultat <- rbind(resultat, lpp_stats)
  
  if(lags == 6){
    res_rkhs <- t(sapply(rkhs,function(x){
      a_coef <- x$weight[,sprintf("q=%i", leads)]
      a_coef <- rjdfilters:::trailingZeroAsNa(a_coef)
      a_coef <- na.omit(a_coef)
      c(fst(a_coef, lb = lags, passband = timeliness.passband)$criteria,
        mse(sweights = sym_filter, a_coef,
            passband = timeliness.passband))
    }))
    data <- rbind(data,
                  matrix(NA,ncol = ncol(data), nrow = 3, dimnames = list(NULL,colnames(data))))
    resultat <- rbind(resultat, res_rkhs)
  }else{
    data <- rbind(data,
                  matrix(NA,ncol = ncol(data), nrow = 3, dimnames = list(NULL,colnames(data))))
    resultat <- rbind(resultat,
                      matrix(NA,ncol = ncol(resultat), nrow = 3, dimnames = list(NULL,colnames(resultat))))
  }
  
  cbind(data, resultat)
  # list(weights = data, results = resultat)
}
plotly_guguemos <- function(x, color = "timeliness.weight", mode = "markers", lpp_filters = TRUE,
                            rkhs_filters = TRUE,
                            rkhs_compute = TRUE,
                            data_tlpp = NULL){
  if(color == "default"){
    color <- "timeliness.weight"
  }
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"Fidelity"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"Smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f x 10^-3', x$timeliness.weight, x[,"Timeliness"]*1000))
  n <- nrow(x)
  if(rkhs_compute){
    legend_method <- c(rep("FST",n-7), rep("Local polynomial",4), rep("RKHS",3))
  }else{
    legend_method <- c(rep("FST",n-4), rep("Local polynomial",4))
  }
  
  
  legend_pos <- c(LC = n-6, QL = n-5, CQ = n-4, DAF = n-3)
  if(!rkhs_compute){
    legend_pos <- legend_pos + 3
  }
  
  text_legend[legend_pos] <- paste0(c("LC</br>","QL</br>", "CQ</br>", "DAF</br>"),
                                    text_legend[legend_pos])
  if(!lpp_filters){
    text_legend <- text_legend[-legend_pos]
    x <- x[-legend_pos,]
    legend_method <- legend_method[-legend_pos]
  }
  if(rkhs_compute){
    n <- nrow(x)
    legend_pos <- c(rkhs_frf = n-2, rkhs_gain = n-1, rkhs_phase = n)
    text_legend[legend_pos] <- paste0(c("RKHS FrF</br>","RKHS Gain</br>", "RKHS Phase</br>"),
                                      text_legend[legend_pos])
    if(!rkhs_filters){
      text_legend <- text_legend[-legend_pos]
      x <- x[-legend_pos,]
      legend_method <- legend_method[-legend_pos]
    }
  }
  
  if(!is.null(data_tlpp)){
    x <- rbind(x, data_tlpp$results)
    text_legend <- c(text_legend, data_tlpp$text_wildi)
  }
  
  if(color == "method"){
    x_color <- c(legend_method,
                 data_tlpp$legend)
  }else{
    x_color <- x[,color]
  }
  
  plot_ly(x=x[,"Fidelity"],
          y=x[,"Smoothness"],
          z=x[,"Timeliness"],
          type="scatter3d", mode=mode, color=x_color,
          hoverinfo = 'text',
          text = text_legend
  ) %>% 
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "Fidelity"),
        yaxis = list(title = "Smoothness"),
        zaxis = list(title = "Timeliness")
      ))
}
plotly_wildi <- function(x, color = "timeliness.weight", mode = "markers", lpp_filters = TRUE,
                         rkhs_filters = TRUE,
                         rkhs_compute = TRUE,
                         data_tlpp = NULL){
  if((color == "default")){
    color <- "timeliness.weight"
  }
  
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"accuracy"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f', x$timeliness.weight, x[,"timeliness"]),
                        sprintf('</br> Residuals: %.3f', x[,"residual"]))
  
  n <- nrow(x)
  if(rkhs_compute){
    legend_method <- c(rep("FST",n-7), rep("Local polynomial",4), rep("RKHS",3))
  }else{
    legend_method <- c(rep("FST",n-4), rep("Local polynomial",4))
  }
  
  
  legend_pos <- c(LC = n-6, QL = n-5, CQ = n-4, DAF = n-3)
  if(!rkhs_compute){
    legend_pos <- legend_pos + 3
  }
    
  text_legend[legend_pos] <- paste0(c("LC</br>","QL</br>", "CQ</br>", "DAF</br>"),
                                 text_legend[legend_pos])
  if(!lpp_filters){
    text_legend <- text_legend[-legend_pos]
    x <- x[-legend_pos,]
    legend_method <- legend_method[-legend_pos]
  }
  if(rkhs_compute){
    n <- nrow(x)
    legend_pos <- c(rkhs_frf = n-2, rkhs_gain = n-1, rkhs_phase = n)
    text_legend[legend_pos] <- paste0(c("RKHS FrF</br>","RKHS Gain</br>", "RKHS Phase</br>"),
                                      text_legend[legend_pos])
    if(!rkhs_filters){
      text_legend <- text_legend[-legend_pos]
      x <- x[-legend_pos,]
      legend_method <- legend_method[-legend_pos]
    }
  }
  
  if(!is.null(data_tlpp)){
    x <- rbind(x, data_tlpp$results)
    text_legend <- c(text_legend, data_tlpp$text_wildi)
  }
  
  if(color == "method"){
    x_color <- c(legend_method,
                 data_tlpp$legend)
  }else{
    x_color <- x[,color]
  }
  
  


  plot_ly(x=x[,"accuracy"],
          y=x[,"smoothness"],
          z=x[,"timeliness"],
          type="scatter3d", mode=mode, color=x_color,
          hoverinfo = 'text',
          text = text_legend
  ) %>% 
    layout(
      title = title,
      scene = list(
        xaxis = list(title = "Accuracy"),
        yaxis = list(title = "Smoothness"),
        zaxis = list(title = "Timeliness")
      ))
}

plot_2graph <- function(x, mode = "markers",
                            rkhs_compute = TRUE,
                        color = "default"){
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  x <- na.omit(x)
  
  text_legend <- paste0(sprintf('</br> Fidelity weight = %.3f', x$fidelity.weight),
                        sprintf('</br> Smoothness weight = %.3f', x$smoothness.weight),
                        sprintf('</br> Timeliness weight = %.3f ', x$timeliness.weight))
  text_legend <- c(paste0("Guggemos</br>", text_legend),
                   paste0("Wildi</br>", text_legend))
  x <- apply(x,2,function(x){
    (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  })
  if(color %in% c("default","method")){
    color_text <- c(rep("Guggemos", nrow(x)), rep("Wildi", nrow(x)))
  }else{
    color_text <- c(x[,color], x[,color])
  }
 
  data_plot <- data.frame(x = c(x[,"Fidelity"], x[,"accuracy"]),
             y = c(x[,"Smoothness"], x[,"smoothness"]),
             z = c(x[,"Timeliness"], x[,"timeliness"]),
             color = color_text)
  axis_title <- list(
    xaxis = list(title = "Fidelity/Accuracy"),
    yaxis = list(title = "Smoothness"),
    zaxis = list(title = "Timeliness")
  )
  
  # data_plot <- data.frame(x = c(x[,"Fidelity"], x[,"smoothness"]),
  #                         y = c(x[,"Smoothness"], x[,"accuracy"]),
  #                         z = c(x[,"Timeliness"], x[,"timeliness"]),
  #                         color = color_text)
  # axis_title <- list(
  #   xaxis = list(title = "Fidelity/Smoothness"),
  #   yaxis = list(title = "Smoothness/Accuracy"),
  #   zaxis = list(title = "Timeliness")
  # )
  
  plot_ly(data_plot,
          x=~x,
          y=~y,
          z=~z,
          type="scatter3d", mode=mode, color=~color,
          hoverinfo = 'text',
          text = text_legend
  ) %>% 
    layout(
      title = title,
      scene = axis_title)
}

