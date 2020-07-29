library(rjdfilters)
library(plotly)

all_fst_res <- function(lags=6, leads=0, pdegree=2, smoothness.weight=1, smoothness.degree=3, 
                        timeliness.weight=0, timeliness.passband=pi/6, timeliness.antiphase=T,
                        resolution=100){
  data <- expand.grid(smoothness.weight = seq(0,1,length.out = resolution),
                      timeliness.weight = seq(0,1,length.out = resolution))
  data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
  sym_filter <- filterproperties(lags, kernel = "Henderson")$filters.coef[,sprintf("q=%i",lags)]
  resultat <- t(mapply(function(x,y){
    tryCatch({
      filter <- fstfilter(lags = lags, leads = leads, pdegree=pdegree, 
                          smoothness.weight=x, smoothness.degree=smoothness.degree,
                          timeliness.weight=y, timeliness.passband=timeliness.passband, timeliness.antiphase=timeliness.antiphase)
      
      c(filter$criterions,
        mse(sweights = sym_filter, filter$filter,passband = timeliness.passband))
    }, error = function(e) rep(NA,7))
    
  }, data$smoothness.weight, data$timeliness.weight))
  
  data <- rbind(data,
                matrix(NA,ncol = ncol(data), nrow = 4, dimnames = list(NULL,colnames(data))))
  
  lpp_stats <- t(sapply(c("LC","QL","CQ","DAF"), function(endpoints){
    a_coef <- filterproperties(lags, kernel = "Henderson", endpoints = endpoints)$filters.coef[,sprintf("q=%i",leads)]
    a_coef <- rjdfilters:::trailingZeroAsNa(a_coef)
    a_coef <- na.omit(a_coef)
    c(fst(a_coef, lb = lags, passband = timeliness.passband)$criterions,
      mse(sweights = sym_filter, a_coef,
          passband = timeliness.passband))
  }))
  
  resultat <- rbind(resultat, lpp_stats)
  cbind(data, resultat)
  # list(weights = data, results = resultat)
}
plotly_guguemos <- function(x, color = "timeliness.weight", mode = "markers", lpp_filters = TRUE){
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"Fidelity"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"Smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f x 10^-3', x$timeliness.weight, x[,"Timeliness"]*1000))
  n <- nrow(x)
  legend_pos <- c(LC = n-3, QL = n-2, CQ = n-1, DAF = n)
  text_legend[legend_pos] <- paste0(c("LC</br>","QL</br>", "CQ</br>", "DAF</br>"),
                                    text_legend[legend_pos])
  if(!lpp_filters){
    text_legend <- text_legend[-legend_pos]
    x <- x[-legend_pos,]
  }
  plot_ly(x=x[,"Fidelity"],
          y=x[,"Smoothness"],
          z=x[,"Timeliness"],
          type="scatter3d", mode=mode, color=x[,color],
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
plotly_wildi <- function(x, color = "timeliness.weight", mode = "markers", lpp_filters = TRUE){
  
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"accuracy"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f', x$timeliness.weight, x[,"timeliness"]),
                        sprintf('</br> Residuals: %.3f', x[,"residual"]))
  
  n <- nrow(x)
  
  legend_pos <- c(LC = n-3, QL = n-2, CQ = n-1, DAF = n)
  text_legend[legend_pos] <- paste0(c("LC</br>","QL</br>", "CQ</br>", "DAF</br>"),
                                 text_legend[legend_pos])
  if(!lpp_filters){
    text_legend <- text_legend[-legend_pos]
    x <- x[-legend_pos,]
  }
  # annotations <- lapply(names(legend_pos), function(ep){
  #   x_p <- legend_pos[ep]
  #   list(
  #     x = x[x_p, "accuracy"],
  #     y = x[x_p, "smoothness"],
  #     z = x[x_p, "timeliness"],
  #     text = ep,
  #     textangle = 0,
  #     ax = 0,
  #     ay = -75,
  #     font = list(
  #       color = "black",
  #       size = 12
  #     ),
  #     arrowcolor = "black",
  #     arrowsize = 3,
  #     arrowwidth = 1,
  #     arrowhead = 1
  #   )
  # })
  plot_ly(x=x[,"accuracy"],
          y=x[,"smoothness"],
          z=x[,"timeliness"],
          type="scatter3d", mode=mode, color=x[,color],
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

