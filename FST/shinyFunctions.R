library(rjdfilters)
library(plotly)

all_fst_res <- function(lags=6, leads=0, pdegree=2, smoothness.weight=1, smoothness.degree=3, 
                        timeliness.weight=0, timeliness.passband=pi/6, timeliness.antiphase=T,
                        resolution=100){
  data <- expand.grid(smoothness.weight = seq(0,1,length.out = resolution),
                      timeliness.weight = seq(0,1,length.out = resolution))
  data$fidelity.weight <- 1 - (data$smoothness.weight + data$timeliness.weight)
  data <- data[data$smoothness.weight + data$timeliness.weight<=1,]
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
  cbind(data, resultat)
  # list(weights = data, results = resultat)
}
plotly_guguemos <- function(x, color = "timeliness.weight", mode = "markers"){
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"Fidelity"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"Smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f x 10^-3', x$timeliness.weight, x[,"Timeliness"]*1000))
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
plotly_wildi <- function(x, color = "timeliness.weight", mode = "markers"){
  title <- sprintf("Color = %s", gsub(".", " ", color, fixed = TRUE))
  text_legend <- paste0(sprintf('</br> Fidelity (%.3f): %.3f', x$fidelity.weight, x[,"accuracy"]),
                        sprintf('</br> Smoothness (%.3f): %.3f', x$smoothness.weight, x[,"smoothness"]),
                        sprintf('</br> Timeliness (%.3f): %.3f', x$timeliness.weight, x[,"timeliness"]),
                        sprintf('</br> Residuals: %.3f', x[,"residual"]))
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
        xaxis = list(title = "Fidelity"),
        yaxis = list(title = "Smoothness"),
        zaxis = list(title = "Timeliness")
      ))
}

