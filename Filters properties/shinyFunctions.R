library(rjdfilters)
library(ggplot2)

all_filters <- function(horizon = 6, degree = 3, ic = 3.5){
  t <- lapply(c("LC", "QL", "CQ", "CC", "DAF"), function(endpoints){
    t <- lapply(c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),function(kernel){
      
      tryCatch(lpp_properties(horizon = horizon, degree = degree,
                                kernel = kernel, endpoints = endpoints, ic = ic),
               error = function(e){
                 NULL
               })
    }
    )
    names(t) <- c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic")
    t
  })
  names(t) <- c("LC", "QL", "CQ", "CC", "DAF")
  t
}

coef_plot_comp <- function(filters_properties, endpoints, kernel, q, fixed = "endpoints"){
  if(fixed == "endpoints"){
    d <- do.call(rbind,lapply(names(filters_properties[[endpoints]]),function(kernel){
      data <- filters_properties[[endpoints]][[kernel]]$filters.coef
      data.frame(data, by = kernel, x = factor(rownames(data),ordered = TRUE,levels = rownames(data)),
                 stringsAsFactors = F,row.names = NULL)
    }))
  }else{
    d <- do.call(rbind,lapply(names(filters_properties),function(endpoints){
      data <- filters_properties[[endpoints]][[kernel]]$filters.coef
      data.frame(data, by = endpoints, x = factor(rownames(data),ordered = TRUE,levels = rownames(data)),
                 stringsAsFactors = F,row.names = NULL)
    }))
    d$by <- factor(d$by, ordered = TRUE, levels = c("LC", "QL", "CQ", "CC", "DAF"))
  }

  colnames(d) <- gsub(".", "=", colnames(d), fixed = TRUE)
  col_to_plot <- sprintf("q=%i", q)
  col_to_plot <- col_to_plot[col_to_plot %in% colnames(d)]
  col_to_plot <- c(col_to_plot, "by", "x")
  if (length(col_to_plot) == 2)
    return(NULL)

  d <- d[,col_to_plot]
  dataGraph <- reshape2::melt(d, id = c("x","by"))

  ggplot(data = dataGraph, aes(x = x, y = value, group = variable,
                               colour = variable)) +
    geom_line(size = 0.7) +
    geom_point(size = 1) +
    facet_wrap(~by) +
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"),
          panel.grid.minor = element_line(colour = "grey92",
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = "Coefficients")
}

diagnostic_table<- function(x,
                            horizon = input$horizon){
  sweight <- x[[1]][["Henderson"]]$filters.coef[,sprintf("q=%i", horizon)]
  res <- do.call(rbind, lapply(names(x),function(endpoints){
    do.call(rbind, lapply(names(x[[endpoints]]),function(kernel){
      data <- apply(x[[endpoints]][[kernel]]$filters.coef,2,diagnostics_matrix, lb = horizon,
                    sweight = sweight)
      data <- data[,-ncol(data)]
      data <- t(data)
      data.frame(kernel = kernel, method = endpoints, q = rownames(data), data,
                 stringsAsFactors = FALSE)
    }))
  }))
  rownames(res) <- NULL
  timeliness_col <- grep("T_g",colnames(res))
  res[,timeliness_col] <- round(res[,timeliness_col],6)
  res[,-c(1:3,timeliness_col)] <- round(res[,-c(1:3,timeliness_col)],3)
  # colnames(res)[-c(1:3)] <- paste0("$$", colnames(res)[-c(1:3)] ,"$$")
  res
  # rownames(data) <- paste0("$",rownames(data) ,"$")
  # data <- data.frame(rownames(data), data)
  # colnames(data) <- c("Criteria", col_to_plot)
  # rownames(data) <- NULL
  # data
  
}
gain_phase_plot_comp <- function(filters_properties, endpoints, kernel, q,
                                     which = "gain", fixed = "endpoints",
                                 xlim = c(0, pi)){
  if (which == "gain"){
    component = "filters.gain"
    ylab = "Gain"
  }else{
    component = "filters.phase"
    ylab = "Phase"
  }
  if(fixed == "endpoints"){
    d <- do.call(rbind,lapply(names(filters_properties[[endpoints]]),function(kernel){
      data <- filters_properties[[endpoints]][[kernel]][[component]]
      data.frame(data, by = kernel,
                 x = seq(0, pi, length.out = nrow(data)),
                 stringsAsFactors = F,row.names = NULL)
    }))
  }else{
    d <- do.call(rbind,lapply(names(filters_properties),function(endpoints){
      data <- filters_properties[[endpoints]][[kernel]][[component]]
      data.frame(data, by = endpoints,
                 x = seq(0, pi, length.out = nrow(data)),
                 stringsAsFactors = F,row.names = NULL)
    }))
  }
  
  colnames(d) <- gsub(".", "=", colnames(d), fixed = TRUE)
  col_to_plot <- sprintf("q=%i", q)
  col_to_plot <- col_to_plot[col_to_plot %in% colnames(d)]
  col_to_plot <- c(col_to_plot, "by", "x")
  if (length(col_to_plot) == 2)
    return(NULL)
  d <- d[,col_to_plot]
  dataGraph <- reshape2::melt(d, id = c("x","by"))
  if (which != "gain"){
    y_changed <- dataGraph$g != 0
    dataGraph$value[y_changed] <- (dataGraph$value / dataGraph$x)[y_changed]
  }
  nxlab = 7
  # x_lab_at <- seq(0, 1, length.out = nxlab)
  x_lab_at <- seq(xlim[1]/pi, xlim[2]/pi, length.out = nxlab)
  ggplot(data = dataGraph, aes(x = x, y = value, group = variable, 
                               colour = variable)) + 
    geom_line(size = 0.7) + 
    facet_wrap(~by) +
    theme(panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major = element_line(colour = "grey92"), 
          panel.grid.minor = element_line(colour = "grey92", 
                                          size = 0.25),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          complete = TRUE, plot.title = element_text(hjust = 0.5),
          legend.title=element_blank()) +
    labs(x = NULL, y = ylab) +
    scale_x_continuous(NULL, 
                       breaks = x_lab_at*pi,
                       labels = parse(text=xlabel(x_lab_at)),
                       limits = xlim)
}
xlabel <- function (x, symbol = "pi") {
  fracs <- strsplit(attr(MASS::fractions(x), "fracs"), "/")
  labels <- sapply(fracs, function(i) if (length(i) > 1) {
    paste(i[1], "*", symbol, "/", i[2])
  }
  else {
    paste(i, "*", symbol)
  })
  labels[1] <- "0"
  labels <- sub("1 * pi", " pi", labels, fixed = TRUE)
  labels
}

