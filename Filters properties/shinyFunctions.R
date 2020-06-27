library(rjdfilters)
library(ggplot2)

all_filters <- function(horizon = 3, degree = 3, ic = 4.5){
  t <- lapply(c("LC", "QL", "CQ", "CC", "DAF"), function(endpoints){
    t <- lapply(c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),function(kernel){
      
      tryCatch(filterproperties(horizon = horizon, degree = degree,
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
  # col_to_plot <- sprintf("q=%i", q)
  # col_to_plot <- col_to_plot[col_to_plot %in% colnames(filters_properties[[1]][[1]]$filters.coef)]
  # if (length(col_to_plot) == 0)
  #   return(NULL)
  # if(fixed == "endpoints"){
  #   d <- do.call(rbind,lapply(names(filters_properties[[endpoints]]),function(kernel){
  #     data <- filters_properties[[endpoints]][[kernel]]$filters.coef
  #     data <- rbind(data,c(rep(0, ncol(data)-1),NA))
  #     h <- (nrow(data)-1)/2
  #     x_values <- sapply(1:ncol(data),function(i){
  #       c(seq.int(1,h + i-1), rep(h+i, 2), seq_len(h+1-i)+(h+i))
  #     })
  #     colnames(x_values) <- colnames(data)
  #     do.call(rbind,lapply(col_to_plot, function(col){
  #       data.frame(y = data[,col], 
  #                  by = kernel, 
  #                  x = factor(rownames(data)[x_values[,col]],ordered = TRUE,levels = rownames(data)),
  #                  q = col,
  #                  stringsAsFactors = F,row.names = NULL)
  #     }))
  #   }))
  # }else{
  #   d <- do.call(rbind,lapply(names(filters_properties),function(endpoints){
  #     data <- filters_properties[[endpoints]][[kernel]]$filters.coef
  #     data <- rbind(data,c(rep(0, ncol(data)-1),NA))
  #     h <- (nrow(data)-1)/2
  #     x_values <- sapply(1:ncol(data),function(i){
  #       c(seq.int(1,h + i-1), rep(h+i, 2), seq_len(h+1-i)+(h+i))
  #     })
  #     colnames(x_values) <- colnames(data)
  #     do.call(rbind,lapply(col_to_plot, function(col){
  #       data.frame(y = data[,col], 
  #                  by = endpoints, 
  #                  x = factor(rownames(data)[x_values[,col]],ordered = TRUE,levels = rownames(data)),
  #                  q = col,
  #                  stringsAsFactors = F,row.names = NULL)
  #     }))
  #   }))
  # }
  # ggplot(data = d, aes(x = x, y = y, group = q, 
  #                      colour = q)) + 
  #   geom_line(size = 0.7) + 
  #   geom_point(size = 1) +
  #   facet_wrap(~by) +
  #   theme(panel.background = element_rect(fill = "white", colour = NA), 
  #         panel.border = element_rect(fill = NA, colour = "grey20"),
  #         panel.grid.major = element_line(colour = "grey92"), 
  #         panel.grid.minor = element_line(colour = "grey92", 
  #                                         size = 0.25),
  #         strip.background = element_rect(fill = "grey85", colour = "grey20"),
  #         complete = TRUE, plot.title = element_text(hjust = 0.5),
  #         legend.title=element_blank()) +
  #   labs(x = NULL, y = "Coefficients")
  #####
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

gain_phase_plot_comp <- function(filters_properties, endpoints, kernel, q,
                                     which = "gain", fixed = "endpoints"){
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
  x_lab_at <- seq(0, 1, length.out = nxlab)
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
                       labels = parse(text=xlabel(x_lab_at)))
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

