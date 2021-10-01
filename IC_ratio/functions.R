
ic_values = seq(0.1, 30, length.out = 2000)
extract_q <- function(res, q=0){
    do.call(rbind, lapply(seq_along(ic_values), function(i){
        data = res[[i]][,sprintf("q=%i",q)]
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

extract_all <- function(kernel = "Henderson", h = 6, method = "LC"){
    if(is.null(kernel) || is.null(h) ||is.null(method))
        return (NULL)
    do.call(rbind, lapply(h, function(h_){
        do.call(rbind, lapply(kernel, function(k){
            do.call(rbind, lapply(method, function(m){
                file <- sprintf("data_coefs/%s_%s_h%i.fst",m,k,h_)
                fst::read.fst(file)
            }))
        })) 
    }))
}
