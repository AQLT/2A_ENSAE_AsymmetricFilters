library(zoo)
data <- as.data.frame(eurostat:::get_eurostat_raw("sts_inpr_m"))
data <- data[,c(1,seq(ncol(data),2, by = -1))]
data[,-1] <- apply(data[,-1],2, function(x){
    x[grep("c|u|s|b|d|z", x)] <- NA
    x <- gsub(" p", "", x)
    as.numeric(x)
})

indices_ligne <- strsplit(data[,1], ",")
indic_bt = sapply(indices_ligne,function(x)x[1])
nace_r2 = sapply(indices_ligne,function(x)x[2])
s_adj = sapply(indices_ligne,function(x)x[3])
unit = sapply(indices_ligne,function(x)x[4])
geo = sapply(indices_ligne,function(x)x[5])
i_unit <- unit == "I15"
i_s_adj <- s_adj == "CA"
i_geo <- geo %in% c("BE", "BG", "CZ", "DK", "DE",
					  "EE", "EL", "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU",
					  "HU", "MT", "NL", "AT", "PL", "PT", "RO", "SI", "SK", "FI", "SE",
					  "UK", "NO", "CH", "ME", "MK", "RS", "TR", "BA")
var_names <- sprintf("%s_%s",nace_r2,geo)
i_na <- !apply(is.na(data[,-1]),1,all)
i_retained <- which(i_geo & i_unit & i_s_adj & i_na)
ts_data <- t(data[i_retained,-1])
colnames(ts_data) <- var_names[i_retained]
first_data <- rownames(ts_data)[1]
start_y <- as.numeric(substr(first_data, 1, 4))
start_m <- as.numeric(substr(first_data, 6, 8))
ts_data <- ts(ts_data,
              start = c(start_y, start_m),
              frequency = 12)
ts_data = window(ts_data, start = 1990)
ts_data_l <- as.list(ts_data)
ts_data_l <- lapply(ts_data_l, na.trim)
ts_data_l <- ts_data_l[which(sapply(ts_data_l, function(x){
    (tail(time(x),1) > 2020.5) &  (length(time(x)) > 10*12)
}))]
ts_data_l <- ts_data_l[-which(sapply(ts_data_l,function(x) any(is.na(x))))]
# too much leading 0 or 0 should be NA
ts_data_l <- ts_data_l[! names(ts_data_l) %in% c("C26_MK", "B051_DE", "B07_MK", "B07_RO", "C1081_ES", "C1081_IT", "C12_FR", 
                                                 "C1391_IT", "C1431_IT", "C151_IT", "C1512_IT", "C234_IT", "C2341_IT", 
                                                 "C2342_IT", "C2361_IT", "C29_LV", "C30_EE", "C3012_EL", "C303_ES", 
                                                 "C32_MK", "C33_MK", "MIG_DCOG_ME")]



saveRDS(ts_data_l,"comparison_filters/eurostat_db.RDS")

length(ts_data_l)

