i_remove <- c(B_NL = 24L, B_PT = 27L, `B-D_PT` = 61L, B051_DE = 82L, B07_MK = 104L, 
              B07_RO = 106L, B07_HU = 107L, B08_PT = 134L, B_C_PT = 177L, B_C_X_MIG_NRG_PT = 202L, 
              C_PT = 232L, C10_PT = 259L, C1081_ES = 396L, C1081_IT = 397L, 
              C11_PT = 475L, C12_FR = 502L, C13_PT = 539L, C1391_IT = 562L, 
              C14_PT = 624L, C1431_IT = 658L, C151_IT = 689L, C15_LV = 690L, 
              C15_PT = 695L, C1512_IT = 700L, C16_PT = 741L, C17_PT = 807L, 
              C18_PT = 878L, C20_PT = 948L, C20_C21_PT = 1060L, C20_C21_SE = 1061L, 
              C21_PT = 1080L, C22_PT = 1112L, C23_PT = 1185L, C234_IT = 1217L, 
              C2341_IT = 1222L, C2342_IT = 1226L, C2361_IT = 1256L, C24_LV = 1331L, 
              C24_PT = 1336L, C24_C25_PT = 1424L, C25_PT = 1444L, C26_MK = 1554L, 
              C26_PT = 1585L, C26_C27_PT = 1641L, C27_PT = 1661L, C28_PT = 1755L, 
              C29_LV = 1858L, C29_PT = 1895L, C291_EL = 1901L, C30_EE = 1909L, 
              C30_EE = 1909L, C29_C30_PT = 1936L, C3012_EL = 1937L, C303_ES = 1947L, 
              C31_PT = 2018L, C32_MK = 2028L, C32_PT = 2068L, C33_MK = 2096L, 
              D_PT = 2209L, D35_PT = 2241L, MIG_CAG_PT = 2288L, MIG_DCOG_ME = 2305L, 
              MIG_COG_PT = 2312L, MIG_DCOG_PT = 2352L, MIG_ING_PT = 2385L, 
              MIG_NDCOG_PT = 2419L, MIG_NRG_X_E_PT = 2464L)

data <- readRDS("comparison_filters/eurostat_db.RDS")
length(data)
# data2 <- sapply(data,function(x) any(is.na(x)))
tmp = data[-which(names(data)%in%names(i_remove))]
length(tmp)

saveRDS(tmp,"comparison_filters/eurostat_db.RDS")



liste_f <- c("comparison_filters/data_x11//old/sa_full_p1_200.RDS",
             "comparison_filters/data_x11//old/sa_full_p201_400.RDS", 
             "comparison_filters/data_x11//old/sa_full_p401_600.RDS",
             "comparison_filters/data_x11//old/sa_full_p601_800.RDS", 
             "comparison_filters/data_x11//old/sa_full_p801_1000.RDS", 
             "comparison_filters/data_x11//old/sa_full_p1001_1200.RDS", 
             "comparison_filters/data_x11//old/sa_full_p1201_1400.RDS", 
             "comparison_filters/data_x11//old/sa_full_p1401_1600.RDS", 
             "comparison_filters/data_x11//old/sa_full_p1601_1800.RDS",
             "comparison_filters/data_x11//old/sa_full_p1801_2000.RDS", 
             "comparison_filters/data_x11//old/sa_full_p2001_2200.RDS",
             "comparison_filters/data_x11//old/sa_full_p2201_2400.RDS",
             "comparison_filters/data_x11//old/sa_full_p2401_2470.RDS")

i <- 0
res_complete <- lapply(liste_f, function(nom_f){
  i <<- i+1
  f <- readRDS(nom_f)
  print(i)
  f <- f[-which(names(f)%in%names(i_remove))]
  saveRDS(f,
          sprintf("comparison_filters/data_x11/sa_full_part%02.f.RDS",i))
  NULL
})


data2 <- readRDS("comparison_filters/sa_full.RDS")
length(data2)
data2 <- data2[-which(names(data2)%in%names(i_remove))]
length(data2)
saveRDS(data2, "comparison_filters/sa_full.RDS")

data <- readRDS("comparison_filters/data_x11/full_train_data.RDS")
length(data)
data <- data[-which(names(data)%in%names(i_remove))]
length(data)
saveRDS(data, "comparison_filters/data_x11/full_train_data.RDS")


data <- readRDS("comparison_filters/data_x11/full_trend_data.RDS")
length(data)
data <- data[-which(names(data)%in%names(i_remove))]
length(data)
saveRDS(data, "comparison_filters/data_x11/full_trend_data.RDS")


