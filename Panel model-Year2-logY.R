library(tidyverse)
library(fixest)

workpath <- getwd()
setwd(workpath)

workdata_ori <- read.csv("maize_model_data.csv")
workdata_ori <- mutate(workdata_ori, year_num = as.numeric(year), year_sq = year_num^2)
workdata_model <- workdata_ori
workdata_model$yield <- log(workdata_model$yield)
#names(workdata_model)
#workdata_model$diff <- workdata_model$wl - workdata_model$hr

fixest_form <- yield ~ tavg + rain + rain2 + radn + 
  hdd + drog + dh + 
  cdd + hr + lyy + wl +
  i(cnty, year_num) | cnty
model <- feols(fixest_form, data = workdata_model, cluster = ~cnty)
save(model,file = "maize_model-logY.RData")
#model$coefficients[1:11]
data.frame(model$coefficients[1:11])

workdata_model$predict_yield <- predict(model)

# 计算NRMSE (假设yield和predict_yield是原始值，未取指数)
nrmse <- function(actual, predicted) {
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  nrmse <- 100 * rmse / mean(actual, na.rm = TRUE)
}
nrmse_value <- nrmse(exp(workdata_model$yield), exp(workdata_model$predict_yield))

ggplot(workdata_model, aes(x = exp(predict_yield), y = exp(yield))) +
  geom_point(alpha = 0.6, color = "gray") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = 10000, y = 2000,  # 调整标注位置
           label = paste("NRMSE =", round(nrmse_value, 3)),
           size = 5, color = "darkgreen") +
  scale_x_continuous(limits = c(0, 15000)) +  # 使用数学表达式
  scale_y_continuous(limits = c(0, 15000)) +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) +
  theme(
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90")
  )

rm(list = ls())

workpath <- getwd()
setwd(workpath)

load("maize_model-logY.RData")

ext_vars <- c("hdd","drog","dh","cdd","lyy","wl","hr")
ave_vars <- c("tavg","rain","radn")

workdata_ori <- read.csv("maize_model_data.csv")
workdata_ori <- mutate(workdata_ori, year_num = as.numeric(year), year_sq = year_num^2)
workdata_model <- workdata_ori
workdata_model$predict_yield <- predict(model, newdata = workdata_model)
predict_yield <- workdata_model[,c(1:2,ncol(workdata_model))]

workdata_base <- subset(workdata_ori,year %in% 1981:1990)
workdata_base <- group_by(workdata_base,cnty)

workdata_base <- summarise(workdata_base,
                           radn = quantile(radn,0.5,na.rm = T),
                           rain = quantile(rain,0.5,na.rm = T),
                           tavg = quantile(tavg,0.5,na.rm = T),
                           hdd = quantile(hdd,0.5,na.rm = T),
                           drog = quantile(drog,0.5,na.rm = T),
                           dh = quantile(dh,0.5,na.rm = T),
                           cdd = quantile(cdd,0.5,na.rm = T),
                           lyy = quantile(lyy,0.5,na.rm = T),
                           wl = quantile(wl,0.5,na.rm = T),
                           hr = quantile(hr,0.5,na.rm = T))

workdata_ave <- merge(workdata_ori[,c(which(colnames(workdata_ori) %in% c(ext_vars,"year","cnty")))],workdata_base[,c(which(colnames(workdata_base) %in% c(ave_vars,"cnty")))],by = c("cnty"))
workdata_ave$rain2 <- workdata_ave$rain^2
workdata_ave <- mutate(workdata_ave, year_num = as.numeric(year), year_sq = year_num^2)
workdata_model <- workdata_ave
workdata_model$ave_yield <- predict(model, newdata = workdata_model)
workdata_ave <- workdata_model[,c(1:2,ncol(workdata_model))]

workdata_ext <- merge(workdata_ori[,c(which(colnames(workdata_ori) %in% c(ave_vars,"year","cnty")))],workdata_base[,c(which(colnames(workdata_base) %in% c(ext_vars,"cnty")))],by = c("cnty"))
workdata_ext$rain2 <- workdata_ext$rain^2
workdata_ext <- mutate(workdata_ext, year_num = as.numeric(year), year_sq = year_num^2)
workdata_model <- workdata_ext
workdata_model$ext_yield <- predict(model, newdata = workdata_model)
workdata_ext <- workdata_model[,c(1:2,ncol(workdata_model))]

final_yield <- merge(predict_yield,workdata_ave,by = c("cnty","year"))
final_yield <- merge(final_yield,workdata_ext,by = c("cnty","year"))

for(var in c(ext_vars,ave_vars)){
  #var  = "hdd"
  print(var)
  workdata_novar <- workdata_ori[,c(which(colnames(workdata_ori) != var))]
  workdata_var <- workdata_base[,c(which(colnames(workdata_base) %in% c(var,"cnty")))]
  workdata_todo <- merge(workdata_novar,workdata_var,by = c("cnty"))
  if(var == "rain"){workdata_todo$rain2 <- workdata_todo$rain ^ 2}
  workdata_model <- workdata_todo
  workdata_model$pred_yield <- predict(model, newdata = workdata_model)
  workdata_todo <- workdata_model[,c(1,2,ncol(workdata_model))]
  names(workdata_todo)[3] <- paste(var,"_yield",sep = "")
  final_yield <- merge(final_yield,workdata_todo,by = c("cnty","year"))
}

write.csv(final_yield,"Simulated_yield_maize-logY.csv",row.names = F)

rm(list = ls())

workpath <- getwd()
setwd(workpath)

workdata_ori <- read.csv("Simulated_yield_maize-logY.csv")
#workdata <- subset(workdata_ori,year >= 1991)
workdata <- workdata_ori
names(workdata_ori)
workdata <- mutate(workdata,
                   imp_ave = predict_yield - ave_yield,
                   imp_ext = predict_yield - ext_yield,
                   
                   imp_tavg = predict_yield - tavg_yield,
                   imp_rain = predict_yield - rain_yield,
                   imp_radn = predict_yield - radn_yield,
                   
                   imp_hdd = predict_yield - hdd_yield,
                   imp_drog = predict_yield - drog_yield,
                   imp_dh = predict_yield - dh_yield,
                   imp_cdd = predict_yield - cdd_yield,
                   imp_lyy = predict_yield - lyy_yield,
                   imp_wl = predict_yield - wl_yield,
                   imp_hr = predict_yield - hr_yield
)
names(workdata)
workdata <- workdata[,c(1:2,16:27)]

workdata <- group_by(workdata,cnty)

result_year <- summarise(workdata,
                         
                         imp_ave = mean(imp_ave),
                         imp_ext = mean(imp_ext),
                         
                         imp_tavg = mean(imp_tavg),
                         imp_rain = mean(imp_rain),
                         imp_radn = mean(imp_radn),
                         
                         imp_hdd = mean(imp_hdd),
                         imp_drog = mean(imp_drog),
                         imp_dh = mean(imp_dh),
                         imp_cdd = mean(imp_cdd),
                         imp_lyy = mean(imp_lyy),
                         imp_wl = mean(imp_wl),
                         imp_hr = mean(imp_hr))

write.csv(result_year,"cnty_mean_maize-logY.csv",row.names = F)

rm(list = ls())

workpath <- getwd()
setwd(workpath)

workdata_ori <- read.csv("Simulated_yield_maize-logY.csv")
workdata <- workdata_ori
#workdata <- subset(workdata_ori,year >= 1991)

names(workdata_ori)
workdata <- mutate(workdata,
                   
                   imp_ave = predict_yield - ave_yield,
                   imp_ext = predict_yield - ext_yield,
                   
                   imp_tavg = predict_yield - tavg_yield,
                   imp_radn = predict_yield - radn_yield,
                   imp_rain = predict_yield - rain_yield,
                   
                   imp_hdd = predict_yield - hdd_yield,
                   imp_drog = predict_yield - drog_yield,
                   imp_dh = predict_yield - dh_yield,
                   imp_cdd = predict_yield - cdd_yield,
                   imp_lyy = predict_yield - lyy_yield,
                   imp_wl = predict_yield - wl_yield,
                   imp_hr = predict_yield - hr_yield
)
workdata <- workdata[,c(1:2,16:27)]

crop_area <- readxl::read_xlsx("Crop_planting_area.xlsx","maize_final")
workdata <- merge(workdata,crop_area,by = "cnty", all.x = T)
workdata$all[which(is.na(workdata$all))] <- 0
workdata$ratio <- workdata$all / (sum(workdata$all) / length(unique(workdata$year)))

workdata <- group_by(workdata,year)

result_year <- summarise(workdata,
                         #yield_obs = sum(yield * ratio),
                         imp_ave = sum(imp_ave * ratio),
                         imp_ext = sum(imp_ext * ratio),
                         
                         imp_tavg = sum(imp_tavg * ratio),
                         imp_radn = sum(imp_radn * ratio),
                         imp_rain = sum(imp_rain * ratio),
                         
                         imp_hdd = sum(imp_hdd * ratio),
                         imp_drog = sum(imp_drog * ratio),
                         imp_dh = sum(imp_dh * ratio),
                         imp_cdd = sum(imp_cdd * ratio),
                         imp_lyy = sum(imp_lyy * ratio),
                         imp_wl = sum(imp_wl * ratio),
                         imp_hr = sum(imp_hr * ratio))
write.csv(result_year,"imp_yearly_change_maize-logY.csv",row.names = F)

