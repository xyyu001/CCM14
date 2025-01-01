library(dplyr)
library(readxl)
library(mgcv)
library(gratia)
library(dlnm)
library(grDevices)
library(dplyr)
library(readxl)
################Please replace C:/.../....../ with your own path
#################################
data_Guangdong_ <- read_excel("C:/.../....../original data/data_Guangdong.xlsx")
means <- data_Guangdong_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)
#attach(data)
V1=data_Guangdong_$Guangdong_cases
V2=data_Guangdong_$Guangdong_Td
V3=data_Guangdong_$Guangdong_GDP
V4=data_Guangdong_$Guangdong_Tn
V5=data_Guangdong_$Guangdong_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))
dev.new()
plot(b, select = 1,  shade = TRUE, residuals = TRUE)

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=3.1762962962963:26.0448387096774,lag=c(0,6),cen=17.9)
col_transparent1 <- adjustcolor("#EBCCE2", alpha.f = 0.7) 
col_transparent2 <- adjustcolor("#CFEADF", alpha.f = 0.7)  



jpeg("C:/.../....../...//Guangdong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "a", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_T_Guangdong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "a", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()





ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.647903225807:767.337289535799,lag=c(0,6),cen=760)
jpeg("C:/.../....../...//PGuangdong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "a", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_P_Guangdong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "a", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()








data_Yunnan_ <- read_excel("C:/.../....../original data/data_Yunnan.xlsx")
means <- data_Yunnan_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Yunnan_$Yunnan_cases
V2=data_Yunnan_$Yunnan_Td
V3=data_Yunnan_$Yunnan_GDP
V4=data_Yunnan_$Yunnan_Tn
V5=data_Yunnan_$Yunnan_P
ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=-4.30365353340353:16.3866820276498,lag=c(0,6),cen=7.50)



jpeg("C:/.../....../...//Yunnan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "b", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_T_Yunnan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "b", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()





ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=590.896583333334:597.738413688788,lag=c(0,6),cen=594)


jpeg("C:/.../....../...//PYunnan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "b", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,     tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_P_Yunnan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "b", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

















data_Fujian_ <- read_excel("C:/.../....../original data/data_Fujian.xlsx")
means <- data_Fujian_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Fujian_$Fujian_cases
V2=data_Fujian_$Fujian_Tn
V3=data_Fujian_$Fujian_GDP
V4=data_Fujian_$Fujian_Tn
V5=data_Fujian_$Fujian_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=5.693125:26.0417886178862,lag=c(0,6),cen=17.4)

jpeg("C:/.../....../...//Fujian_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "c", cex.main = 2.5 , xlab = "Tn",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()

jpeg("C:/.../....../...//GDP_T_Fujian_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "c", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=752.174412955466:769.173009259259,lag=c(0,6),cen=761)


jpeg("C:/.../....../...//PFujian_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "c", cex.main = 2.5 , xlab = "P",  zlab = "RR", col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_P_Fujian_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "c", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()






data_Zhejiang_ <- read_excel("C:/.../....../original data/data_Zhejiang.xlsx")
means <- data_Zhejiang_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Zhejiang_$Zhejiang_cases
V2=data_Zhejiang_$Zhejiang_Tn
V3=data_Zhejiang_$Zhejiang_GDP
V4=data_Zhejiang_$Zhejiang_Tn
V5=data_Zhejiang_$Zhejiang_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=0.720439814814814:26.5839112903226,lag=c(0,6),cen=15.1)


jpeg("C:/.../....../...//Zhejiang_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "d", cex.main = 2.5 , xlab = "Tn",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()


jpeg("C:/.../....../...//GDP_T_Zhejiang_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "d", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()


ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=752.558822972443:772.062592592592,lag=c(0,6),cen=762)


jpeg("C:/.../....../...//PZhejiang_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "d", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Zhejiang_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "d", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()








data_Guangxi_ <- read_excel("C:/.../....../original data/data_Guangxi.xlsx")
means <- data_Guangxi_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Guangxi_$Guangxi_cases
V2=data_Guangxi_$Guangxi_Td
V3=data_Guangxi_$Guangxi_GDP
V4=data_Guangxi_$Guangxi_Tn
V5=data_Guangxi_$Guangxi_P
ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=2.49494558991982:25.3427282812767,lag=c(0,6),cen=17.5)


jpeg("C:/.../....../...//Guangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "e", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Guangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "e", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()




ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.047302777778:768.231532913716,lag=c(0,6),cen=759)


jpeg("C:/.../....../...//PGuangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "e", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Guangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "e", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()





data_Chongqing_ <- read_excel("C:/.../....../original data/data_Chongqing.xlsx")
means <- data_Chongqing_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Chongqing_$Chongqing_cases
V2=data_Chongqing_$Chongqing_Tx
V3=data_Chongqing_$Chongqing_GDP
V4=data_Chongqing_$Chongqing_Tn
V5=data_Chongqing_$Chongqing_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=6.85810185185185:37.9162601626016,lag=c(0,6),cen=22.7)


jpeg("C:/.../....../...//Chongqing_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "f", cex.main = 2.5 , xlab = "Tx",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Chongqing_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "f", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()


ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=748.88347107438:770.979032258065,lag=c(0,6),cen=760)

jpeg("C:/.../....../...//PChongqing_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "f", cex.main = 2.5 , xlab = "P",  zlab = "RR",   col = col_transparent2,   tck = 0.01,  cex.main = 2  )


dev.off()

jpeg("C:/.../....../...//GDP_P_Chongqing_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "f", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()







data_Jiangxi_ <- read_excel("C:/.../....../original data/data_Jiangxi.xlsx")
means <- data_Jiangxi_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Jiangxi_$Jiangxi_cases
V2=data_Jiangxi_$Jiangxi_T
V3=data_Jiangxi_$Jiangxi_GDP
V4=data_Jiangxi_$Jiangxi_Tn
V5=data_Jiangxi_$Jiangxi_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=2.86295532646048:30.8538577678688,lag=c(0,6),cen= 19.0)


jpeg("C:/.../....../...//Jiangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "g", cex.main = 2.5 , xlab = "T",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()

jpeg("C:/.../....../...//GDP_T_Jiangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "g", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.693664815066:771.887342497136,lag=c(0,6),cen=761)

jpeg("C:/.../....../...//PJiangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "g", cex.main = 2.5 , xlab = "P",  zlab = "RR", col = col_transparent2,     tck = 0.01,  cex.main = 2  )


dev.off()

jpeg("C:/.../....../...//GDP_P_Jiangxi_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "g", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()










data_Hunan_ <- read_excel("C:/.../....../original data/data_Hunan.xlsx")
means <- data_Hunan_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Hunan_$Hunan_cases
V2=data_Hunan_$Hunan_T
V3=data_Hunan_$Hunan_GDP
V4=data_Hunan_$Hunan_Tn
V5=data_Hunan_$Hunan_P
ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=2.00869804292425:30.8552277773621,lag=c(0,6),cen=17.7)


jpeg("C:/.../....../...//Hunan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "h", cex.main = 2.5 , xlab = "T",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Hunan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "h", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.40088334914:773.153581295453,lag=c(0,6),cen=762)

jpeg("C:/.../....../...//PHunan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "h", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()

jpeg("C:/.../....../...//GDP_P_Hunan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "h", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()








data_Sichuan_ <- read_excel("C:/.../....../original data/data_Sichuan.xlsx")
means <- data_Sichuan_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Sichuan_$Sichuan_cases
V2=data_Sichuan_$Sichuan_T
V3=data_Sichuan_$Sichuan_GDP
V4=data_Sichuan_$Sichuan_Tn
V5=data_Sichuan_$Sichuan_P


ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=3.26196513553366:29.2233870967742,lag=c(0,6),cen=17.3)


jpeg("C:/.../....../...//Sichuan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "i", cex.main = 2.5 , xlab = "T",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Sichuan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "i", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()


ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=749.788091739223:771.561658798846,lag=c(0,6),cen=761)

jpeg("C:/.../....../...//PSichuan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "i", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Sichuan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "i", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()






data_Henan_ <- read_excel("C:/.../....../original data/data_Henan.xlsx")
means <- data_Henan_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Henan_$Henan_cases
V2=data_Henan_$Henan_Td
V3=data_Henan_$Henan_GDP
V4=data_Henan_$Henan_Tn
V5=data_Henan_$Henan_P



ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=-15.9082544375039:23.9749786750076,lag=c(0,6),cen=7.69)


jpeg("C:/.../....../...//Henan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "j", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Henan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "j", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()


ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=750.94880432533:774.575089213928,lag=c(0,6),cen=763)

jpeg("C:/.../....../...//PHenan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "j", cex.main = 2.5 , xlab = "P",  zlab = "RR",   col = col_transparent2,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Henan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "j", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()





data_Hainan_ <- read_excel("C:/.../....../original data/data_Hainan.xlsx")

means <- data_Hainan_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)

V1=data_Hainan_$Hainan_cases
V2=data_Hainan_$Hainan_T
V3=data_Hainan_$Hainan_GDP
V4=data_Hainan_$Hainan_Tn
V5=data_Hainan_$Hainan_P


ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=14.9729938271605:29.4769444444445,lag=c(0,6),cen=24.3)


jpeg("C:/.../....../...//Hainan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "k", cex.main = 2.5 , xlab = "T",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Hainan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "k", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.539821428572:765.630669577875,lag=c(0,6),cen=759)

jpeg("C:/.../....../...//PHainan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "k", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Hainan_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "k", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()







data_Jiangsu_ <- read_excel("C:/.../....../original data/data_Jiangsu.xlsx")

means <- data_Jiangsu_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Jiangsu_$Jiangsu_cases
V2=data_Jiangsu_$Jiangsu_Td
V3=data_Jiangsu_$Jiangsu_GDP
V4=data_Jiangsu_$Jiangsu_Tn
V5=data_Jiangsu_$Jiangsu_P
ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=-12.5809259259259:25.3557258064516,lag=c(0,6),cen=9.57)


jpeg("C:/.../....../...//Jiangsu_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "l", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Jiangsu_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "l", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()


ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=752.265476382951:773.82875,lag=c(0,6),cen=763)

jpeg("C:/.../....../...//PJiangsu_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "l", cex.main = 2.5 , xlab = "P",  zlab = "RR",   col = col_transparent2,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Jiangsu_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "l", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()







data_Hubei_ <- read_excel("C:/.../....../original data/data_Hubei.xlsx")
means <- data_Hubei_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Hubei_$Hubei_cases
V2=data_Hubei_$Hubei_T
V3=data_Hubei_$Hubei_GDP
V4=data_Hubei_$Hubei_Tn
V5=data_Hubei_$Hubei_P


ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=0.530165961163744:30.1455775325709,lag=c(0,6),cen=16.8)


jpeg("C:/.../....../...//Hubei_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "m", cex.main = 2.5 , xlab = "T",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Hubei_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "m", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()

ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.140701758619:773.69677978236,lag=c(0,6),cen=762)

jpeg("C:/.../....../...//PHubei_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "m", cex.main = 2.5 , xlab = "P",  zlab = "RR",   col = col_transparent2,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Hubei_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "m", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()









data_Shandong_ <- read_excel("C:/.../....../original data/data_Shandong.xlsx")


means <- data_Shandong_ %>%
  select(-DATE) %>%
  summarise_all(mean, na.rm = TRUE)


V1=data_Shandong_$Shandong_cases
V2=data_Shandong_$Shandong_Tn
V3=data_Shandong_$Shandong_GDP
V4=data_Shandong_$Shandong_Tn
V5=data_Shandong_$Shandong_P

ns.basis2 <- crossbasis(V2,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis2+s(V3),family=poisson(link = "log"))

summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred2 <- crosspred(ns.basis2,b,at=-8.16437603710528:24.9477086076366,lag=c(0,6),cen=9.99)


jpeg("C:/.../....../...//Shandong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred2, 
     main = "n", cex.main = 2.5 , xlab = "Td",  zlab = "RR",  col = col_transparent1,   tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_T_Shandong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "n", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()
ns.basis5 <- crossbasis(V5,lag=c(0,6),argvar=list(fun="lin"),arglag=list(fun="ns"))
b <- gam(V1~ns.basis5+s(V3),family=poisson(link = "log"))
summary(b)
AIC(b)
dev.new()
gam.check(b)
ns.pred5 <- crosspred(ns.basis5,b,at=751.521185095293:774.456483120279,lag=c(0,6),cen=763)

jpeg("C:/.../....../...//PShandong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
par(mar = c(1, 1, 2, 0))  
plot(ns.pred5, 
     main = "n", cex.main = 2.5 , xlab = "P",  zlab = "RR",  col = col_transparent2,    tck = 0.01,  cex.main = 2  )


dev.off()
jpeg("C:/.../....../...//GDP_P_Shandong_plot.jpg", width = 1500, height = 1500, res = 300, bg = "transparent")
plot(b, main = "n", xlab = "GDP", ylab = "log(y)", cex.main = 2)

dev.off()




