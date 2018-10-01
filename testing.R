library(ggplot2)
library(tidyverse)
library(stargazer)
#Importing data set, this should be the same as the other R code

library(readxl)
Emerging_Markets <- read_csv("C:/Users/nguye/Google Drive/GRAD HELL/FALL 18/dataset/FDI Data Emerge.csv")

#Emerging_Markets<-Emerging_Marketsx[-which(is.na(Emerging_Marketsx$UNCTAD.FDI.outstock & Emerging_Marketsx$UNCTAD.FDI.out)),]

Emerging_Markets$Country_Name<-as.factor(Emerging_Markets$`Country Name.x`)
for (i in 1:nrow(Emerging_Markets)){
if (Emerging_Markets$ccode[i]==710) {Emerging_Markets$CHN[i]<-1} else {Emerging_Markets$CHN[i]<-0}}
#Descriptives
summary(Emerging_Markets)
Emerging_Markets$CCodeA3<-Emerging_Markets$CCodeA3.x
Emerging_Markets$gwf_regimetype<-as.factor(Emerging_Markets$gwf_regimetype)
summary(Emerging_Markets$CCodeA3)
#Dropping Some More Countries
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="KIR"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="PLW"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="SLE"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="SYC"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="TZA"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BRB"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="SAU"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="OMN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="QAT"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="KWT"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BHR"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BHS"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BLZ"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BRN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="SYC"),]


Emerging_Markets$CCodeA3<-as.factor(Emerging_Markets$CCodeA3)
                                             
matrix1<-as.matrix(Emerging_Markets[,c("UNCTAD.FDI.outstock","UNCTAD.FDI.out","UNCTAD.FDI.in.stock",
                                       "UNCTAD.FDI.in","polity2","trade.in.GDP","GDPGrowth","GDP.PerCapita","GDP.PerCapita.PPP","ExPriskShort","ExPriskLongMed","ExPriskSpecial","FDIExpro","FDITransfer")]
)
matrix2<-as.matrix(Emerging_Markets[,c("UNCTAD.FDI.outstock","UNCTAD.FDI.out","UNCTAD.FDI.in.stock",
                                       "UNCTAD.FDI.in","polity2","trade.in.GDP","GDP.PerCapita","GDP.PerCapita.PPP","PRS_VA","PRS_PV","PRS_GE","PRS_RQ","PRS_RL","PRS_CC")]
)
matrix3<-as.matrix(Emerging_Markets[,c("UNCTAD.FDI.outstock","UNCTAD.FDI.out","UNCTAD.FDI.in.stock",
                                       "UNCTAD.FDI.in","polity2","trade.in.GDP","GDP.PerCapita","GDP.PerCapita.PPP")]
)
matrix4<-as.matrix(Emerging_Markets[,c("PRS_VA","PRS_PV","PRS_GE","PRS_RQ","PRS_RL","PRS_CC","ExPriskShort","ExPriskLongMed","ExPriskSpecial","FDIExpro","FDITransfer")]
)
matrix5<-as.matrix(Emerging_Markets[,c("vae","pve","gee","rqe","rle","cce","PRS_VA","PRS_PV","PRS_GE","PRS_RQ","PRS_RL","PRS_CC")]
)
matrix6<-as.matrix(Emerging_Markets[,c("GDPGrowth","polity2",
                                       "trade.in.GDP","UNCTAD.FDI.out","UNCTAD.FDI.outstock","UNCTAD.FDI.in","UNCTAD.FDI.in.stock",
                                       "GDP.PerCapita","GDP.PerCapita.PPP","tech.ex","TaxRate","icrg_qog","ti_cpi","gwf_party","gwf_personal","gwf_military","gwf_monarch")])

Emerging_Markets.vars<-as.matrix(Emerging_Markets[,c("GDPGrowth","polity2",
                                           "trade.in.GDP","UNCTAD.FDI.out","UNCTAD.FDI.outstock","UNCTAD.FDI.in","UNCTAD.FDI.in.stock",
                                           "GDP.PerCapita","GDP.PerCapita.PPP","PRS_VA","PRS_PV","PRS_GE","PRS_RQ","PRS_RL","PRS_CC",
                                           "vae","pve","gee","rqe","rle","cce","tech.ex","TaxRate","icrg_qog","ti_cpi","gwf_party","gwf_personal","gwf_military","gwf_monarch"
                                           )])
#Density plots
plot(density(Emerging_Markets$UNCTAD.FDI.outstock,na.rm = TRUE))
plot(density(Emerging_Markets$UNCTAD.FDI.out,na.rm = TRUE))
plot(density(Emerging_Markets$UNCTAD.FDI.in.stock,na.rm = TRUE))
plot(density(Emerging_Markets$UNCTAD.FDI.in,na.rm = TRUE))
plot(density(Emerging_Markets$polity2,na.rm = TRUE))
plot(density(Emerging_Markets$trade.in.GDP,na.rm = TRUE))
plot(density(Emerging_Markets$GDP,na.rm = TRUE))
plot(density(Emerging_Markets$GDPGrowth,na.rm = TRUE))
plot(density(Emerging_Markets$GDP.PerCapita,na.rm = TRUE))
plot(density(Emerging_Markets$GDP.PerCapita.PPP,na.rm = TRUE))
#Correlations
cor1<-cor(matrix1,use = "complete.obs")
cor2<-cor(matrix2,use = "complete.obs")
cor3<-cor(matrix3,use = "complete.obs")
cor4<-cor(matrix4,use = "complete.obs")
cor5<-cor(matrix5,use = "complete.obs")
cor6<-cor(matrix6,use = "complete.obs")

#Run regressions
model1<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP+GDP.PerCapita
           +GDPGrowth+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer,data=Emerging_Markets)
model2<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP+GDP.PerCapita
           +GDPGrowth+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer,data=Emerging_Markets)

model1a<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
            +GDPGrowth+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer,data=Emerging_Markets)
model2a<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
            +GDPGrowth+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer,data=Emerging_Markets)
#baseline

model3<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
           +GDP.PerCapita+tech.ex+ti_cpi,data=Emerging_Markets)
model4<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
           +GDP.PerCapita+tech.ex+ti_cpi,data=Emerging_Markets)
model3a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+polity2+trade.in.GDP
           +GDP.PerCapita+tech.ex+ti_cpi,data=Emerging_Markets)
model4a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+polity2+trade.in.GDP
           +GDP.PerCapita+tech.ex+ti_cpi,data=Emerging_Markets)
#ONDD SERIES
model11<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
           +GDP.PerCapita+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model12<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
           +GDP.PerCapita+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model11a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+polity2+trade.in.GDP
            +GDP.PerCapita+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer
            +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model12a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+polity2+trade.in.GDP
            +GDP.PerCapita+ExPriskShort+ExPriskLongMed+ExPriskSpecial+WarRiskFDI+FDIExpro+FDITransfer
            +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

#PRS SERIES
model5<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
           +GDP.PerCapita+PRS_VA+PRS_RQ+PRS_RL+PRS_PV+PRS_GE+PRS_CC
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model6<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
           +GDP.PerCapita+PRS_VA+PRS_RQ+PRS_RL+PRS_PV+PRS_GE+PRS_CC
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model5a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+polity2+trade.in.GDP
           +GDP.PerCapita+PRS_VA+PRS_RQ+PRS_RL+PRS_PV+PRS_GE+PRS_CC
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model6a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+polity2+trade.in.GDP
           +GDP.PerCapita+PRS_VA+PRS_RQ+PRS_RL+PRS_PV+PRS_GE+PRS_CC
           +gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

#WGI Series
model7<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
           +GDP.PerCapita+vae+pve+gee+rqe+rle+cce+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model8<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
           +GDP.PerCapita+vae+pve+gee+rqe+rle+cce+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model7a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+polity2+trade.in.GDP
           +GDP.PerCapita+vae+pve+gee+rqe+rle+cce+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model8a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+polity2+trade.in.GDP
           +GDP.PerCapita+vae+pve+gee+rqe+rle+cce+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

#add icrg_qog to replace PRS series

model9<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+trade.in.GDP
           +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model10<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+trade.in.GDP
           +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
#logged
model9a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+polity2+trade.in.GDP
           +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model10a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+polity2+trade.in.GDP
            +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
#add stability,memove polity
model9b<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+polity2+durable+trade.in.GDP
           +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model10b<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+polity2+durable+trade.in.GDP
            +GDPGrowth+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

#remove Polity, keep qog

#replace GDP Per Capita with tech exports

model13<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+trade.in.GDP
            +tech.ex+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model14<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in+trade.in.GDP
            +tech.ex+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

model13a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+trade.in.GDP
            +tech.ex+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model14a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+trade.in.GDP
            +tech.ex+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

# add cpi
model15<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+trade.in.GDP+ti_cpi
            +icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model16<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+trade.in.GDP+ti_cpi
            +icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)

model15a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+trade.in.GDP+ti_cpi
            +icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
model16a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+trade.in.GDP+ti_cpi
            +icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP,data=Emerging_Markets)
# add cpi, tech exports, gwf, and a lot of stuff
model17<-lm(UNCTAD.FDI.out~UNCTAD.FDI.in+trade.in.GDP+polity2+tech.ex+ti_cpi
             +GDP.PerCapita+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP+CHN,data=Emerging_Markets)
model18<-lm(UNCTAD.FDI.outstock~UNCTAD.FDI.in.stock+trade.in.GDP+polity2+tech.ex+ti_cpi
             +GDP.PerCapita+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP+CHN,data=Emerging_Markets)

model17a<-lm(log(UNCTAD.FDI.out)~log(UNCTAD.FDI.in)+trade.in.GDP+polity2+tech.ex+ti_cpi
             +GDP.PerCapita+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP+CHN,data=Emerging_Markets)
model18a<-lm(log(UNCTAD.FDI.outstock)~log(UNCTAD.FDI.in.stock)+trade.in.GDP+polity2+tech.ex+ti_cpi
             +GDP.PerCapita+icrg_qog+gwf_party*trade.in.GDP+gwf_personal*trade.in.GDP+ gwf_military*trade.in.GDP+gwf_monarch*trade.in.GDP+CHN,data=Emerging_Markets)


#add tax, tech exports, regime vars
#Yearly Average and plots
FDI.OUT.means<-aggregate(UNCTAD.FDI.out~year,Emerging_Markets,mean)
FDI.OUTSTOCK.means<-aggregate(UNCTAD.FDI.outstock~year,Emerging_Markets,mean)
FDI.IN.means<-aggregate(UNCTAD.FDI.in~year,Emerging_Markets,mean)
FDI.INSTOCK.means<-aggregate(UNCTAD.FDI.in.stock~year,Emerging_Markets,mean)
plot(FDI.OUT.means)
plot(FDI.IN.means)
plot(FDI.OUTSTOCK.means)
plot(FDI.INSTOCK.means)

#Country AVerages and means
FDI.OUT.ccmeans<-aggregate(UNCTAD.FDI.out~CCodeA3,Emerging_Markets,mean)
FDI.OUTSTOCK.ccmeans<-aggregate(UNCTAD.FDI.outstock~CCodeA3,Emerging_Markets,mean)
FDI.IN.ccmeans<-aggregate(UNCTAD.FDI.in~CCodeA3,Emerging_Markets,mean)
FDI.INSTOCK.ccmeans<-aggregate(UNCTAD.FDI.in.stock~CCodeA3,Emerging_Markets,mean)


#Country totals
FDI.OUT.ccsum<-aggregate(UNCTAD.FDI.out~CCodeA3,Emerging_Markets,sum)
FDI.OUTSTOCK.ccsum<-aggregate(UNCTAD.FDI.outstock~CCodeA3,Emerging_Markets,sum)
FDI.IN.ccsum<-aggregate(UNCTAD.FDI.in~CCodeA3,Emerging_Markets,sum)
FDI.INSTOCK.ccsum<-aggregate(UNCTAD.FDI.in.stock~CCodeA3,Emerging_Markets,sum)
summary(Emerging_Markets.vars)

#ggplot
ggplot( data=Emerging_Markets,
       aes(x=year, y=UNCTAD.FDI.out, colour=`Country Name.x`)) +
  geom_line()

ggplot( data=Emerging_Markets,
       aes(x=year, y=UNCTAD.FDI.outstock, colour=`Country Name.x`)) +
  geom_line()

stargazer(model17,model17a,model18,model18a, type="html",out="Models1717.html",summary=TRUE, summary.stat=c("max"	,"mean","median","min","n","sd"))
stargazer(model3,model3a,model4,model4a, type="html",out="Models34.html")
stargazer(model5,model5a,model6,model6a, type="html",out="Models56.html")
stargazer(model7,model7a,model8,model8a, type="html",out="Models78.html")
stargazer(model11,model11a,model12,model12a, type="html",out="Models1112.html")


cor(Emerging_Markets.vars, use="complete.obs")