library(ggplot2)
library(tidyverse)
library(stargazer)
#Importing data set, this should be the same as the other R code

library(readxl)
Emerging_Markets <- read_csv("C:/Users/nguye/Google Drive/GRAD HELL/FALL 18/dataset/FDI Data Emerge.csv")

#Emerging_Markets<-Emerging_Marketsx[-which(is.na(Emerging_Marketsx$UNCTAD.FDI.outstock & Emerging_Marketsx$UNCTAD.FDI.out)),]

Emerging_Markets$Country_Name<-as.factor(Emerging_Markets$`Country Name.x`)
Emerging_Markets$CHN<-rep(0,nrow(Emerging_Markets))
for (i in 1:nrow(Emerging_Markets)){
if (Emerging_Markets$ccode[i]==710) {Emerging_Markets$CHN[i]<-1}}
#Descriptives
summary(Emerging_Markets)
Emerging_Markets$CCodeA3<-Emerging_Markets$CCodeA3.x
Emerging_Markets$gwf_regimetype<-as.factor(Emerging_Markets$gwf_regimetype)
summary(Emerging_Markets$CCodeA3)
#Dropping Some More Countries
Emerging_Markets1<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="KIR"),]
Emerging_Markets2<-Emerging_Markets1[-which(Emerging_Markets1$CCodeA3=="PLW"),]
Emerging_Markets3<-Emerging_Markets2[-which(Emerging_Markets2$CCodeA3=="SLE"),]
Emerging_Markets4<-Emerging_Markets3[-which(Emerging_Markets3$CCodeA3=="SYC"),]
Emerging_Markets5<-Emerging_Markets4[-which(Emerging_Markets4$CCodeA3=="TZA"),]
Emerging_Markets6<-Emerging_Markets5[-which(Emerging_Markets5$CCodeA3=="BRB"),]
Emerging_Markets7<-Emerging_Markets6[-which(Emerging_Markets6$CCodeA3=="SAU"),]
Emerging_Markets8<-Emerging_Markets7[-which(Emerging_Markets7$CCodeA3=="OMN"),]
Emerging_Markets9<-Emerging_Markets8[-which(Emerging_Markets8$CCodeA3=="QAT"),]
Emerging_Markets10<-Emerging_Markets9[-which(Emerging_Markets9$CCodeA3=="KWT"),]
Emerging_Markets11<-Emerging_Markets10[-which(Emerging_Markets10$CCodeA3=="BHR"),]
Emerging_Markets12<-Emerging_Markets11[-which(Emerging_Markets11$CCodeA3=="BHS"),]
Emerging_Markets<-Emerging_Markets12[-which(Emerging_Markets12$CCodeA3=="BLZ"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="TWN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="ARE"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="FJI"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="AFG"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="ALB"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="AGO"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BLR"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="BEN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="CIV"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="GNQ"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="GHA"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="GIN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="GNB"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="GUY"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="HND"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="JAM"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="KGZ"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="LAO"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="LBR"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MKD"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MDG"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MWI"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MLI"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MRT"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MDA"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MNE"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MOZ"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="NER"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="PNG"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="RWA"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="MLI"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="SEN"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="TGO"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="TTO"),]
Emerging_Markets<-Emerging_Markets[-which(Emerging_Markets$CCodeA3=="YEM"),]



for (i in 1:nrow(Emerging_Markets)){if (is.na(Emerging_Markets$polity2[i])==TRUE) next
  if (Emerging_Markets$polity2[i] < -10) {Emerging_Markets$polity2[i]<-NA}}
Emerging_Markets$CCodeA3<-as.factor(Emerging_Markets$CCodeA3)

#copy this to data section
Emerging_Markets<-Emerging_Markets[,c("CCodeA3","year","Country Name.x","ccode","GDP", "GDPGrowth","polity2","durable",
                                                     "trade.in.GDP","UNCTAD.FDI.out","UNCTAD.FDI.outstock","UNCTAD.FDI.in","UNCTAD.FDI.in.stock",
                                                     "GDP.PerCapita","GDP.PerCapita.PPP","ExPriskShort","ExPriskLongMed","ExPriskSpecial","CommRisk","WarRiskFDI","FDIExpro","FDITransfer",
                                                      "PRS_VA","PRS_PV","PRS_GE","PRS_RQ","PRS_RL","PRS_CC",
                                                     "vae","pve","gee","rqe","rle","cce","tech.ex","TaxRate","icrg_qog","ti_cpi","gwf_regimetype","gwf_party","gwf_personal","gwf_military","gwf_monarch","CHN","v2x_polyarchy","v2x_libdem","v2x_delibdem","v2x_partipdem","v2x_egaldem"
)]
write.csv(Emerging_Markets,"C:/Users/nguye/Google Drive/GRAD HELL/FALL 18/FDIDataVars.csv")