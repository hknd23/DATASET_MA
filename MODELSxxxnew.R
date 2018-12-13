library(foreign)
library(haven)
library(RCurl)
library(plm)
library(interplot)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(jtools)
library(lattice)
library(stargazer)
library(pcse)
library(astsa)
library(nlme)
library(mice)
library(Amelia)
library(forecast)
#out= UNCTADout/1000
DevDem<-read_dta(file = "https://github.com/hknd23/DATASET_MA/blob/master/PolityDem_FDIOutflow_14Oct2018.dta?raw=true")
#create lag data
DevDem$Country_Name_x<-to_character(DevDem$Country_Name_x)
DevDem$Country_Name_x<-as.factor(DevDem$Country_Name_x)
DevDem<-pdata.frame(DevDem)
DevDem$lag_outflows<-lag(DevDem$FDI_outflows)
DevDem$FDI_inflows<-DevDem$UNCTAD_FDI_in/1000
DevDem$dummy_persr<-as.factor(ifelse(DevDem$jw_persr>6,1,0))
vars<-c("PX_REX_REER","ln_GDP_PerCapita","jw_persr*TX_VAL_OTHR_ZS_WT")
vars.2<-c("lag_outflows", "PX_REX_REER","ln_GDP_PerCapita","jw_persr*TX_VAL_OTHR_ZS_WT")
vars.3<-c("lag_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*TX_VAL_OTHR_ZS_WT")
vars.4<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*TX_VAL_OTHR_ZS_WT")



vars.alt<-c("PX_REX_REER","ln_GDP_PerCapita","jw_persr*service_conc")
vars.2.alt<-c("lag_outflows", "PX_REX_REER","ln_GDP_PerCapita","jw_persr*service_conc")
vars.3.alt<-c("lag_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*service_conc")
vars.4.alt<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*service_conc")

vars.alt1<-c("PX_REX_REER","ln_GDP_PerCapita","dummy_persr*service_conc")
vars.2.alt1<-c("lag_outflows", "PX_REX_REER","ln_GDP_PerCapita","dummy_persr*service_conc")
vars.3.alt1<-c("lag_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","dummy_persr*service_conc")
vars.4.alt1<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","dummy_persr*service_conc")

MODELs<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars,collapse="+"))))
MODELs2<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.2,collapse="+"))))
MODELs3<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.3,collapse="+"))))
MODELs4<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.4,collapse="+"))))


MODELs.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.alt,collapse="+"))))
MODELs2.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.2.alt,collapse="+"))))
MODELs3.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.3.alt,collapse="+"))))
MODELs4.alt<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.4.alt,collapse="+"))))

MODELs.alt1<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.alt1,collapse="+"))))
MODELs2.alt1<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.2.alt1,collapse="+"))))
MODELs3.alt1<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.3.alt1,collapse="+"))))
MODELs4.alt1<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.4.alt1,collapse="+"))))

Model.1<-plm(MODELs,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.1a<-plm(MODELs,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.2<-plm(MODELs2,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.2a<-plm(MODELs2,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.3<-plm(MODELs3,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.3a<-plm(MODELs3,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.4<-plm(MODELs4,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.4a<-plm(MODELs4,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

###########
#Alt Model
Model.1.alt<-plm(MODELs.alt,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.1a.alt<-plm(MODELs.alt,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.2.alt<-plm(MODELs2.alt,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.2a.alt<-plm(MODELs2.alt,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.3.alt<-plm(MODELs3.alt,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.3a.alt<-plm(MODELs3.alt,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.4.alt<-plm(MODELs4.alt,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.4a.alt<-plm(MODELs4.alt,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")






#Alt Model
Model.1.alt1<-plm(MODELs.alt1,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.1a.alt1<-plm(MODELs.alt1,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.2.alt1<-plm(MODELs2.alt1,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.2a.alt1<-plm(MODELs2.alt1,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.3.alt1<-plm(MODELs3.alt1,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.3a.alt1<-plm(MODELs3.alt1,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.4.alt1<-plm(MODELs4.alt1,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")

Model.4a.alt1<-plm(MODELs4.alt1,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")






############
#checking
t1<-DevDem[DevDem$service_conc<=0.05,][-which(is.na(DevDem$service_conc)),]
t2<-DevDem[DevDem$service_conc>0.05 & DevDem$service_conc<=0.08,][-which(is.na(DevDem$service_conc)),]
t3<-DevDem[DevDem$service_conc>0.08 & DevDem$service_conc<=0.1,][-which(is.na(DevDem$service_conc)),]
t4<-DevDem[DevDem$service_conc>0.1 & DevDem$service_conc<=0.15,][-which(is.na(DevDem$service_conc)),]
t5<-DevDem[DevDem$service_conc>0.15,][-which(is.na(DevDem$service_conc)),]
t1<-t1[-which(is.na(t1$service_conc)),]
t2<-t2[-which(is.na(t2$service_conc)),]
t3<-t3[-which(is.na(t3$service_conc)),]
t4<-t4[-which(is.na(t4$service_conc)),]
t5<-t5[-which(is.na(t5$service_conc)),]

#######



# ------------------------------------
# Create Marginal Effect Plot Function
na.rm = # ------------------------------------
meplot <- function(model,var1,var2,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=3,me_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = ylab,
       main = main)
  lines(z0, dy.dx, lwd = me_lwd, lty = me_lty, col = me_col)
  lines(z0, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  lines(z0, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)
}


###NEW
DevDem$Skilled.ZS<-(DevDem$TX_VAL_INSF_ZS_WT/100)^2+(DevDem$TX_VAL_OTHR_ZS_WT/100)^2
DevDem$Skilled.ZS2<-(DevDem$TX_VAL_INSF_ZS_WT/100+DevDem$TX_VAL_OTHR_ZS_WT/100)^2

DevDem$TX_VAL_INSF_ZS_WT2<-DevDem$TX_VAL_INSF_ZS_WT^2
DevDem$TX_VAL_OTHR_ZS_WT2<-DevDem$TX_VAL_OTHR_ZS_WT^2

DevDem$skilled<-rowSums(DevDem[,c("TX_VAL_INSF_ZS_WT2","TX_VAL_OTHR_ZS_WT2")],na.rm=TRUE)
summary(DevDem$skilled)






######
vars.5.alt1<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")
vars.5.alt<-c("lag_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")

MODELs5.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.5.alt,collapse="+"))))
MODELs5.altx<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.5.alt1,collapse="+"))))

Model.5<-plm(MODELs5.alt,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")
Model.5a<-plm(MODELs5.alt,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Model.5x<-plm(MODELs5.altx,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")
Model.5ax<-plm(MODELs5.altx,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

vars.5.alt1x<-c( "FDI_inflows","PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")
vars.5.altx<-c( "FDI_inflows","PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")

MODELs5.alt2<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.5.alt1x,collapse="+"))))
MODELs5.altx2<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.5.altx,collapse="+"))))

Model.5fd<-plm(MODELs5.alt2,index = "CCodeA3", data = DevDem,model = "fd",effect = "individual")
Model.5fdx<-plm(MODELs5.altx2,index = "CCodeA3", data = DevDem,model = "fd",effect = "individual")

#######
#GMM 
#Model.5pgm<-pgmm(MODELs5.alt, data = DevDem,effect = "individual")
#Model.5apgm<-pgmm(MODELs5.alt,data = DevDem,effect = "individual")

#Model.5xpgm<-pgmm(MODELs5.altx, data = DevDem,effect = "individual")
#Model.5axpgm<-pgmm(MODELs5.altx,data = DevDem,effect = "individual")
#GLS AR(1)
lmmodelag<-lm(MODELs5.alt,data = DevDem)



#out put random
stargazer(Model.1.alt,Model.2.alt,Model.3.alt,Model.5,star.cutoffs = c(0.05,0.01,0.001),out = "random.html")
stargazer(Model.1a.alt,Model.2a.alt,Model.3a.alt,Model.5a,star.cutoffs = c(0.05,0.01,0.001),out= "fixed.html")

# Better Marginal plots
png("Marginal_model5a_jw.png",600,600)
par(mfrow=c(1,1))
meplot(Model.5a,var1 = "jw_persr",var2 = "service_conc",xlab = "Sevice Sector Exports Concentration", ylab = "Marginal Effect of Electoral Particularism",main = "Marginal Effect Plot of Electoral Particularism")
par(mfrow=c(1,1))
dev.off()
##
png("Marginal_model5a_sc.png",600,600)
par(mfrow=c(1,1))
meplot(Model.5a,var1 = "service_conc",var2 = "jw_persr",xlab = "Electoral Particularism", ylab = "Marginal Effect of Service Sector Export Concentration",main = "Marginal Effect Plot of Service Sector Export Concentration")
par(mfrow=c(1,1))
dev.off()



meplot(Model.4.alt,var1 = "jw_persr",var2 = "service_conc")
meplot(Model.4.alt,var1 = "service_conc",var2 = "jw_persr")

meplot(Model.4a.alt,var1 = "jw_persr",var2 = "service_conc")
meplot(Model.4a.alt,var1 = "service_conc",var2 = "jw_persr")


#model tests
purtest(Model.1.alt$model)
purtest(Model.3.alt$model)

purtest(Model.5$model)

pdwtest(Model.1.alt)
pdwtest(Model.2.alt)
pdwtest(Model.3.alt)
pdwtest(Model.5)


pdwtest(Model.1a.alt)
pdwtest(Model.2a.alt)
pdwtest(Model.3a.alt)
pdwtest(Model.5a)

purtest(Model.1.alt$model,exo="trend",test=c("levinlin"))
purtest(Model.1.alt$model,exo="trend",test=c("hadri"))
purtest(Model.1.alt$model,exo="trend",test=c("ips"))

purtest(Model.3.alt$model,exo="trend",test=c("levinlin"))
purtest(Model.3.alt$model,exo="trend",test=c("hadri"))
purtest(Model.3.alt$model,exo="trend",test=c("ips"))

purtest(Model.3.alt$model,exo="trend",test=c("levinlin"))
purtest(Model.3.alt$model,exo="trend",test=c("hadri"))
purtest(Model.3.alt$model,exo="trend",test=c("ips"))


purtest(Model.5$model,exo="trend",test=c("levinlin"))
purtest(Model.5$model,exo="trend",test=c("hadri"))
purtest(Model.5$model,exo="trend",test=c("ips"))

Pann1<-DevDem[,c("year","CCodeA3","ccode","lag_outflows","FDI_outflows","PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr","service_conc")]
Pann2<-DevDem[,c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr","service_conc")]

phtest(Model.1a.alt,Model.1.alt)
phtest(Model.2a.alt,Model.2.alt)
phtest(Model.3a.alt,Model.3.alt)
phtest(Model.5a,Model.5)

pbgtest(Model.1.alt)
pbgtest(Model.2.alt)
pbgtest(Model.3.alt)
pbgtest(Model.5)

pbgtest(Model.1a.alt)



pdwtest(Model.1.alt)
pdwtest(Model.2.alt)
pdwtest(Model.3.alt)
pdwtest(Model.5)

pdwtest(Model.1a.alt)
pdwtest(Model.2a.alt)
pdwtest(Model.3a.alt)
pdwtest(Model.5a)

pwartest(Model.1a.alt)
pwartest(Model.2a.alt)
pwartest(Model.3a.alt)
pwartest(Model.5a)

library(lmtest)
bptest(MODELs.alt,data = DevDem)
bptest(MODELs2.alt,data = DevDem)
bptest(MODELs3.alt,data = DevDem)
bptest(MODELs5.alt,data = DevDem)

coeftest(Model.1a.alt,vcovHC(Model.1a.alt,method = "arellano"))
coeftest(Model.2a.alt,vcovHC(Model.2a.alt,method = "arellano"))
coeftest(Model.3a.alt,vcovHC(Model.3a.alt,method = "arellano"))
coeftest(Model.5a,vcovHC(Model.5a,method = "arellano"))
######
vars.1x.alt<-c("PX_REX_REER","ln_GDP_PerCapita","jw_persr*service_conc")
vars.3x.alt<-c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")
vars.5x.alt<-c("lag_outflows","PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")

MODELs1x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.1x.alt,collapse="+"))))
MODELs3x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.3x.alt,collapse="+"))))

MODELs5x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.5x.alt,collapse="+"))))
MODELs5x.altx<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.5.alt1,collapse="+"))))

Model.1lm<-lm(MODELs1x.alt, data = DevDem)
Model.2lm<-lm(MODELs3x.alt, data = DevDem)
Model.5lm<-lm(MODELs5x.alt, data = DevDem)

Model.5alm<-lm(MODELs5x.altx, data = DevDem)

aj.lm<-sarima (DevDem$FDI_outflows, 1,0,0, xreg=DevDem[,c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr","service_conc","persr_service_conc")])
aj.lm3<-sarima (DevDem$FDI_outflows, 1,0,0, xreg=DevDem[,c("PX_REX_REER","ln_GDP_PerCapita","jw_persr","service_conc","persr_service_conc")])
aj.lm5<-sarima (DevDem$FDI_outflows, 1,0,0, xreg=DevDem[,c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr","service_conc","persr_service_conc")])
stargazer(aj.lm$ttable,aj.lm3$ttable,aj.lm5$ttable,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out= "ARIMA.html")

######
# GLS with homoscedasticity & AR(1) autocorrelation:

a1a.GLS <- gls(MODELs1x.alt,data=DevDem, correlation=corAR1(form=~1|ccode),na.action=na.omit)
a3a.GLS <- gls(MODELs3x.alt,data=DevDem, correlation=corAR1(form=~1|ccode),na.action=na.omit)
a5a.GLS <- gls(MODELs5x.alt,data=DevDem, correlation=corAR1(form=~1|ccode),na.action=na.omit)
stargazer(a1a.GLS,a3a.GLS,a5a.GLS,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out= "GLSAR1.html")

######
#Descriptives
histogram(DevDem$jw_persr)
length(which(DevDem$jw_persr==1))
length(which(DevDem$jw_persr==10))
length(which(is.na(DevDem$jw_persr)))

length(which(DevDem$service_conc<0.00))


DevDem.imp<-mice(Pann1,maxit = 50, method = 'pmm', seed = 7814 )
DevDem.imputed<-complete(DevDem.imp,1)

#imputed models
Model.1.imp<-plm(MODELs.alt,index = "CCodeA3", data = DevDem.imputed,model = "random",effect = "individual")

Model.1a.imp<-plm(MODELs.alt,index = "CCodeA3", data = DevDem.imputed,model = "within",effect = "individual")

Model.2.imp<-plm(MODELs2.alt,index = "CCodeA3", data = DevDem.imputed,model = "random",effect = "individual")

Model.2a.imp<-plm(MODELs2.alt,index = "CCodeA3", data = DevDem.imputed,model = "within",effect = "individual")

Model.3.imp<-plm(MODELs3.alt,index = "CCodeA3", data = DevDem.imputed,model = "random",effect = "individual")

Model.3a.imp<-plm(MODELs3.alt,index = "CCodeA3", data = DevDem.imputed,model = "within",effect = "individual")


Model.5.imp<-plm(MODELs5.alt,index = "CCodeA3", data = DevDem.imputed,model = "random",effect = "individual")
Model.5a.imp<-plm(MODELs5.alt,index = "CCodeA3", data = DevDem.imputed,model = "within",effect = "individual")
####### 
#imputed ouput
stargazer(Model.1.imp,Model.2.imp,Model.3.imp,Model.5.imp,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out = "randomimp.html")
stargazer(Model.1a.imp,Model.2a.imp,Model.3a.imp,Model.5a.imp,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out = "fixedimp.html")


meplot(Model.5a.imp,var1 = "jw_persr",var2 = "service_conc")
meplot(Model.5a.imp,var1 = "service_conc",var2 = "jw_persr")


# Better Marginal plots
png("Marginal_model5imp_jw.png",600,600)
par(mfrow=c(1,1))
meplot(Model.5a,var1 = "jw_persr",var2 = "service_conc")
par(mfrow=c(1,1))
dev.off()
##
png("Marginal_model5imp_sc.png",600,600)
par(mfrow=c(1,1))
meplot(Model.5a,var1 = "service_conc",var2 = "jw_persr")
par(mfrow=c(1,1))
dev.off()

########
#Amelia Imputation
Amelia_Devdem<-amelia(Pann1,idvars = c("year","CCodeA3","ccode"),m=5, parallel = "multicore")

######
#amelia models
Model.1.amimp<-plm(MODELs.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "random",effect = "individual")

Model.1a.amimp<-plm(MODELs.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "within",effect = "individual")

Model.2.amimp<-plm(MODELs2.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "random",effect = "individual")

Model.2a.amimp<-plm(MODELs2.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "within",effect = "individual")

Model.3.amimp<-plm(MODELs3.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "random",effect = "individual")

Model.3a.amimp<-plm(MODELs3.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "within",effect = "individual")


Model.5.amimp<-plm(MODELs5.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "random",effect = "individual")
Model.5a.amimp<-plm(MODELs5.alt,index = "CCodeA3", data = Amelia_Devdem$imputations$imp2,model = "within",effect = "individual")

stargazer(Model.1.amimp,Model.2.amimp,Model.3.amimp,Model.5.amimp,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out = "randomimpam.html")
stargazer(Model.1a.amimp,Model.2a.amimp,Model.3a.amimp,Model.5a.amimp,single.row = TRUE,star.cutoffs = c(0.05,0.01,0.001),out = "fixedimpam.html")

###Differencing
DevDem2<-DevDem
Panna<-DevDem2[,c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr","service_conc")]
Pann2<-DevDem2[,c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr","service_conc")]
DevDem2$FDI_outflows<-diff(DevDem2$FDI_outflows)
plot.ts(DevDem$FDI_outflows)
plot.ts(DevDem2$FDI_outflows)

vars.1x.alt<-c("PX_REX_REER","ln_GDP_PerCapita","jw_persr*service_conc")
vars.3x.alt<-c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*service_conc")
vars.5x.alt<-c("PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")

MODELs1x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.1x.alt,collapse="+"))))
MODELs3x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.3x.alt,collapse="+"))))
MODELs5x.alt<-as.formula(paste(paste("FDI_outflows ~ ", paste(vars.5x.alt,collapse="+"))))

Model.1d.alt<-plm(MODELs1x.alt,index = "CCodeA3", data = DevDem,model = "fd",effect = "individual")
Model.3d.alt<-plm(MODELs3x.alt,index = "CCodeA3", data = DevDem,model = "fd",effect = "individual")
Model.5d<-plm(MODELs5x.alt,index = "CCodeA3", data = DevDem,model = "fd",effect = "individual")

stargazer(Model.1d.alt,Model.3d.alt,Model.5d,star.cutoffs = c(0.05,0.01,0.001),out= "fixeddiff3.html")

#######
#scaled models
DevDem$lag_scaled_outflows<-lag(DevDem$ScaledFDI_Out_GDP)
vars.a.alt<-c("lag_scaled_outflows", "PX_REX_REER", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*service_conc")
vars.b.alt<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","jw_persr*service_conc")
vars.c.alt<-c("lag_scaled_outflows", "PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr*service_conc")

MODEL1<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.a.alt,collapse="+"))))
MODEL3<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.b.alt,collapse="+"))))
MODEL5<-as.formula(paste(paste("ScaledFDI_Out_GDP ~ ", paste(vars.c.alt,collapse="+"))))



Modela.1.alt<-plm(MODEL1,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")
Modela.1a.alt<-plm(MODEL1,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Modela.3.alt<-plm(MODEL3,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")
Modela.3a.alt<-plm(MODEL3,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

Modela.5.alt<-plm(MODEL5,index = "CCodeA3", data = DevDem,model = "random",effect = "individual")
Modela.5a.alt<-plm(MODEL5,index = "CCodeA3", data = DevDem,model = "within",effect = "individual")

stargazer(Modela.1.alt,Modela.3.alt,Modela.5.alt,star.cutoffs = c(0.05,0.01,0.001),out= "newrandom.html")
stargazer(Modela.1a.alt,Modela.3a.alt,Modela.5a.alt,single.row = FALSE,star.cutoffs = c(0.05,0.01,0.001),out= "newfixed.html")

counts<-table(DevDem$jw_persr)
barplot(counts,main = "Distribution of Electoral Particularism Index")

png("Elec.png",600,600)
par(mfrow=c(1,1))
barplot(counts,main = "Distribution of Electoral Particularism Index")
par(mfrow=c(1,1))
dev.off()


Des<-DevDem[,c("FDI_outflows","FDI_outflGDP","PX_REX_REER","wdi_oilrent", "trade_in_GDP","ln_GDP_PerCapita","ti_cpi","jw_persr","service_conc")]
stargazer(Des)
stargazer(Model.2lm,Model.5lm,Model.5,Model.5a,Model.5d)