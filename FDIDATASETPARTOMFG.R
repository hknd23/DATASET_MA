library(tidyr)
library(stargazer)
library(readr)
library(readxl)
library(countrycode)

PRSFULL <- read_excel("C:/Users/nguye/Google Drive/GRAD HELL/FALL 18/dataset/CountryData (1).xlsx")
FDIDataVars <- read_csv("C:/Users/nguye/Google Drive/GRAD HELL/FALL 18/dataset/FDIDataVars.csv")

VARfromPRS<-levels(as.factor(PRSFULL$Variable))

PRSFULL[which(PRSFULL$Variable==VARfromPRS[i])]

