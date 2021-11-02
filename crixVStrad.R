############################################################
# R-Script for Comparing CC indices to traditional indices #
############################################################

#Author: Albane Kirsch

Sys.setlocale("LC_TIME", "English")

#Load packages
library(readxl)
library(zoo)
library(xts)
library(MASS)
library(fitdistrplus)
library(RColorBrewer)
library(hrbrthemes)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(TTR)

########
# Data #
########

#Import data from Excel
crix <- read_excel("~/BA/Data/crix.xlsx", col_types = c("date", "numeric"))
tmi <- read_excel("~/BA/Data/tmi.xlsx", col_types = c("date","numeric"))
dax <- read_excel("~/BA/Data/dax.xlsx", col_types = c("date",  "numeric"))
cdax <- read_excel("~/BA/Data/cdax.xlsx", col_types = c("date", "numeric"))
sp500 <- read_excel("~/BA/Data/sp500.xlsx", col_types = c("date", "numeric"))
w5000 <- read_excel("~/BA/Data/w5000.xlsx", col_types = c("date", "numeric"))

#Define series as time series objects
crix.xts <-xts(crix$CRIX, order.by = crix$Date)
tmi.xts <-xts(tmi$TMI, order.by = tmi$Date)
dax.xts <-xts(dax$DAX, order.by = dax$Date)
cdax.xts <-xts(cdax$CDAX, order.by = cdax$Date)
sp500.xts <-xts(sp500$SP500, order.by = sp500$Date)
w5000.xts <-xts(w5000$W5000, order.by = w5000$Date)

#Drop NA's
crix <- crix.xts[!is.na(crix.xts),]
tmi <- tmi.xts[!is.na(tmi.xts),]
dax <- dax.xts[!is.na(dax.xts),]
cdax <- cdax.xts[!is.na(cdax.xts),]
sp500 <- sp500.xts[!is.na(sp500.xts),]
w5000 <- w5000.xts[!is.na(w5000.xts),]

##########
# Prices #
##########

#Index values
index_values <-merge(crix,dax,cdax,sp500,w5000)
plot.xts(index_values, main= "", ylab = "Index values", yaxis.left = FALSE, lwd=3)

#Normalised index values (Standard score)
crix.tmi.dax.cdax.sp500.w5000 = cbind(crix,tmi,dax,cdax,sp500,w5000)
index_scaled <- scale(crix.tmi.dax.cdax.sp500.w5000, center = TRUE, scale = TRUE)
plot.xts(index_scaled, main= "",ylab = "Normalised index values", yaxis.left = FALSE, lwd=3)

###########
# Returns #
###########

rcrix.xts =diff(log(crix),lag=1,differences=1)
rtmi.xts =diff(log(tmi),lag=1,differences=1)
rdax.xts =diff(log(dax),lag=1,differences=1)
rcdax.xts =diff(log(cdax),lag=1,differences=1)
rsp500.xts = diff(log(sp500),lag=1,differences=1)  
rw5000.xts = diff(log(w5000),lag=1,differences=1)

rcrix <- rcrix.xts[!is.na(rcrix.xts),]
rtmi <- rtmi.xts[!is.na(rtmi.xts),]
rdax <- rdax.xts[!is.na(rdax.xts),]
rcdax <- rcdax.xts[!is.na(rcdax.xts),]
rsp500 <- rsp500.xts[!is.na(rsp500.xts),]
rw5000 <- rw5000.xts[!is.na(rw5000.xts),]

returns <-merge(rcrix,rtmi,rdax,rcdax,rsp500,rw5000)
plot.xts(returns, main= "", multi.panel = TRUE, yaxis.left = FALSE, lwd=3)

#Descriptive statistics
summary(returns)

#Calculate growth of 1 Euro invested in the indices
#Compute simple returns
crix_ret = diff(crix)/lag(crix)
tmi_ret = diff(tmi)/lag(tmi)
dax_ret = diff(dax)/lag(dax)
cdax_ret = diff(cdax)/lag(cdax)
sp500_ret = diff(sp500)/lag(sp500)
w5000_ret = diff(w5000)/lag(w5000)

crix_ret <- crix_ret[!is.na(crix_ret),]
tmi_ret <- tmi_ret[!is.na(tmi_ret),]
dax_ret <- dax_ret[!is.na(dax_ret),]
cdax_ret <- cdax_ret[!is.na(cdax_ret),]
sp500_ret <- sp500_ret[!is.na(sp500_ret),]
w5000_ret <- w5000_ret[!is.na(w5000_ret),]

#Compute gross returns
crix_gret <- 1 + crix_ret
tmi_gret <- 1 + tmi_ret
dax_gret <- 1 + dax_ret
cdax_gret <- 1 + cdax_ret
sp500_gret <- 1 + sp500_ret
w5000_gret <- 1 + w5000_ret

#Compute future values
crix_fv <- cumprod(crix_gret)
tmi_fv <- cumprod(tmi_gret)
dax_fv <- cumprod(dax_gret)
cdax_fv <- cumprod(cdax_gret)
sp500_fv <- cumprod(sp500_gret)
w5000_fv <- cumprod(w5000_gret)

fvs <- merge(crix_fv,tmi_fv,dax_fv,cdax_fv,sp500_fv,w5000_fv)
plot.xts(fvs, main= "",ylab = "Euros", yaxis.left = FALSE, lwd=3)

summary(fvs)

#################
# Distributions #
#################

#Boxplot of the logarithm of index values
lcrix.xts =log(crix.xts)
ltmi.xts =log(tmi.xts)
ldax.xts =log(dax.xts)
lcdax.xts =log(cdax.xts)
lsp500.xts = log(sp500.xts)  
lw5000.xts = log(w5000.xts)

lcrix <- lcrix.xts[!is.na(lcrix.xts),]
ltmi <- ltmi.xts[!is.na(ltmi.xts),]
ldax <- ldax.xts[!is.na(ldax.xts),]
lcdax <- lcdax.xts[!is.na(lcdax.xts),]
lsp500 <- lsp500.xts[!is.na(lsp500.xts),]
lw5000 <- lw5000.xts[!is.na(lw5000.xts),]

lcrix.ltmi.ldax.lcdax.lsp500.lw5000 = cbind(lcrix,ltmi,ldax,lcdax,lsp500,lw5000)

boxplot(lcrix.ltmi.ldax.lcdax.lsp500.lw5000,
        main="",
        at = c(1,2,3,4,5,6),
        names = c("crix","tmi","dax", "cdax", "sp500", "w5000"),
        ylab="Logarithm of the index values",
        col = brewer.pal(n = 6, name = 'Set3')
)

#Density function of the logarithm of index values
display.brewer.all()
cols <- brewer.pal(12, "Set3")
cols

plot(density(lcrix), kernel ="epanech", xlim=c(0, 30), ylim= c(0, 6), lwd=2, main="")
lines(density(ltmi), col = "#8DD3C7", lwd=2) 
lines(density(ldax), col="#FFFFB3", lwd=2)
lines(density(lcdax), col="#BEBADA", lwd=2)
lines(density(lsp500), col="#FB8072", lwd=2)
lines(density(lw5000), col="#B3DE69", lwd=2)

#Cullen and Frey graphs
vecrix = as.vector(rcrix)
vecrix<-vecrix[!is.na(vecrix)]
descdist(vecrix, discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")
vesp500 = as.vector(rsp500)  
vesp500<-vesp500[!is.na(vesp500)]
descdist(vesp500, discrete = FALSE, boot = 1000, method = "unbiased",graph = TRUE, obs.col = "darkblue", obs.pch = 16, boot.col = "orange")

#Density function of the returns
plot(density(rcrix), kernel ="epanech", xlim=c(-0.2, 0.2), ylim= c(0, 60), lwd=2, main="")  
lines(density(rtmi), col = "#8DD3C7", lwd=2) 
lines(density(rdax), col="#FFFFB3", lwd=2)
lines(density(rcdax), col="#BEBADA", lwd=2)
lines(density(rsp500), col="#FB8072", lwd=2)
lines(density(rw5000), col="#B3DE69", lwd=2)

################
# Correlations #
################

crix.tmi.dax.cdax.sp500.w5000= cbind(crix,tmi,dax,cdax,sp500,w5000)
res1 <- rcorr(as.matrix(crix.tmi.dax.cdax.sp500.w5000), type = "spearman")
res1
corrplot(res1$r, method = 'color', type="upper", order="AOE", tl.col = 'black', col = brewer.pal(n = 10, name = 'BrBG'), 
         p.mat = res1$P, sig.level = 0.05, insig = "blank",
         addCoef.col = TRUE,  number.cex=0.8)

################
# Volatilities #
################

#Squared returns
crix.sqr <- rcrix^2
tmi.sqr <- rtmi^2
dax.sqr <- rdax^2
cdax.sqr <- rcdax^2
sp500.sqr <- rsp500^2
w5000.sqr <- rw5000^2

plot.xts(crix.sqr, main= "", ylab = "CRIX squared returns", yaxis.left = FALSE)
plot.xts(dax.sqr, main= "", ylab = "DAX squared returns", yaxis.left = FALSE)
plot.xts(sp500.sqr, main= "", ylab = "S&P 500 squared returns", yaxis.left = FALSE)

#Annualised volatilities (in percent, 30-day moving average)
crix_vol <- xts(apply(rcrix,2,runSD,n=30), index(rcrix))*sqrt(352)*100
tmi_vol <- xts(apply(rtmi,2,runSD,n=30), index(rtmi))*sqrt(352)*100
dax_vol <- xts(apply(rdax,2,runSD,n=30), index(rdax))*sqrt(252)*100
cdax_vol <- xts(apply(rcdax,2,runSD,n=30), index(rcdax))*sqrt(252)*100
sp500_vol <- xts(apply(rsp500,2,runSD,n=30), index(rsp500))*sqrt(252)*100
w5000_vol <- xts(apply(rw5000,2,runSD,n=30), index(rw5000))*sqrt(252)*100

vola = cbind(crix_vol,tmi_vol,dax_vol,cdax_vol,sp500_vol,w5000_vol)
plot.xts(vola, main= "", ylab= "Annualised volatilities", yaxis.left = FALSE)
