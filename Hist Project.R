library(dplyr)
library(car)
library(tidyr)
library(vars)

setwd("/Users/acher/Downloads/")
df=read.csv("data.csv")
df$SPX <- c(NA, 100*diff(log(df$SPX)))
df$VNQ <- c(NA, 100*diff(log(df$VNQ)))
df <- df[-c(1),]

treasury = read.csv("treasuries.csv")

treasury <- treasury %>% filter_all(all_vars(. != "."))

df<- merge(df, treasury, by = "Date")
df[-1] <- lapply(df[-1], function(x) as.numeric(as.character(x)))
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
df<- df[order(df$Date), ]

dfb<-df[, -c(1)]

dft <-df
dft$datemarker <- NA


#segment data
start=which(df$Date=="2005-04-01")
stop=which(df$Date=="2007-03-30")
dft$datemarker[start:stop] <- "A"
dfpre=df[start:stop,]
dfpre <- dfpre[, -c(1)]
start=which(df$Date=="2007-04-02")
stop=which(df$Date=="2009-03-31")
dft$datemarker[start:stop] <- "B"
dfdur=df[start:stop,]
dfdur <- dfdur[, -c(1)]
start=which(df$Date=="2009-04-01")
stop=which(df$Date=="2011-03-31")
dft$datemarker[start:stop] <- "C"
dfpost=df[start:stop,]
dfpost <- dfpost[, -c(1)]

#Create data summary tables
dfmean <- data.frame(matrix(ncol = 3, nrow = ncol(dfpre)))
dfstd <- data.frame(matrix(ncol = 3, nrow = ncol(dfpre)))
dflev <- data.frame(matrix(ncol=2, nrow = ncol(dfpre)))

rownames(dfmean) <- colnames(dfpre)
colnames(dfmean) <- c("2005-2007","2007-2009","2009-2011")
rownames(dfstd) <- colnames(dfpre)
colnames(dfstd) <- c("2005-2007","2007-2009","2009-2011")
rownames(dflev) <- colnames(dfpre)
colnames(dflev) <- c("Test-Statistic","P-Value")


#Populate the Empty Tables
for (i in 1:nrow(dfmean)){
    dfmean[i,1] <- mean(dfpre[,i])
    dfmean[i,2] <- mean(dfdur[,i])
    dfmean[i,3] <- mean(dfpost[,i])
    dfstd[i,1] <- sd(dfpre[,i])
    dfstd[i,2] <- sd(dfdur[,i])
    dfstd[i,3] <- sd(dfpost[,i])
    lev <- leveneTest(dft[,(i+1)] ~ datemarker, data = dft)
    dflev[i,1] <- lev$`F value`[1]
    dflev[i,2] <- lev$`Pr(>F)`[1]
}

#View(dfmean)
#View(dfstd)
#View(dflev)

write.csv(dfmean,"dfmean.csv")
write.csv(dfstd,"dfstd.csv")
write.csv(dflev,"dflev.csv")

#Create Schwartz table
dfsic<- data.frame(matrix(ncol = 24, nrow = ))
rownames(dfsic) <- c("SIC")
colnames(dfsic) <- 1:24

var_models <- list()
for (lag in 1:24) {
  # Fit VAR model with the current lag
  var_models[[lag]] <- VAR(dfb, p = lag, type = "const") 
  dfsic[1,lag] <- BIC(var_models[[lag]])
}

write.csv(dfsic,"dfsic.csv")

#Create VAR models

modelpre <-VAR(dfpre, p = 1, type = "const") 
modeldur <-VAR(dfdur, p = 1, type = "const") 
modelpost <-VAR(dfpost, p = 1, type = "const") 
coef(modelpre)
coef(modeldur)
coef(modelpost)

#Create Correlation tables
dfRpre <- data.frame(matrix(ncol = 5, nrow = 5))
rownames(dfRpre) <- c("rho in '05-'07","rho in '07-'09","rho* in '07-'09","z-score","p-value")
colnames(dfRpre) <- colnames(dfpre)
for (i in 1:ncol(dfpre)){
  dfRpre[1,i]=cor(dfpre$VNQ,dfpre[i])
  dfRpre[2,i]=cor(dfdur$VNQ,dfdur[i])
  d=var(dfdur[i])/var(dfpre[i])-1
  dfRpre[3,i]=dfRpre[2,i]/sqrt(1+d*(1-dfRpre[2,i]^2))
  z1 <- 0.5 * log((1 + dfRpre[1,i]) / (1 - dfRpre[1,i]))
  z2 <- 0.5 * log((1 + dfRpre[3,i]) / (1 - dfRpre[3,i]))
  n1 <- nrow(dfpre)
  n2 <- nrow(dfdur) 
  se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
  z_diff <- (z1 - z2) / se_diff
  p_value <- 2 * (1 - pnorm(abs(z_diff)))
  dfRpre[4,i] <- z_diff
  dfRpre[5,i] <- p_value
}

dfRpost <- data.frame(matrix(ncol = 5, nrow = 5))
rownames(dfRpost) <- c("rho in '09-'11","rho in '07-'09","rho* in '07-'09","z-score","p-value")
colnames(dfRpost) <- colnames(dfpre)
for (i in 1:ncol(dfpre)){
  dfRpost[1,i]=cor(dfpost$VNQ,dfpost[i])
  dfRpost[2,i]=cor(dfdur$VNQ,dfdur[i])
  d=var(dfdur[i])/var(dfpre[i])-1
  dfRpost[3,i]=dfRpost[2,i]/sqrt(1+d*(1-dfRpost[2,i]^2))
  z1 <- 0.5 * log((1 + dfRpost[1,i]) / (1 - dfRpost[1,i]))
  z2 <- 0.5 * log((1 + dfRpost[3,i]) / (1 - dfRpost[3,i]))
  n1 <- nrow(dfpre)
  n2 <- nrow(dfdur) 
  se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
  z_diff <- (z1 - z2) / se_diff
  p_value <- 2 * (1 - pnorm(abs(z_diff)))
  dfRpost[4,i] <- z_diff
  dfRpost[5,i] <- p_value
}

dfSpre <- data.frame(matrix(ncol = 5, nrow = 5))
rownames(dfSpre) <- c("rho in '05-'07","rho in '07-'09","rho* in '07-'09","z-score","p-value")
colnames(dfSpre) <- colnames(dfpre)
for (i in 1:ncol(dfpre)){
  dfSpre[1,i]=cor(dfpre$SPX,dfpre[i])
  dfSpre[2,i]=cor(dfdur$SPX,dfdur[i])
  d=var(dfdur[i])/var(dfpre[i])-1
  dfSpre[3,i]=dfSpre[2,i]/sqrt(1+d*(1-dfSpre[2,i]^2))
  z1 <- 0.5 * log((1 + dfSpre[1,i]) / (1 - dfSpre[1,i]))
  z2 <- 0.5 * log((1 + dfSpre[3,i]) / (1 - dfSpre[3,i]))
  n1 <- nrow(dfpre)
  n2 <- nrow(dfdur) 
  se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
  z_diff <- (z1 - z2) / se_diff
  p_value <- 2 * (1 - pnorm(abs(z_diff)))
  dfSpre[4,i] <- z_diff
  dfSpre[5,i] <- p_value
}

dfSpost <- data.frame(matrix(ncol = 5, nrow = 5))
rownames(dfSpost) <- c("rho in '09-'11","rho in '07-'09","rho* in '07-'09","z-score","p-value")
colnames(dfSpost) <- colnames(dfpre)
for (i in 1:ncol(dfpre)){
  dfSpost[1,i]=cor(dfpost$SPX,dfpost[i])
  dfSpost[2,i]=cor(dfdur$SPX,dfdur[i])
  d=var(dfdur[i])/var(dfpre[i])-1
  dfSpost[3,i]=dfSpost[2,i]/sqrt(1+d*(1-dfSpost[2,i]^2))
  z1 <- 0.5 * log((1 + dfSpost[1,i]) / (1 - dfSpost[1,i]))
  z2 <- 0.5 * log((1 + dfSpost[3,i]) / (1 - dfSpost[3,i]))
  n1 <- nrow(dfpre)
  n2 <- nrow(dfdur) 
  se_diff <- sqrt((1 / (n1 - 3)) + (1 / (n2 - 3)))
  z_diff <- (z1 - z2) / se_diff
  p_value <- 2 * (1 - pnorm(abs(z_diff)))
  dfSpost[4,i] <- z_diff
  dfSpost[5,i] <- p_value
}

write.csv(dfRpre,"dfRpre.csv")
write.csv(dfRpost,"dfRpost.csv")
write.csv(dfSpre,"dfSpre.csv")
write.csv(dfSpost,"dfSpost.csv")
