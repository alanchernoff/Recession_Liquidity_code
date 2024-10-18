library(dplyr)
library(car)
library(tidyr)
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


#segment data
start=which(df$Date=="2005-04-01")
stop=which(df$Date=="2007-03-30")
dfpre=df[start:stop,]
dfpre <- dfpre[, -c(1)]
start=which(df$Date=="2007-04-02")
stop=which(df$Date=="2009-03-31")
dfdur=df[start:stop,]
dfdur <- dfdur[, -c(1)]
start=which(df$Date=="2009-04-01")
stop=which(df$Date=="2011-03-31")
dfpost=df[start:stop,]
dfpost <- dfpost[, -c(1)]

#Create data summary tables
dfmean <- data.frame(matrix(ncol = 3, nrow = ncol(dfpre)))
dfstd <- data.frame(matrix(ncol = 3, nrow = ncol(dfpre)))

rownames(dfmean) <- colnames(dfpre)
colnames(dfmean) <- c("2005-2007","2007-2009","2009-2011")
rownames(dfstd) <- colnames(dfpre)
colnames(dfstd) <- c("2005-2007","2007-2009","2009-2011")

#Populate the Empty Tables
for (i in 1:nrow(dfmean)){
    dfmean[i,1] <- mean(dfpre[,i])
    dfmean[i,2] <- mean(dfdur[,i])
    dfmean[i,3] <- mean(dfpost[,i])
    dfstd[i,1] <- sd(dfpre[,i])
    dfstd[i,2] <- sd(dfdur[,i])
    dfstd[i,3] <- sd(dfpost[,i])
}

View(dfmean)
View(dfstd)

View(df)

df <- df[,-c(1)]

View(df)



