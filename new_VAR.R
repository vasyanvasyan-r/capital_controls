Sys.setenv(LANG = "en")


setwd("C:/Users/vasil/YandexDisk/D is not for Dragons/calculations")

library(AER)
library(dynlm)
library(forecast)
library(stargazer)
library(scales)
library(quantmod)
library(urca)
library(vars)
library(dLagM)
library(tseries)
library(data.table)
library(nlme)
library(data.table)
library(caroline)
library(corrplot)
library(openxlsx)
library(combinat)

weeek <- function (x) {
  ## change time format
  x <-  as.Date(x)
  ## group by week
  paste(as.character(week(x)), as.character(year(x)), collapse = "." )
  return(paste(as.character(week(x)), as.character(year(x)), collapse = "." ))
}

weekdata <- function(x, fn) {
  x$week <- as.character(lapply(X = x$date, FUN = weeek)) ## week group index
  ## aggregate by week index
  wdata <- aggregate(x[, -which(colnames(x) %in% c('date', 'week'))],
                     by = list(x$week), FUN = mean)
  wdata <- wdata[with(wdata, order(t)), ] ## sort by time
  
  wdata$t <- 1:length(wdata$t)
  return(wdata)
}
idiff <- function(x){
  d <- ur.df(x, "trend", 1)@teststat[1]
  ts <- ur.df(x, "trend", 1)@cval[1, 3]
  i <- 0 
  while (d > ts){
    x <- c(0, diff(x))
    d <- ur.df(x, "trend", 1)@teststat[1]
    ts <- ur.df(x, "trend", 1)@cval[1, 3]
    i <- i + 1
  }
  return(list(x, i))
} 
#==== DATA ====
dataindex <- read.csv("index-a.csv", header = TRUE, sep = ",", dec = ".")
colnames(dataindex)[1] <- "t"

dataindex1 <- subset(dataindex, #neutral
                     date %in% as.Date(as.Date('2010-06-30'):as.Date('2012-08-01')))
dataindex2 <- subset(dataindex, #dovish
                     date %in% as.Date(as.Date('2012-06-20'):as.Date('2015-01-28')))
dataindex3 <- subset(dataindex, #hawkish
                     date %in% as.Date(as.Date('2014-10-30'):as.Date('2019-03-20')))



dataindex1week <- weekdata(dataindex1, mean)
dataindex2week <- weekdata(dataindex2, mean)
dataindex3week <- weekdata(dataindex3, mean)

df = dataindex1week[, c('DGS10', 'DXY', 'SP500', 'VIX', 'ARGENTINA')]
row.names(df) <- NULL
library(vars)

# Estimate model
model <- VAR(df, type = "const", lag.max = 5, ic = 'AIC')

# Look at summary statistics
summary(model)

df = (df[-1, ] / df[-nrow(df), ])-1

#====== Daily ======

#first period
ch4p1 <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex1)[m]
  df = dataindex1[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p1[[m-2]] <- model
  names(ch4p1)[m-2] <- colnames(dataindex1)[m]
}

ch4p2  <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex2)[m]
  df = dataindex2[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p2[[m-2]] <- model
  names(ch4p2)[m-2] <- colnames(dataindex2)[m]
}
#third period
ch4p3  <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex3)[m]
  df = dataindex3[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p3[[m-2]] <- model
  names(ch4p3)[m-2] <- colnames(dataindex3)[m]
}
chapter4 <- list(ch4p1, ch4p2, ch4p3)
names(chapter4) <- c("first period", "second period", "third period")

#granger tests


#====== Weekly ========
#first period
ch4p1w <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex1week)[m]
  df = dataindex1week[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p1w[[m-2]] <- model
  names(ch4p1w)[m-2] <- colnames(dataindex1week)[m]
}

ch4p2w  <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex2week)[m]
  df = dataindex2week[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p2w[[m-2]] <- model
  names(ch4p2w)[m-2] <- colnames(dataindex2week)[m]
}
#third period
ch4p3w  <- list(NaN)

for (m in 3:20){
  country <- colnames(dataindex3week)[m]
  df = dataindex3week[, c('DGS10', 'DXY', 'SP500', 'VIX', country)]
  df = (df[-1, ] / df[-nrow(df), ])-1
  
  x<- idiff(ts(df[,1]))
  timeseries <- x[[1]]
  d <- x[[2]]
  for (i in 2:(ncol(df))){
    x <- idiff(ts(df[i]))
    
    d <- max(d, x[[2]])
    if (d > x[[2]]){
      timeseries <- cbind(timeseries, c(0, diff(x[[1]])))
    } else {
      timeseries <- cbind(timeseries, x[[1]])
    }
  }
  colnames(timeseries) <- colnames(df)
  timeseries <- ts(timeseries[-c(1:d),])
  model <- VAR(timeseries, type = "const", lag.max = 5, ic = 'AIC')
  ch4p3w[[m-2]] <- model
  names(ch4p3w)[m-2] <- colnames(dataindex3week)[m]
}
chapter4w <- list(ch4p1w, ch4p2w, ch4p3w)
names(chapter4w) <- c("first period", "second period", "third period")

chapter4 <- list(chapter4, chapter4w)
names(chapter4) <- c('daily', 'weekly')

countrynames <- colnames(dataindex)[3:20]
periods <-  c("first period", "second period", "third period")
tps <- c('SP', 'DXY', '10y', 'VIX')
tps_ <- c('DGS10', 'DXY', 'SP500', 'VIX')

data_imp <- as.data.frame(NULL)
for (type in 1:4){
  for (freq in c('daily', 'weekly')){
    for (per in periods){
      for (con in countrynames){
        or <- irf(chapter4[[freq]][[per]][[con]], 
                  impulse = tps_[type], response = con,
                  n.ahead = 12, ortho = TRUE, runs = 1000)
        
        temp <- as.data.frame(t(cbind(as.vector(or[[1]][[1]]), 
                                      as.vector(or[[2]][[1]]), 
                                      as.vector(or[[3]][[1]]))))
        
        name_of_table <- paste(tps[type], freq, which(periods == per), con,
                               sep = '_')
        print(name_of_table)
        row.names(temp) <- sapply(names(or)[1:3], FUN = {function (x) 
          paste(name_of_table, x, sep = '_')})
        data_imp <- rbind(data_imp, temp)
      }
    }
  }
}


data_imp <- cbind(data_imp, as.vector(row.names(data_imp)))
colnames(data_imp)[14] <- 'row'
openxlsx::write.xlsx(data_imp, file = 'data_imp_var.xlsx')
res <- as.data.frame(NULL)
  for (freq in c('daily', 'weekly')){
    for (per in periods){
      for (con in countrynames){
        temp_var_list <- c(tps_, con)
        ljung <- as.vector(NULL)
        for (tp in temp_var_list){
          x = chapter4[[freq]][[per]][[con]][["varresult"]][[tp]][["residuals"]]
          x <- Box.test(as.vector(x), lag = 1, type = "Ljung")[3]
          ljung <- append(ljung, x)
        }
        ljung <- length(which(unlist(ljung) == 0))
        p <- chapter4[[freq]][[per]][[con]][["p"]]
        res <- rbind(res, c(freq, per, con, p, ljung))
      }
    }
  }
colnames(res) <- c('частота', 'сегмент', "страна", 
                   "количество лагов", "коррелированные ошибки")
openxlsx::write.xlsx(res, file = 'specification.xlsx')
