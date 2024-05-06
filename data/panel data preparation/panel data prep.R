#required packages
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
library(mondate)
library(glue)
Sys.setenv(LANG = "en")


setwd("C:/Users/vasil/YandexDisk/D is not for Dragons/calculations")

dataindex <- read.csv("index-a.csv", header = TRUE, sep = ";", dec = ".")[]
colnames(dataindex)[1] <- "date"
dataindex$date <- as.Date(dataindex$date, "%d.%m.%Y")
qtrs <- as.vector(NULL)
for (i in dataindex$date) {
  i <- as.Date(i)
  qtrs <- append(qtrs, paste(year(i), quarters(i), sep = ""))
}
dataindex$qtr <- qtrs
dataindex_qtr <- aggregate(x = dataindex[, -c(1,2,26)], by = list(dataindex$qtr), FUN = 'mean')
colnames(dataindex_qtr)[1] <- "QTR"
dataindex_qtr[-1, c(2:19)] <-
  (dataindex_qtr[-1, c(2:19)]/dataindex_qtr[-34, c(2:19)] - 1)*100
dataindex_qtr[1,c(2:19)] <- 0


# capital flow data
portfolio <- readxl::read_xlsx("portfolio.xlsx")
direct <- readxl::read_xlsx("direct.xlsx")
gdp <- readxl::read_xlsx("gdp.xlsx")

gdp4 <- matrix(0, ncol = 19, nrow = 16*4)
gdp <- as.matrix(gdp)
for (i in 1:16) {
  for (j in 0:3) {
    gdp4[(i*4 - j), ] <- gdp[i, ]
  }
}
gdp <- cbind(gdp4[, -1], gdp4[, -1], gdp4[, -1], gdp4[, -1])[1:58, ]


PtoGDP <- portfolio[, -1] * (100*100000/gdp)
PtoGDP$QTR <- portfolio$YEAR
DtoGDP <- direct[, -1] * (100*100000/gdp)
DtoGDP$QTR <- portfolio$YEAR
PtoGDP <- PtoGDP[24:57, !grepl('DOMESTIC', colnames(PtoGDP))]
DtoGDP <- DtoGDP[24:57, !grepl('DOMESTIC', colnames(DtoGDP))]


colnames(PtoGDP) <- sub('nonoutflow', 'inflow', 
                    sub('resident', 'outflow', colnames(PtoGDP)))
colnames(DtoGDP) <- sub('nonoutflow', 'inflow', 
                        sub('resident', 'outflow', colnames(DtoGDP)))

var_names <- unname(as.matrix(expand.grid(
  c('direct', 'portfolio'), c('inflow', 'outflow'), c('equity', 'debt')), ))

var_names  <- paste(var_names[,1], var_names[,2], var_names[,3], sep = '_')
c_indexes <- colnames(dataindex)[3:20][-8]
t_indexes <- dataindex_qtr$QTR
pnl <- expand.grid(c_indexes, t_indexes)
colnames(pnl) <- c('c_index', 'QTR')



pnl$v2 <- c(t(dataindex_qtr[, 2:19][-7]))
pnl$v1 <- c(t(dataindex_qtr[, 2:19][-8]))
pnl <- merge(pnl, dataindex_qtr[, c('VIX', 'USA', 'DGS10', 'DXY', 'QTR')], 
             by = 'QTR')
for (i in var_names){
  eval(parse(text = glue('pnl${i} <- 0')))
}
for (m in c('outflow', 'inflow')){
  for (k in c('equity', 'debt')){  
    for (c in c_indexes){
      for (t in t_indexes){
        y <- grepl(m, colnames(PtoGDP)) &
             grepl(k, colnames(PtoGDP)) &
             grepl(c, colnames(PtoGDP))  
        pnl[pnl$QTR == t & pnl$c_index == c, glue('portfolio_{m}_{k}')] <-
          as.numeric(PtoGDP[PtoGDP$QTR == t, y])
      }
    }
  }
}
for (m in c('outflow', 'inflow')){
  for (k in c('equity', 'debt')){  
    for (c in c_indexes){
      for (t in t_indexes){
        y <- grepl(m, colnames(DtoGDP)) &
          grepl(k, colnames(DtoGDP)) &
          grepl(c, colnames(DtoGDP))  
        pnl[pnl$QTR == t & pnl$c_index == c, glue('direct_{m}_{k}')] <-
           as.numeric(DtoGDP[DtoGDP$QTR == t, y])
      }
    }
  }
}
pnl$delta_VIX <- (c(rep(0, 17), 
                   tail(pnl$VIX, length(pnl$VIX)-17)/head(pnl$VIX, 17))-1)*100


#economic growth

gdp <- readxl::read_xlsx("gdp.xlsx")

pnl$growth <- 0
r_growth <- function(v, r){
  l = length(v)
  f <- r:l
  result <- v[f]
  for (i in 1:(r-1)){
    result <- result + v[f - i]
  }
  l = length(result)
  result <- (1 - head(result, l-1)/result[-1])*100
  return(c(rep(0, r), result))
}
#fred data
for (cntr in c_indexes[c(1:3, 5, 7, 8, 10, 14, 15, 17)]){
  x <- read.csv(glue('{tolower(cntr)} gdp.csv'))
  colnames(x)[2] <- 'gdp'
  x$DATE <- as.Date(x$DATE)
  x$QTR <- c('start',
             paste0(year(head(x$DATE, length(x$DATE)-1)), 'Q',
                    quarter(head(x$DATE, length(x$DATE)-1))))
  
  x$growth <- r_growth(x$gdp, 4)
  for (t in t_indexes){
    pnl[pnl$QTR == t & pnl$c_index == cntr, 'growth'] <-
      x[x$QTR == t, 'growth']
  }
}

#non fred data

nonfred <- readxl::read_xlsx('nonfred gdps.xlsx')
nonfred$date <- as.Date(nonfred$date)
nonfred$QTR <- c('start', 
                 paste0(year(nonfred$date), 
                        'Q', 
                        quarter(nonfred$date))[-length(nonfred$date)])
for (cntr in c_indexes[-c(1:3, 5, 7, 8, 10, 14, 15, 17)]){
  for (t in t_indexes){
    pnl[pnl$QTR == t & pnl$c_index == cntr, 'growth'] <-
      nonfred[nonfred$QTR == t, tolower(cntr)]
  }
}


#effective exchange rate


m_ex_rate <- readxl::read_xlsx('m_ex_rate.xlsx')
m_ex_rate$QTR <- paste0(year(m_ex_rate$date), 'Q', quarter(m_ex_rate$date))
q_ex_rate <- aggregate(m_ex_rate, by = list(m_ex_rate$QTR), FUN = tail, n = 1)

pnl$ex_rate <- 0
for (cntr in c_indexes){
  for (t in t_indexes){
    pnl[pnl$QTR == t & pnl$c_index == cntr, 'ex_rate'] <-
      q_ex_rate[q_ex_rate$QTR == t, tolower(cntr)]
  }
}
rm(list = setdiff(ls(), 'pnl'))
write.xlsx(pnl, 'pnl.xlsx')

