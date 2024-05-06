setwd("C:/Users/vasil/YandexDisk/D is not for Dragons/data/effective exchange rate")
library(mondate)
library(glue)
library(openxlsx)

#Pakistan
paki <- read.csv('pakistan.csv', sep = ',')
paki <-  paki[,4:5]
colnames(paki) <- c('date', 'value')
paki[is.na(paki)] <- 0
#change date format
m <- c('Jan', 'Feb', 'Mar', 'Apr', 
       'May', 'Jun', 'Jul', 'Aug', 
       'Sep', 'Oct', 'Nov', 'Dec')
m_n <- c('01', '02', '03', '04', 
         '05', '06', '07', '08',
         '09', '10', '11', '12')
m <- data.frame(m, m_n)
for (i in 1:12){
  paki$date <- gsub(m[i, 1], m[i, 2], paki$date)
}
paki$date <- as.Date(paki$date, '%d-%m-%Y')

qtrs <- as.vector(NULL)
for (i in paki$date) {
  i <- as.Date(i)
  qtrs <- append(qtrs, paste(year(i), quarters(i), sep = ""))
}
paki$QTR <- qtrs
paki <- paki[49:270,]

rate <- as.vector(100)
for (i in paki$value){

  rate <- append(rate, tail(rate, 1)*(100 + i)/100)
}
paki$rate <- rate[-length(rate)]
paki_qtr <- aggregate(x = paki[, 4], by = list(paki$QTR), FUN = 'mean')
paki$key <- paste(month(paki$date), year(paki$date))

#rm(list = setdiff(ls(), 'paki_qtr'))

#Egypt
egy_exchange_rate <- readxl::read_xlsx('egypt.xlsx')
infl_egy <- readxl::read_xlsx('inflation egy.xlsx')
infl_usa <- readxl::read_xls('inflation usa.xls')
infl_eu <- read.csv('eu.csv')
colnames(infl_egy) <- c('date', 'infl')
colnames(infl_usa) <- c('date', 'infl')

## effective usd and euro
egy_exchange_rate$Sell <- as.numeric(egy_exchange_rate$Sell)
egy_exchange_rate$Buy <- as.numeric(egy_exchange_rate$Buy)
egy_exchange_rate$rate <- (egy_exchange_rate[,3] + egy_exchange_rate[,4])/2

usd <- egy_exchange_rate[egy_exchange_rate$Currency == 'US Dollar', c(1, 4)]
euro <- egy_exchange_rate[egy_exchange_rate$Currency == 'Euro',c(1, 4)]
colnames(usd) <- c('date', 'rate')
colnames(euro) <- c('date', 'rate')
usd$date <- as.Date(usd$date, '%d/%m/%Y')
euro$date <- as.Date(euro$date, '%d/%m/%Y')
usd$m <- month(usd$date)
usd$y <- year(usd$date)
euro$m <- month(euro$date)
euro$y <- year(euro$date)
usd <- usd[order(usd$date),]
euro <- euro[order(euro$date),]
usd$key <- paste(usd$m, usd$y)
euro$key <- paste(euro$m, euro$y)

usd <- aggregate(list(usd$rate, usd$date),
                 by = list(usd$key), FUN = tail, n = 1)
euro <- aggregate(list(euro$rate, euro$date),
                   by = list(euro$key), FUN = tail, n = 1)
colnames(usd) <- c('key', 'rate', 'date')
colnames(euro) <- c('key', 'rate', 'date')

x <- data.frame(
  '1 2015', usd[usd$key == '12 2014', 2], as.Date('29-01-2015', '%d-%m-%Y'))
colnames(x) <- c('key', 'rate', 'date')
usd <- rbind(usd, x)

x <- data.frame(
  '5 2015', usd[usd$key == '4 2015', 2], as.Date('31-05-2015', '%d-%m-%Y'))
colnames(x) <- c('key', 'rate', 'date')
usd <- rbind(usd, x)
usd <- usd[order(usd$date),]

x <- data.frame(
  '1 2015', euro[euro$key == '12 2014', 2], as.Date('29-01-2015', '%d-%m-%Y'))
colnames(x) <- c('key', 'rate', 'date')
euro <- rbind(euro, x)

x <- data.frame(
  '5 2015', euro[euro$key == '4 2015', 2], as.Date('31-05-2015', '%d-%m-%Y'))
colnames(x) <- c('key', 'rate', 'date')
euro <- rbind(euro, x)
euro <- euro[order(euro$date),]

#inflation in usa
infl_usa$key <-
  append('12_1959', 
  paste(month(infl_usa$date), year(infl_usa$date)))[1:length(infl_usa$date)]

usd$infl <- infl_usa[infl_usa$key %in% usd$key, 2]/100 + 1

#inflation in eu
infl_eu$infl <- c(0, infl_eu[-1,2]/infl_eu[-length(infl_eu[,1]),2])
infl_eu$date <- as.Date(infl_eu$date)
infl_eu$key <-
  append('start', 
         paste(month(infl_eu$date), year(infl_eu$date)))[1:length(infl_eu$date)]
euro$infl <- infl_eu[infl_eu$key %in% euro$key, 'infl']

#egypt inflation
infl_egy <- infl_egy[, 1:2]

infl_egy$infl <- as.numeric(sub('%', '',infl_egy$infl, fixed = TRUE))

char_index <- c('01-01-', '01-02-', '01-03-', '01-04-', '01-05-', '01-06-',
                '01-07-', '01-08-', '01-09-', '01-10-', '01-11-', '01-12-')
m <- c('Jan', 'Feb', 'Mar', 'Apr', 
       'May', 'Jun', 'Jul', 'Aug', 
       'Sep', 'Oct', 'Nov', 'Dec')

m <- data.frame(m, char_index)
for (i in 1:12){
  infl_egy$date <- gsub(m[i, 1], m[i, 2], infl_egy$date)
  
}
infl_egy$date <- as.Date(gsub(' ', '', infl_egy$date), '%d-%m-%Y')
infl_egy <- infl_egy[order(infl_egy$date),]
infl_egy$key <- append('start', 
    paste(month(infl_egy$date), year(infl_egy$date)))[1:length(infl_egy$date)]
euro$infl_e <- infl_egy[infl_egy$key %in% euro$key, 'infl']/100 + 1
usd$infl_e <- infl_egy[infl_egy$key %in% usd$key, 'infl']/100 + 1

egypt <- data.frame( usd$key, usd$date,
                    (usd$rate/usd$infl + euro$rate/euro$infl)*usd$infl_e)
colnames(egypt) <- c('key', 'date', 'rate')


#the other countries
country_names <- c('argentina', 'brazil', 'chile', 'china', 'colombia',
                   'egypt', 'india', 'indonesia', 'malaysia', 'mexico', 
                   'pakistan', 'peru', 'philippines',
                   'russia', 'south.africa', 'thailand', 'turkey')
n <- c('key', 'date', 'egypt')
for (i in country_names[-c(11, 6)]){
  n <- append(n, i)
  eval(parse(text = glue('{i} <- readxl::read_excel("{i}.xls",
                             skip = 11, col_types = c("date", "numeric"),
                             col_names = c("date", "rate"))')))
  eval(parse(text = glue('{i}$key <- append("start", 
          paste(month({i}$date), year({i}$date)))[1:length({i}$date)]')))

  eval(parse(text = glue('egypt <- 
          merge(egypt, {i}, by = "key")')))

  egypt <- egypt[, -length(egypt)+1]
  colnames(egypt) <- n
  eval(parse(text = glue('rm({i})')))
}
m_ex_rate <- merge(egypt, paki, by = 'key')[, -c(19:21)]
colnames(m_ex_rate)[c(2, 19)] <- c('date', 'pakistan')
m_ex_rate <- m_ex_rate[order(m_ex_rate$date), ]
for (i in country_names){
  eval(parse(text = 
          glue('m_ex_rate${i} <- m_ex_rate${i}*100/m_ex_rate${i}[1]')))
}
m_ex_rate$egypt <- 10000*m_ex_rate$egypt^-1


rm(list = setdiff(ls(), 'm_ex_rate'))

setwd("C:/Users/vasil/YandexDisk/D is not for Dragons/calculations")
write.xlsx(m_ex_rate, 'm_ex_rate.xlsx')
