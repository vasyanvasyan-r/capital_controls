setwd("C:/Users/vasil/YandexDisk/D is not for Dragons/calculations")
library(AER)
library(plm)
library(stargazer)
library(glue)
library(sandwich)
library(lmtest)


pnl <- readxl::read_xlsx('pnl.xlsx')

pnl[,'growth_lag_1'] <- c(rep(0, 17), 
                      pnl$growth[-c(1:17)])
pnl[,'VIX_lag_1'] <- c(rep(0, 17), 
                   pnl$VIX[-c(1:17)])
x <- as.vector(NULL) 
for (i in 1:length(unique(pnl$QTR))) {x <- append(x, rep(i, 17))}
pnl$trend <- x ; rm(x)
pnl$stock_growth <- (pnl$v1 + pnl$v2)/2

colnames(pnl)[9:16] <- sub('outflow', 'resident', 
                           sub('inflow', 'nonresident',
                                colnames(pnl)[9:16]))

for (typ in c('direct', 'portfolio')){   #type of flow
  for(i in 1:4){                        # 2 outflows and 2 inflows
    #get the variable name v
    eval(parse(text = glue("v <- colnames(pnl)[9:16][grep('{typ}', 
                                colnames(pnl)[9:16])][i]")))
    
    #get a lag of the variable
    eval(parse(text = glue("pnl[,'{v}_lag_1'] <- c(rep(0, 17), 
                     pnl${v}[-c(1:17)])")))
    
    eval(parse(text = glue(
      "f_{v} <- stock_growth ~ 
              VIX*{v} +
              VIX_lag_1*{v}_lag_1 + 
              ex_rate + growth + trend")))
    
    #make the model
    eval(parse(text = glue(
    "m{i} <- plm(f_{v}, 
              data = pnl[-(1:17),],
              model = 'pooling')")))
    
    #get clustered errors for every country
    cl <- list(NULL)
    eval(parse(
    text=glue('cl[[{i}]] <- coef(summary(m{i},cluster = c("QTR")))[, 2]')))
    
    #tests for cross-dependence
    eval(parse(text = glue("
    bp{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], test = 'lm')$p.value, 5)")))
    eval(parse(text = glue("
    pa_t{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], 
                           index = 'QTR', test = 'cd')$p.value, 5)")))
    eval(parse(text = glue("
    pa_c{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], 
                           index = 'c_index', test = 'cd')$p.value, 5)
    rm(f_{v})            
    ")))
  }
  
  bp <- c('Breusch and Pagan', bp1, bp2, bp3, bp4)
  pa_t <- c('Pesaran CD for time', pa_t1, pa_t2, pa_t3, pa_t4)
  pa_c <- c('Pesaran CD for countries', pa_c1, pa_c2, pa_c3, pa_c4)
  
  #model names
  names_model <- colnames(pnl)[9:16][grep(typ, colnames(pnl)[9:16])]
  names_model <- sub('_', ' ', sub(paste0(typ, '_'), '', names_model))
  
  if (typ == 'direct'){
    table_name = 'Таблица 2 Прямые инвестиции'
  }else {
    table_name = 'Таблица 3 Портфельные инвестиции'
  }
  #get the final table
  eval(parse(text=glue("stargazer(m1, m2, m3, m4, type = 'html', 
                                      title = table_name,
                                      out = paste0('{typ}', '.html'),
                                      object.names = TRUE,
                                      column.labels = names_model,
                                      column.sep.width = '20pt',
                                      se = cl,
                                      add.lines = list(bp, pa_t, pa_c))")))
  rm(m1, m2, m3, m4, v, i, typ, cl, names_model,
     bp, bp1, bp2, bp3, bp4, 
     pa_t, pa_t1, pa_t2, pa_t3, pa_t4,
     pa_c, pa_c1, pa_c2, pa_c3, pa_c4)
}

for (typ in c('direct', 'portfolio')){
  for (i in c('debt', 'equity')){
    eval(parse(text = glue("
    x <- pnl[intersect(grep('{typ}', colnames(pnl)),
                  grep('{i}', colnames(pnl)))]
    
    x1 <- x[grep('lag', colnames(x))]; x2 <- x[-grep('lag', colnames(x))] 
    
    
    pnl[,'{typ}_{i}_lag_1'] <- x1[grep('non', colnames(x1))] -
                           x1[-grep('non', colnames(x1))]
    
    
    pnl[,'{typ}_{i}'] <- x2[grep('non', colnames(x2))] -
                     x2[-grep('non', colnames(x2))]
    ")))
    rm(x1, x2, x)

  }
}
names_model <- c(NULL)
for (i in 1:4){
  eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  f_{v} <- stock_growth ~ 
              VIX*{v} +
              VIX_lag_1*{v}_lag_1 + 
              ex_rate + growth + trend
              
  m{i} <- plm(f_{v}, 
              data = pnl[-(1:17),],
              model = 'pooling')
              
  cl <- list(NULL)
  cl[[{i}]] <- coef(summary(m{i},cluster = c('QTR')))[, 2]
  
  bp{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], test = 'lm')$p.value, 5)
  
  pa_t{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'QTR', test = 'cd')$p.value, 5)
  
  pa_c{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'c_index', test = 'cd')$p.value, 5)
                  
  names_model <- append(names_model, v)
  rm(f_{v})
  ")))
}
bp <- c('Breusch and Pagan', bp1, bp2, bp3, bp4)
pa_t <- c('Pesaran CD for time', pa_t1, pa_t2, pa_t3, pa_t4)
pa_c <- c('Pesaran CD for countries', pa_c1, pa_c2, pa_c3, pa_c4)

stargazer(m1, m2, m3, m4, type = 'html', 
          title = 'Таблица ',
          out = paste0('agg_flows', '.html'),
          se = cl,
          add.lines = list(bp, pa_t, pa_c))
rm(m1, m2, m3, m4, v, i, typ, cl, names_model,
   bp, bp1, bp2, bp3, bp4, 
   pa_t, pa_t1, pa_t2, pa_t3, pa_t4,
   pa_c, pa_c1, pa_c2, pa_c3, pa_c4)

pnl[, 'VIX_diff'] <- pnl$VIX - mean(pnl$VIX)

for (i in 1:4){
    eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  pnl[, 'delta_VIX_abs_{v}'] <- pnl$delta_VIX*abs(pnl[, '{v}'])
  ")))
  
}


names_model <- c(NULL)
for (i in 1:4){
  eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  f_{v} <- stock_growth ~ {v} + delta_VIX + 
              delta_VIX_abs_{v} +
              ex_rate + growth + trend
              
  m{i} <- plm(f_{v}, 
              data = pnl[-(1:17),],
              model = 'pooling')
              
  cl <- list(NULL)
  cl[[{i}]] <- coef(summary(m{i},cluster = c('QTR')))[, 2]
  
  bp{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], test = 'lm')$p.value, 5)
  
  pa_t{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'QTR', test = 'cd')$p.value, 5)
  
  pa_c{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'c_index', test = 'cd')$p.value, 5)
                  
  names_model <- append(names_model, v)
  rm(f_{v})
  ")))
}
bp <- c('Breusch and Pagan', bp1, bp2, bp3, bp4)
pa_t <- c('Pesaran CD for time', pa_t1, pa_t2, pa_t3, pa_t4)
pa_c <- c('Pesaran CD for countries', pa_c1, pa_c2, pa_c3, pa_c4)

stargazer(m1, m2, m3, m4, type = 'html', 
          title = 'Таблица 4 с приростом VIX',
          out = paste0('deltaVIX', '.html'),
          se = cl,
          add.lines = list(bp, pa_t, pa_c))
rm(m1, m2, m3, m4, v, i, cl, names_model,
   bp, bp1, bp2, bp3, bp4, 
   pa_t, pa_t1, pa_t2, pa_t3, pa_t4,
   pa_c, pa_c1, pa_c2, pa_c3, pa_c4)


for (i in 1:4){
  eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  pnl[, 'VIX_diff_abs_{v}'] <- pnl$VIX_diff*abs(pnl[, '{v}'])
  ")))
  
}
names_model <- c(NULL)
for (i in 1:4){
  eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  f_{v} <- stock_growth ~ {v} + VIX_diff + 
              VIX_diff_abs_{v} +
              ex_rate + growth + trend
              
  m{i} <- plm(f_{v}, 
              data = pnl[-(1:17),],
              model = 'pooling')
              
  cl <- list(NULL)
  cl[[{i}]] <- coef(summary(m{i},cluster = c('QTR')))[, 2]
  
  bp{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], test = 'lm')$p.value, 5)
  
  pa_t{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'QTR', test = 'cd')$p.value, 5)
  
  pa_c{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'c_index', test = 'cd')$p.value, 5)
                  
  names_model <- append(names_model, v)
  rm(f_{v})
  ")))
}
bp <- c('Breusch and Pagan', bp1, bp2, bp3, bp4)
pa_t <- c('Pesaran CD for time', pa_t1, pa_t2, pa_t3, pa_t4)
pa_c <- c('Pesaran CD for countries', pa_c1, pa_c2, pa_c3, pa_c4)

stargazer(m1, m2, m3, m4, type = 'html', 
          title = 'Таблица 5 с отклонением VIX от среднего',
          out = paste0('diffVIX', '.html'),
          se = cl,
          add.lines = list(bp, pa_t, pa_c))
rm(m1, m2, m3, m4, v, i, cl, names_model,
   bp, bp1, bp2, bp3, bp4, 
   pa_t, pa_t1, pa_t2, pa_t3, pa_t4,
   pa_c, pa_c1, pa_c2, pa_c3, pa_c4)


names_model <- c(NULL)
for (i in 1:4){
  eval(parse(text = glue("
  v <- colnames(pnl)[32:39][-grep('lag', colnames(pnl)[32:39])][{i}]")))
  eval(parse(text = glue("
  f_{v} <- {v} ~ VIX + VIX_lag_1 +
              ex_rate + growth +USA + DGS10 + DXY + trend
              
  m{i} <- plm(f_{v}, 
              data = pnl[-(1:17),],
              model = 'pooling')
              
  cl <- list(NULL)
  cl[[{i}]] <- coef(summary(m{i},cluster = c('c_index')))[, 2]
  
  bp{i} <- round(pcdtest(f_{v}, 
                        data = pnl[-(1:17),], test = 'lm')$p.value, 5)
  
  pa_t{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'QTR', test = 'cd')$p.value, 5)
  
  pa_c{i} <- round(pcdtest(f_{v}, 
                  data = pnl[-(1:17),], 
                  index = 'c_index', test = 'cd')$p.value, 5)
                  
  names_model <- append(names_model, v)
  rm(f_{v})
  ")))
}
bp <- c('Breusch and Pagan', bp1, bp2, bp3, bp4)
pa_t <- c('Pesaran CD for time', pa_t1, pa_t2, pa_t3, pa_t4)
pa_c <- c('Pesaran CD for countries', pa_c1, pa_c2, pa_c3, pa_c4)

stargazer(m1, m2, m3, m4, type = 'html', 
          title = 'Таблица 6 Потоки капитала зависимая переменная',
          out = paste0('flows', '.html'),
          se = cl,
          add.lines = list(bp, pa_t, pa_c))
rm(m1, m2, m3, m4, v, i, cl, names_model,
   bp, bp1, bp2, bp3, bp4, 
   pa_t, pa_t1, pa_t2, pa_t3, pa_t4,
   pa_c, pa_c1, pa_c2, pa_c3, pa_c4)